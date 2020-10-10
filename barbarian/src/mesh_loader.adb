
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Buffers;

with Game_Utils;

package body Mesh_Loader is
   use GL.Objects.Vertex_Arrays;
   use GL.Types.Singles;
   type Animation is record
      null;
   end record;

   package Animations_Package is new Ada.Containers.Doubly_Linked_Lists (Animation);
   type Animations_List is new Animations_Package.List with null record;

   package Matrix4_Package is new Ada.Containers.Doubly_Linked_Lists (Singles.Matrix4);
   type Matrix4_List is new Matrix4_Package.List with null record;

   package Vector2_Package is new Ada.Containers.Vectors (Positive, Singles.Vector2);
   type Vector2_List is new Vector2_Package.Vector with null record;

   package Vector3_Package is new Ada.Containers.Vectors (Positive, Singles.Vector3);
   type Vector3_List is new Vector3_Package.Vector with null record;

   package Vector4_Package is new Ada.Containers.Vectors (Positive, Singles.Vector4);
   type Vector4_List is new Vector4_Package.Vector with null record;

   type Node_Children_Array is array (1 .. Max_Bones, 1 .. Max_Bones) of Integer;

   type Mesh is record
      File_Name              : Unbounded_String := To_Unbounded_String ("");
      VAO                    : Vertex_Array_Object := Null_Array_Object;
      Shadow_VAO             : Vertex_Array_Object := Null_Array_Object;
      Point_Count            : Integer := 0;
      Vp_Count               : Integer := 0;
      Vn_Count               : Integer := 0;
      Vt_Count               : Integer := 0;
      Vtan_Count             : Integer := 0;
      Vb_Count               : Integer := 0;
      Bounding_Radius        : Float := 1.0;
      --  the skeleton hierarchy
      Root_Transform_Matrix  : Singles.Matrix4 := Singles.Identity4;
      Offset_Matrices        : Matrix4_List;
      Current_Bone_Matrices  : Matrix4_List;
      Anim_Node_Parents      : Matrix4_List;
      Anim_Node_Children     : Node_Children_Array := (others => (others => 0));
      Anim_Node_Num_Children : Int_Array (1 .. Max_Bones) := (others => 0);
      Anim_Node_Bone_Ids     : GL_Maths.Ints_List;
      Bone_Count             : Integer := 0;
      -- animations using the skeleton
      Animations             : Animations_List;
      Animation_Count        : Integer := 0;
      Points_Vbo             : GL.Objects.Buffers.Buffer;
      Normals_Vbo            : GL.Objects.Buffers.Buffer;
      Texcoords_Vbo          : GL.Objects.Buffers.Buffer;
      Bone_Ids_Vbo           : GL.Objects.Buffers.Buffer;
      Vtans_Vbo              : GL.Objects.Buffers.Buffer;
   end record;

   package Meshes_Package is new Ada.Containers.Vectors (Positive, Mesh);
   type Mesh_List is new Meshes_Package.Vector with null record;

   --      Loaded_Mesh_Count    : Integer := 0;
   --      Allocated_Mesh_Count : Integer := 128;
   Loaded_Meshes        : Mesh_List;

   function Load_Mesh (Path : String; Meshes : in out Mesh_List) return Integer;

   --  ------------------------------------------------------------------------

   procedure Init is
   begin
      Loaded_Meshes.Clear;
      --          Loaded_Mesh_Count := 0;
      --          Allocated_Mesh_Count := 0;
   end Init;

   --  ------------------------------------------------------------------------

   procedure Load_Normal_Data (Input_File : File_Type; Vec : in out Vector3) is
      use Ada.Strings;
      Data  : constant String := Get_Line (Input_File);
      Pos1  : constant Natural := Fixed.Index (Data, " ");
      Pos2  : Natural;
   begin
      Vec (GL.X) :=
        Single (Integer'Value (Data (1 .. Pos1)));
      Pos2 :=
        Fixed.Index (Data (Pos1 + 1 .. Data'Last), " ");
      Vec (GL.Y) :=
        Single (Integer'Value (Data (Pos1 + 1 .. Pos2)));
      Vec (GL.Z) :=
        Single (Integer'Value (Data (Pos2 + 1 .. Data'Last)));
   end Load_Normal_Data;

   --  ------------------------------------------------------------------------

   procedure Load_Point_Data (Input_File : File_Type; Vec : in out Vector3) is
      use Ada.Strings;
      Data  : constant String := Get_Line (Input_File);
      Pos1  : constant Natural := Fixed.Index (Data, " ");
      Pos2  : Natural;
   begin
      Vec (GL.X) :=
        Single'Value (Data (1 .. Pos1 - 1));
      Pos2 :=
        Fixed.Index (Data (Pos1 .. Data'Last), " ");
      Vec (GL.Y) :=
        Single'Value (Data (Pos1 + 1 .. Pos2 - 1));
      Vec (GL.Z) :=
        Single'Value (Data (Pos2 + 1 .. Data'Last));
   end Load_Point_Data;

   --  ------------------------------------------------------------------------

   function Load_Managed_Mesh (Mesh_Name               : String; Has_Vp, Has_Vn, Has_Vt,
                               Has_Tangents, Has_bones : Boolean := False)
                                return Integer is
      use Meshes_Package;
      aMesh  : Mesh;
      Found  : Boolean := False;
      Index  : Meshes_Package.Extended_Index;
      Result : Integer := 0;
   begin
      if not Loaded_Meshes.Is_Empty then
         Index := Loaded_Meshes.First_Index;
         while Index <= Loaded_Meshes.Last_Index and not Found loop
            aMesh := Loaded_Meshes.Element (index);
            Found := aMesh.File_Name = To_Unbounded_String (Mesh_Name);
            if Found then
               Result := Index;
            else
               Index := Index + 1;
            end if;
         end loop;
      end if;

      if not Found then
         Result := Load_Mesh (Mesh_Name, Loaded_Meshes);
      end if;
      return Result;
   end Load_Managed_Mesh;

   --  ------------------------------------------------------------------------

   function Load_Mesh (Path : String; Meshes : in out Mesh_List)
                       return Integer is
      use Ada.Strings;
      Vp_Comps             : Integer := 0;
      Vn_Comps             : Integer := 0;
      Vt_Comps             : Integer := 0;
      Vtan_Comps           : Integer := 0;
      Vb_Comps             : Integer := 0;
      Offset_Mat_Comps     : Integer := 0;
      Current_Anim_Index   : Integer := -1;
      Input_File           : File_Type;
      New_Mesh             : Mesh;
      Result               : Integer := 0;

      function Read_Vec2 (Vec : String) return Vector2 is
         Vec_Start : constant Integer := Vec'First;
         Vec_Last  : constant Integer := Vec'Last;
         theVec    : Vector2;
         Pos_1     : Natural := Fixed.Index (Vec, ",");
         Pos_2     : Natural;
      begin
         theVec (GL.X) := Single'Value (Vec (Vec_Start + 1 .. Pos_1 - 1));
         Pos_2 := Fixed.Index (Vec (Pos_1 + 1 .. Vec_Last), ")");
         theVec (GL.Y) := Single'Value (Vec (Pos_1 + 1 .. Pos_2 - 1));
         return theVec;
      end Read_Vec2;

      function Read_Vec3 (Vec : String) return Vector3 is
         Vec_Start : constant Integer := Vec'First;
         Vec_Last  : constant Integer := Vec'Last;
         theVec    : Vector3;
         Pos_1     : Natural := Fixed.Index (Vec, ",");
         Pos_2     : Natural;
      begin
         theVec (GL.X) := Single'Value (Vec (Vec_Start + 1 .. Pos_1 - 1));
         Pos_2 := Fixed.Index (Vec (Pos_1 + 1 .. Vec_Last), ",");
         theVec (GL.Y) := Single'Value (Vec (Pos_1 + 1 .. Pos_2 - 1));
         Pos_1 := Fixed.Index (Vec (Pos_2 + 2 .. Vec_Last), ")");
         theVec (GL.Z) := Single'Value (Vec (Pos_2 + 2 .. Pos_1 - 1));
         return theVec;
      end Read_Vec3;

      function Read_Vec4 (Vec : String) return Vector4 is
         Vec_Start   : constant Integer := Vec'First;
         Vec_Last    : constant Integer := Vec'Last;
         theVec      : Vector4;
         Pos_1       : Natural := Fixed.Index (Vec, ",");
         Pos_2       : Natural;
      begin
         --        Game_Utils.Game_Log ("Particle System Manager Read_Vec4 Vec: " & Vec);
         theVec (GL.X) := Single'Value (Vec (Vec_Start + 1 .. Pos_1 - 1));
         Pos_2 := Fixed.Index (Vec (Pos_1 + 2 .. Vec_Last), ",");
         theVec (GL.Y) := Single'Value (Vec (Pos_1 + 2 .. Pos_2 - 1));
         Pos_1 := Fixed.Index (Vec (Pos_2 + 2 .. Vec_Last), ",");
         theVec (GL.Z) := Single'Value (Vec (Pos_2 + 2 .. Pos_1 - 1));
         Pos_2 := Fixed.Index (Vec (Pos_1 + 2 .. Vec_Last), ")");
         theVec (GL.W) := Single'Value (Vec (Pos_1 + 2 .. Pos_2 - 1));
         return theVec;
      end Read_Vec4;
   begin
      Game_Utils.Game_Log ("Mesh_Loader.Load_Mesh loading " & Path);
      Open (Input_File, In_File, Path);
      New_Mesh.File_Name := To_Unbounded_String (Path);
      while not End_Of_File (Input_File) loop
         declare
            aLine          : constant String := Get_Line (Input_File);
            Last           : constant Integer := aLine'Length;
            Pos            : constant Natural := Fixed.Index (aLine, " ");
            Head           : constant String := aLine (2 .. Pos - 1);
            Tail           : constant String := aLine (Pos + 1 .. Last);
            VBS            : Vector3_List;
            VPS            : Vector3_List;
            VNS            : Vector3_List;
            VTS            : Vector2_List;
            VTanS          : Vector4_List;
         begin
            if aLine (1) = '@' then
               if Head = "Anton's" then
                  null;
               elsif Head = "vert_count" then
                  New_Mesh.Point_Count := Integer'Value (Tail);
               elsif Head = "vp" then
                  Vp_Comps := Integer'Value (Tail);
                  if Vp_Comps = 3 then
                     for index in 1 .. New_Mesh.Point_Count loop
                        VPS.Append (Read_Vec3 (Get_Line (Input_File)));
                     end loop;
                  end if;
               elsif Head = "vn" then
                  Vn_Comps := Integer'Value (Tail);
                  if Vn_Comps = 3 then
                     for index in 1 .. New_Mesh.Point_Count loop
                        VNS.Append (Read_Vec3 (Get_Line (Input_File)));
                     end loop;
                  end if;
               elsif Head = "vt" then
                  Vt_Comps := Integer'Value (Tail);
                  if Vt_Comps = 3 then
                     for index in 1 .. New_Mesh.Point_Count loop
                        VTS.Append (Read_Vec2 (Get_Line (Input_File)));
                     end loop;
                  end if;
               elsif Head = "vtan" then
                  Vtan_Comps := Integer'Value (Tail);
                  if Vtan_Comps = 4 then
                     for index in 1 .. New_Mesh.Point_Count loop
                        VTanS.Append (Read_Vec4 (Get_Line (Input_File)));
                     end loop;
                  end if;
               end if;
            elsif Head = "vb" then
               Vb_Comps := Integer'Value (Tail);
               if Vb_Comps = 3 then
                  for index in 1 .. New_Mesh.Point_Count loop
                     VBS.Append (Read_Vec3 (Get_Line (Input_File)));
                  end loop;
               end if;
            elsif Head = "skeleton" then
               null;
            elsif Head = "root_transform" then
               null;
            elsif Head = "offset_mat" then
               null;
            elsif Head = "hierarchy" then
               null;
            elsif Head = "animation" then
               null;
            elsif Head = "tra_keys" then
               null;
            elsif Head = "sca_keys" then
               null;
            elsif Head = "rot_keys" then
               null;
            elsif Head = "bounding_radius" then
               New_Mesh.Bounding_Radius := Float'Value (Tail);
            end if;
         end; --  declare block
      end loop;
      Close (Input_File);

      return Result;
   end Load_Mesh;

   --  ------------------------------------------------------------------------

   function Load_Mesh_Data_Only (File_Name   : String;
                                 Points      : in out GL_Maths.Vector3_List;
                                 Tex_Coords  : in out GL_Maths.Vector2_List;
                                 Normals     : in out GL_Maths.Vector3_List;
                                 Point_Count : in out Integer)
                                  return Boolean is
      use Ada.Strings;
      Input_File : File_Type;
   begin
      Point_Count := 0;
      Game_Utils.Game_Log ("Loaded_Mesh_Data_Only loading mesh data from: " &
                             File_Name);
      Open (Input_File, In_File, File_Name);
      while not End_Of_File (Input_File) loop
         declare
            aString       : constant String := Get_Line (Input_File);
            String_Length : constant Integer := aString'Length;
            Comps         : Integer := 0;
            Vec3          : Vector3 := (0.0, 0.0, 0.0);
         begin
            --                  Game_Utils.Game_Log ("Loaded_Mesh_Data_Only String: " & aString);
            if aString (1 .. 1) = "@" then
               if String_Length > 8 and aString (2 .. 9) = "Anton's " then
                  null;
               elsif String_Length > 12 and then aString (2 .. 12) = "vert_count " then
                  Point_Count := Integer'Value (aString (13 .. aString'Last));
               elsif String_Length > 11 and then aString (2 .. 10) = "vp comps " then
                  Comps  := Integer'Value (aString (11 .. aString'Last));
                  if Comps * Point_Count > 0 then
                     for index in 1 .. Comps * Point_Count loop
                        Load_Point_Data (Input_File, Vec3);
                        Points.Append (Vec3);
                     end loop;
                  end if;

               elsif String_Length > 11 and then aString (2 .. 10) = "vn comps " then
                  Comps  := Integer'Value (aString (11 .. aString'Last));
                  if Comps * Point_Count > 0 then
                     for index in 1 .. Comps * Point_Count loop
                        Load_Normal_Data (Input_File, Vec3);
                        Normals.Append (Vec3);
                     end loop;
                  end if;

               elsif String_Length > 11 and then aString (2 .. 10) = "vt comps " then
                  Comps := Integer'Value (aString (11 .. aString'Last));
                  if Comps * Point_Count > 0 then
                     for index in 1 .. Comps * Point_Count loop
                        declare
                           Value : constant String := Get_Line (Input_File);
                           Pos1  : constant Natural := Fixed.Index (Value, " ");
                           Vec2  : Vector2;
                        begin
                           Vec2 (GL.X) :=
                             Single'Value (Value (1 .. Pos1 - 1));
                           Vec2 (GL.Y) :=
                             Single'Value (Value (Pos1 + 1 .. Value'Last));
                           Tex_Coords.Append (Vec2);
                        end;  --  declare block
                     end loop;
                  end if;
               end if;
            end if;
         end;  --  declare block
      end loop;
      Close (Input_File);
      Game_Utils.Game_Log ("Loaded_Mesh_Data_Only mesh data loaded from: " &
                             File_Name);
      return True;
   end Load_Mesh_Data_Only;

   --  ------------------------------------------------------------------------

   function Loaded_Mesh_VAO (Index : Integer;
                             VAO   : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object)
                              return Boolean is
      use Meshes_Package;
      Curs  : Cursor := Loaded_Meshes.First;
      Found : Boolean := False;
   begin
      while Has_Element (Curs) and not Found loop
         Found := To_Index (Curs) = Index;
         if Found then
            VAO := Element (Curs).VAO;
         else
            Next (Curs);
         end if;
      end loop;

      return Found;
   end Loaded_Mesh_VAO;

   --  ------------------------------------------------------------------------

   function Point_Count (Index : Integer) return Integer is
      use Meshes_Package;
      Curs  : Cursor := Loaded_Meshes.First;
      Found : Boolean := False;
      Count : Integer := 0;
   begin
      while Has_Element (Curs) and not Found loop
         Found := To_Index (Curs) = Index;
         if Found then
            Count := Element (Curs).Point_Count;
         else
            Next (Curs);
         end if;
      end loop;

      return Count;
   end Point_Count;

   --  ------------------------------------------------------------------------

end Mesh_Loader;
