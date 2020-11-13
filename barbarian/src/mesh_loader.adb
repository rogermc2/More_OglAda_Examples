
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GL.Attributes;
with GL.Objects.Buffers;

with Shader_Attributes;
with Game_Utils;
with GL_Utils;

package body Mesh_Loader is
   use GL.Objects.Vertex_Arrays;
   use GL.Types.Singles;

   type Tra_Anim_Key is record
	Tra  : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
	Time : Float := 0.0;
   end record;

   type Sca_Anim_Key is record
	Sca  : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
	Time : Float := 0.0;
   end record;

   type Rot_Anim_Key is record
	Rot  : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
	Time : Float := 0.0;
   end record;
   --  Channel_Data defines a channel of keys within an animation.
   --  All the keys in anim for a particular animation node,
   --  where an 'animation node' is a bone in the skeleton but doesn't
   --  necessarily have any vertices weighted to it
   --  i.e. can be an in-between joint
   type Channel_Data is record
	Tra_Keys       : Tra_Anim_Key;
	Tra_Keys_Count : Integer := 0;
	Sca_Keys       : Sca_Anim_Key;
	Sca_Keys_Count : Integer := 0;
	Rot_Keys       : Rot_Anim_Key;
        Rot_Keys_Count : Integer := 0;
   end record;

   package Channels_Package is new Ada.Containers.Vectors (Positive, Channel_Data);
   type Channel_List is new Channels_Package.Vector with null record;

   type Animation is record
      Name  : Unbounded_String := To_Unbounded_String ("");
      Duration : Float := 0.0;
      --  Order of channels corresponds to anim nodes in hierarchy
      Channels : Channel_List;
   end record;

   package Animations_Package is new Ada.Containers.Vectors (Positive, Animation);
   type Animations_List is new Animations_Package.Vector with null record;

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
      Bones_Vbo              : GL.Objects.Buffers.Buffer;
      Vtans_Vbo              : GL.Objects.Buffers.Buffer;
   end record;

   package Meshes_Package is new Ada.Containers.Vectors (Positive, Mesh);
   type Mesh_List is new Meshes_Package.Vector with null record;

   Loaded_Meshes : Mesh_List;

   function Loaded_Mesh_Animation (Mesh_ID : Integer; Anim_ID : Positive)
                                   return Animation;
   function Load_Mesh (Path : String; Mesh_ID : out Integer) return Boolean;
   function To_Vector2_Array (Vec : Vector2_Package.Vector)
                              return Singles.Vector2_Array;
   function To_Vector3_Array (Vec : Vector3_Package.Vector)
                              return Singles.Vector3_Array;
   function To_Vector4_Array (Vec : Vector4_Package.Vector)
                              return Singles.Vector4_Array;

   --  ------------------------------------------------------------------------

   function Animation_Duration (Mesh_ID : Integer; Anim_ID : Positive)
                                return Float is
   begin
      return  Loaded_Mesh_Animation (Mesh_ID, Anim_ID).Duration;
   end Animation_Duration;

   --  ------------------------------------------------------------------------

   function Bone_Count (Index : Integer) return Integer is
      use Meshes_Package;
      Curs  : Cursor := Loaded_Meshes.First;
      Found : Boolean := False;
      Count : Integer := 0;
   begin
      while Has_Element (Curs) and not Found loop
         Found := To_Index (Curs) = Index;
         if Found then
            Count := Element (Curs).Bone_Count;
         else
            Next (Curs);
         end if;
      end loop;

      return Count;
   end Bone_Count;

   --  ------------------------------------------------------------------------

   procedure Init is
   begin
      Loaded_Meshes.Clear;
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

   function Load_Managed_Mesh (Mesh_Name               : String;
                               Has_Vp, Has_Vn, Has_Vt,
                               Has_Tangents, Has_Bones : Boolean := False)
                               return Integer is
      use GL.Attributes;
      use GL.Objects.Buffers;
      use Shader_Attributes;
      use Meshes_Package;
      aMesh      : Mesh;
      Found      : Boolean := False;
      Index      : Meshes_Package.Extended_Index;
      Attr_Count : Integer := 0;
      Mesh_ID    : Integer := 0;
   begin
      if not Loaded_Meshes.Is_Empty then
         Index := Loaded_Meshes.First_Index;
         while Index <= Loaded_Meshes.Last_Index and not Found loop
            aMesh := Loaded_Meshes.Element (index);
            Found := aMesh.File_Name = To_Unbounded_String (Mesh_Name);
            if Found then
               Mesh_ID := Index;
            else
               Index := Index + 1;
            end if;
         end loop;
      end if;

      if not Found then
         Game_Utils.Game_Log("Mesh_Loader.Load_Managed_Mesh Load_Mesh loading " &
                              Mesh_Name);
--           if not Load_Mesh (Mesh_Name, Loaded_Meshes, Mesh_ID) then
         if not Load_Mesh (Mesh_Name, Mesh_ID) then
            raise Mesh_Loader_Exception with
            "Mesh_Loader.Load_Managed_Mesh couldn't load " & Mesh_Name;
         end if ;
      end if;

      aMesh := Loaded_Meshes.Element (Mesh_ID);
      aMesh.VAO.Initialize_Id;
      GL_Utils.Bind_VAO (aMesh.VAO);
      Loaded_Meshes.Replace_Element (Mesh_ID, aMesh);

--        Game_Utils.Game_Log ("Mesh_Loader.Load_Managed_Mesh " & Mesh_Name &
--                               " Vp_Count, Vn_Count, Vt_Count " &
--                               Integer'Image (aMesh.Vp_Count) & ", " &
--                               Integer'Image (aMesh.Vn_Count)
--                             & ", " & Integer'Image (aMesh.Vt_Count));

      if Has_Vp and then aMesh.Vp_Count > 0 then
         Array_Buffer.Bind (aMesh.Points_Vbo);
         Set_Vertex_Attrib_Pointer (Attrib_VP, 3, Single_Type, False, 0, 0);
         Enable_Vertex_Attrib_Array (Attrib_VP);
      end if;

      if Has_Vn and then aMesh.Vn_Count > 0 then
         Array_Buffer.Bind (aMesh.Normals_Vbo);
         Set_Vertex_Attrib_Pointer (Attrib_VN, 3, Single_Type, False, 0, 0);
         Enable_Vertex_Attrib_Array (Attrib_VN);
      end if;

      if Has_Vt and then aMesh.Vt_Count > 0 then
         Array_Buffer.Bind (aMesh.Texcoords_Vbo);
         Set_Vertex_Attrib_Pointer (Attrib_VT, 2, Single_Type, False, 0, 0);
         Enable_Vertex_Attrib_Array (Attrib_VT);
      end if;

      if Has_Tangents and then aMesh.Vtan_Count > 0 then
         Array_Buffer.Bind (aMesh.Vtans_Vbo);
         Set_Vertex_Attrib_Pointer (Attrib_VTangent, 4, Single_Type, False, 0, 0);
         Enable_Vertex_Attrib_Array (Attrib_VTangent);
      end if;

      if Has_Bones and then aMesh.Vb_Count > 0 then
         Array_Buffer.Bind (aMesh.Bones_Vbo);
         Set_Vertex_Attrib_Pointer (Attrib_Bone, 1, Single_Type, False, 0, 0);
         Enable_Vertex_Attrib_Array (Attrib_Bone);
      end if;

      aMesh := Loaded_Meshes.Element (Loaded_Meshes.Last_Index);
      aMesh.Shadow_VAO.Initialize_Id;
      GL_Utils.Bind_VAO (aMesh.Shadow_VAO);
      if Has_Vp and then aMesh.Vp_Count > 0 then
         Array_Buffer.Bind (aMesh.Points_Vbo);
         Set_Vertex_Attrib_Pointer (Attrib_VP, 3, Single_Type, False, 0, 0);
         Enable_Vertex_Attrib_Array (Attrib_VP);
      end if;

      if Has_bones and then aMesh.Vb_Count > 0 then
         Array_Buffer.Bind (aMesh.Bones_Vbo);
         Set_Vertex_Attrib_Pointer (Attrib_Bone, 1, Single_Type, False, 0, 0);
         Enable_Vertex_Attrib_Array (Attrib_Bone);
      end if;

      Loaded_Meshes.Replace_Element (Mesh_ID, aMesh);
--        Game_Utils.Game_Log("Mesh_Loader.Load_Managed_Mesh Managed_Meshe " &
--                              Mesh_Name & " loaded.");
      return Mesh_ID;
   end Load_Managed_Mesh;

   --  ------------------------------------------------------------------------

--     function Load_Mesh (Path : String; Meshes : in out Mesh_List;
   function Load_Mesh (Path : String; Mesh_ID : out Integer) return Boolean is
      use Ada.Strings;
      use GL_Utils;
      VBS                  : Vector3_Package.Vector;
      VPS                  : Vector3_Package.Vector;
      VNS                  : Vector3_Package.Vector;
      VTS                  : Vector2_Package.Vector;
      VTans                : Vector4_Package.Vector;
      Vp_Comps             : Integer := 0;
      Vn_Comps             : Integer := 0;
      Vt_Comps             : Integer := 0;
      Vtan_Comps           : Integer := 0;
      Vb_Comps             : Integer := 0;
      Offset_Mat_Comps     : Integer := 0;
      Current_Anim_Index   : Integer := -1;
      Input_File           : File_Type;
      New_Mesh             : Mesh;
      Result               : Boolean := False;
   begin
      --        Game_Utils.Game_Log ("Mesh_Loader.Load_Mesh loading " & Path);
      Mesh_ID := -1;
      Open (Input_File, In_File, Path);
      New_Mesh.File_Name := To_Unbounded_String (Path);
      while not End_Of_File (Input_File) loop
         declare
            aLine          : constant String := Get_Line (Input_File);
            Last           : constant Integer := aLine'Length;
            Pos            : constant Natural := Fixed.Index (aLine, " ");
            Head           : constant String := aLine (2 .. Pos - 1);
            Tail           : constant String := aLine (Pos + 1 .. Last);
            Pos2           : constant Natural := Fixed.Index (Tail, " ");
         begin
            --              Game_Utils.Game_Log ("Mesh_Loader.Load_Mesh aLine " & aLine);
            if aLine (1) = '@' then
               if Head = "Anton's" then
                  null;
               elsif Head = "vert_count" then
                  New_Mesh.Point_Count := Integer'Value (Tail);
               elsif Head = "vp" then
                  Vp_Comps := Integer'Value (Tail(Pos2 + 1 .. Last));
                  if Vp_Comps = 3 then
                     for index in 1 .. New_Mesh.Point_Count loop
                        VPS.Append (Read_Vec3 (Get_Line (Input_File)));
                     end loop;
                  end if;
               elsif Head = "vn" then
                  Vn_Comps := Integer'Value (Tail (Pos2 + 1 .. Last));
                  if Vn_Comps = 3 then
                     for index in 1 .. New_Mesh.Point_Count loop
                        VNS.Append (Read_Vec3 (Get_Line (Input_File)));
                     end loop;
                  end if;
               elsif Head = "vt" then
                  Vt_Comps := Integer'Value (Tail (Pos2 + 1 .. Last));
                  if Vt_Comps = 2 then
                     for index in 1 .. New_Mesh.Point_Count loop
                        VTS.Append (Read_Vec2 (Get_Line (Input_File)));
                     end loop;
                  end if;
               elsif Head = "vtan" then
                  Vtan_Comps := Integer'Value (Tail (Pos2 + 1 .. Last));
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
               Put_Line ("Mesh_Loader.Load_Mesh, skeleton not yet implemented");
            elsif Head = "root_transform" then
               Put_Line ("Mesh_Loader.Load_Mesh, root_transform not yet implemented");
            elsif Head = "offset_mat" then
               Put_Line ("Mesh_Loader.Load_Mesh, offset_mat not yet implemented");
            elsif Head = "hierarchy" then
               Put_Line ("Mesh_Loader.Load_Mesh, hierarchy not yet implemented");
            elsif Head = "animation" then
               Put_Line ("Mesh_Loader.Load_Mesh, animation not yet implemented");
            elsif Head = "tra_keys" then
               Put_Line ("Mesh_Loader.Load_Mesh, tra_keys not yet implemented");
            elsif Head = "sca_keys" then
               Put_Line ("Mesh_Loader.Load_Mesh, sca_keys not yet implemented");
            elsif Head = "rot_keys" then
               Put_Line ("Mesh_Loader.Load_Mesh, rot_keys not yet implemented");
            elsif Head = "bounding_radius" then
               New_Mesh.Bounding_Radius := Float'Value (Tail);
            end if;
         end; --  declare block
      end loop;
      Close (Input_File);

      if not VTans.Is_Empty then
         New_Mesh.Vtan_Count := New_Mesh.Point_Count;
         New_Mesh.Vtans_Vbo := Create_4D_VBO (To_Vector4_Array (VTanS));
      end if;

      if not VBS.Is_Empty then
         New_Mesh.Vb_Count := New_Mesh.Point_Count;
         New_Mesh.Bones_Vbo := Create_3D_VBO (To_Vector3_Array (VBS));
      end if;

      if not VTS.Is_Empty then
         New_Mesh.Vt_Count := New_Mesh.Point_Count;
         New_Mesh.Texcoords_Vbo := Create_2D_VBO (To_Vector2_Array (VTS));
      end if;

      if not VNS.Is_Empty then
         New_Mesh.Vn_Count := New_Mesh.Point_Count;
         New_Mesh.Normals_Vbo := Create_3D_VBO (To_Vector3_Array (VNS));
      end if;

      if not VPS.Is_Empty then
         New_Mesh.Vp_Count := New_Mesh.Point_Count;
         New_Mesh.Points_Vbo := Create_3D_VBO (To_Vector3_Array (VPS));
      end if;

      Result := New_Mesh.Point_Count > 0;
      if Result then
         Loaded_Meshes.Append (New_Mesh);
         Mesh_ID := Loaded_Meshes.Last_Index;
      else
         Game_Utils.Game_Log ("Mesh_Loader.Load_Mesh mesh data not created for "
                              & Path);
      end if;

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
--        Game_Utils.Game_Log ("Loaded_Mesh_Data_Only loading mesh data from: " &
--                               File_Name);
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

      return True;
   end Load_Mesh_Data_Only;

   --  ------------------------------------------------------------------------

   function Loaded_Mesh_VAO (Mesh_ID : Integer;
                             VAO   : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object)
                             return Boolean is
      use Meshes_Package;
      Curs  : Cursor := Loaded_Meshes.First;
      Found : Boolean := False;
   begin
      while Has_Element (Curs) and not Found loop
         Found := To_Index (Curs) = Mesh_ID;
         if Found then
            VAO := Element (Curs).VAO;
            if VAO = Null_Array_Object then
               raise Mesh_Loader_Exception with
                 "Mesh_Loader.Loaded_Mesh_VAO, a Loaded Mesh VAO has not been initialised";
            end if;
         else
            Next (Curs);
         end if;
      end loop;
      return Found;

   end Loaded_Mesh_VAO;

   --  ------------------------------------------------------------------------

   function Loaded_Mesh_Animation (Mesh_ID : Integer; Anim_ID : Positive)
                                   return Animation is
      use Meshes_Package;
      use Animations_Package;
      Mesh_Curs  : Meshes_Package.Cursor := Loaded_Meshes.First;
      Anim_List  : Animations_List;
      Anim_Curs  : Animations_Package.Cursor;
      Mesh_Found : Boolean := False;
      Anim_Found : Boolean := False;
      theAnim    : Animation;
   begin
      while Has_Element (Mesh_Curs) and not Mesh_Found loop
         Mesh_Found := To_Index (Mesh_Curs) = Mesh_ID;
         if Mesh_Found then
            Anim_List := Element (Mesh_Curs).Animations;
            if Is_Empty (Anim_List) then
               raise Mesh_Loader_Exception with
                 "Mesh_Loader.Loaded_Mesh_Animation, the Animation List is empty";
            end if;
            Anim_Curs := Anim_List.First;
            while Has_Element (Mesh_Curs) and not Anim_Found loop
               Anim_Found := To_Index (Anim_Curs) = Anim_ID;
               if Anim_Found then
                  theAnim := Element (Anim_Curs);
               else
                  Next (Anim_Curs);
               end if;
            end loop;
         else
            Next (Mesh_Curs);
         end if;
      end loop;
      return theAnim;

   end Loaded_Mesh_Animation;

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

   function To_Vector2_Array (Vec : Vector2_Package.Vector)
                              return Singles.Vector2_Array is
      use Vector2_Package;
      Curs      : Cursor := Vec.First;
      Vec_Array : Singles.Vector2_Array (1 .. Int (Vec.Length));
   begin
      for index in Int range Vec_Array'Range loop
         Vec_Array (index) := Vec (Curs);
         Next  (Curs);
      end loop;
      return Vec_Array;

   end To_Vector2_Array;

   --  ------------------------------------------------------------------------

   function To_Vector3_Array (Vec : Vector3_Package.Vector)
                              return Singles.Vector3_Array is
      use Vector3_Package;
      Curs      : Cursor := Vec.First;
      Vec_Array : Singles.Vector3_Array (1 .. Int (Vec.Length));
   begin
      for index in Int range Vec_Array'Range loop
         Vec_Array (index) := Vec (Curs);
         Next  (Curs);
      end loop;
      return Vec_Array;

   end To_Vector3_Array;

   --  ------------------------------------------------------------------------

   function To_Vector4_Array (Vec : Vector4_Package.Vector)
                              return Singles.Vector4_Array is
      use Vector4_Package;
      Curs      : Cursor := Vec.First;
      Vec_Array : Singles.Vector4_Array (1 .. Int (Vec.Length));
   begin
      for index in Int range Vec_Array'Range loop
         Vec_Array (index) := Vec (Curs);
         Next  (Curs);
      end loop;
      return Vec_Array;

   end To_Vector4_Array;

   --  ------------------------------------------------------------------------

end Mesh_Loader;
