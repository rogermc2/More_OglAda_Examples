
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with GL.Attributes;

with Shader_Attributes;
with Game_Utils;

package body Mesh_Loader is
   use GL.Objects.Vertex_Arrays;
   use GL.Types.Singles;

   Meshes : Mesh_List;

   function Loaded_Mesh_Animation (Mesh_ID : Integer; Anim_ID : Positive)
                                   return Animation;
   function Load_Mesh (Path : String; Mesh_ID : out Integer) return Boolean;
   procedure Load_Normal_Data (Input_File : File_Type; Vec : in out Vector3);
   procedure Load_Point_Data (Input_File : File_Type; Vec : in out Vector3);

   --  ------------------------------------------------------------------------

   function Animation_Duration (Mesh_ID : Integer; Anim_ID : Positive)
                                return Float is
   begin
      return  Loaded_Mesh_Animation (Mesh_ID, Anim_ID).Duration;
   end Animation_Duration;

   --  ------------------------------------------------------------------------

   function Bone_Count (Index : Integer) return Integer is
      use Meshes_Package;
      Curs  : Cursor := Meshes.First;
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
      Meshes.Clear;
   end Init;

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
      if not Meshes.Is_Empty then
         Index := Meshes.First_Index;
         while Index <= Meshes.Last_Index and not Found loop
            aMesh := Meshes.Element (index);
            Found := aMesh.File_Name = To_Unbounded_String (Mesh_Name);
            if Found then
               Mesh_ID := Index;
            else
               Index := Index + 1;
            end if;
         end loop;
      end if;

      if not Found then
         Game_Utils.Game_Log ("Mesh_Loader.Load_Managed_Mesh Load_Mesh loading " &
                              Mesh_Name);
--           if not Load_Mesh (Mesh_Name, Meshes, Mesh_ID) then
         if not Load_Mesh (Mesh_Name, Mesh_ID) then
            raise Mesh_Loader_Exception with
            "Mesh_Loader.Load_Managed_Mesh couldn't load " & Mesh_Name;
         end if ;
      end if;

      aMesh := Meshes.Element (Mesh_ID);
      aMesh.VAO.Clear;
      aMesh.VAO.Initialize_Id;
      GL_Utils.Bind_VAO (aMesh.VAO);
      Meshes.Replace_Element (Mesh_ID, aMesh);

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

      aMesh := Meshes.Element (Meshes.Last_Index);
      aMesh.Shadow_VAO.Clear;
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

      Meshes.Replace_Element (Mesh_ID, aMesh);
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
         Meshes.Append (New_Mesh);
         Mesh_ID := Meshes.Last_Index;
      else
         Game_Utils.Game_Log ("Mesh_Loader.Load_Mesh mesh data not created for "
                              & Path);
      end if;

      return Result;
   end Load_Mesh;

   --  ------------------------------------------------------------------------

    function Loaded_Meshes return Mesh_List is
    begin
      return Meshes;
   end Loaded_Meshes;

   --  ------------------------------------------------------------------------

   function Load_Mesh_Data_Only (File_Name   : String;
                                 Points      : in out GL_Maths.Vec3_List;
                                 Tex_Coords  : in out GL_Maths.Vec2_List;
                                 Normals     : in out GL_Maths.Vec3_List)
                                 return Boolean is
      use Ada.Strings;
      Input_File : File_Type;
      Point_Count   : Integer := 0;
   begin
      Game_Utils.Game_Log ("Load_Mesh_Data_Only loading mesh data from: " &
                             File_Name);
      Open (Input_File, In_File, File_Name);
      while not End_Of_File (Input_File) loop
         declare
            aString       : constant String := Get_Line (Input_File);
            String_Length : constant Integer := aString'Length;
            Comps         : Integer := 0;
            Vec3          : Vector3 := (0.0, 0.0, 0.0);
         begin
--              Game_Utils.Game_Log ("Loaded_Mesh_Data_Only String: " & aString);
            if aString (1 .. 1) = "@" then
               if String_Length > 8 and aString (2 .. 9) = "Anton's " then
                  null;
               elsif String_Length > 12 and then aString (2 .. 12) = "vert_count " then
                  Point_Count := Integer'Value (aString (13 .. aString'Last));
--                    Put_Line ("Mesh_Loader.Loaded_Mesh_Data_Only Point_Count: " &
--                                Integer'Image (Point_Count));
               elsif String_Length > 9 and then aString (2 .. 10) = "vp comps " then
                  Comps  := Integer'Value (aString (11 .. aString'Last));
                  if Comps /= 3 then
                            raise Mesh_Loader_Exception with
                            "Mesh_Loader.Loaded_Mesh_Data_Only, invalid point vector dimession:" &
                              Integer'Image (Comps);
                  end if;
--                    Put_Line ("Mesh_Loader.Loaded_Mesh_Data_Only loading Point_Data, Point_Count: " &
--                                Integer'Image (Point_Count));
                  if Point_Count > 0 then
                     for index in 1 .. Point_Count loop
                        Load_Point_Data (Input_File, Vec3);
                        Points.Append (Vec3);
                     end loop;
                  end if;

               elsif String_Length > 9 and then aString (2 .. 10) = "vn comps " then
                  Comps  := Integer'Value (aString (11 .. aString'Last));
                  if Comps /= 3 then
                            raise Mesh_Loader_Exception with
                            "Mesh_Loader.Loaded_Mesh_Data_Only, invalid normals vector dimession:" &
                              Integer'Image (Comps);
                  end if;
                  if Point_Count > 0 then
                     for index in 1 .. Point_Count loop
                        Load_Normal_Data (Input_File, Vec3);
                        Normals.Append (Vec3);
                     end loop;
                  end if;

               elsif String_Length > 9 and then aString (2 .. 10) = "vt comps " then
                  Comps := Integer'Value (aString (11 .. aString'Last));
                  if Comps /= 2 then
                            raise Mesh_Loader_Exception with
                            "Mesh_Loader.Loaded_Mesh_Data_Only, invalid texture vector dimession:" &
                              Integer'Image (Comps);
                  end if;
                  if Point_Count > 0 then
                     for index in 1 .. Point_Count loop
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

   procedure Load_Normal_Data (Input_File : File_Type; Vec : in out Vector3) is
      use Ada.Strings;
      Data  : constant String := Get_Line (Input_File);
      Pos1  : constant Natural := Fixed.Index (Data, " ");
      Pos2  : Natural;
   begin
      Vec (GL.X) :=
        Single (Single'Value (Data (1 .. Pos1 - 1)));
      Pos2 :=
        Fixed.Index (Data (Pos1 + 1 .. Data'Last), " ");
      Vec (GL.Y) :=
        Single (Single'Value (Data (Pos1 + 1 .. Pos2 - 1)));
      Vec (GL.Z) :=
        Single (Single'Value (Data (Pos2 + 1 .. Data'Last)));
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
        Fixed.Index (Data (Pos1 + 1 .. Data'Last), " ");
      Vec (GL.Y) :=
        Single'Value (Data (Pos1 + 1 .. Pos2 - 1));
      Vec (GL.Z) :=
        Single'Value (Data (Pos2 + 1 .. Data'Last));
   end Load_Point_Data;

   --  ------------------------------------------------------------------------

   function Loaded_Mesh_VAO (Mesh_ID : Integer;
                             VAO   : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object)
                             return Boolean is
      use Meshes_Package;
      Curs  : Cursor := Meshes.First;
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
      Mesh_Curs  : Meshes_Package.Cursor := Meshes.First;
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
      Curs  : Cursor := Meshes.First;
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
