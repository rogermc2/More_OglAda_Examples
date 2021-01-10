
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

with Game_Utils;
with GL_Maths;
with  GL_Utils;
with Settings;
with Shader_Attributes;
with Texture_Manager;

package body Particle_System_Manager is

   procedure Load_Attribute_Data (PS                         : in out Particle_Script;
                                  Min_Velocity, Max_Velocity : Singles.Vector3);

   -- -------------------------------------------------------------------------

   procedure Load_Particle_Script (File_Name : String;
                                   Scripts   : in out Particle_Script_List) is
      use Ada.Strings;
      Input_File       : File_Type;
      Min_Velocity     : Vector3 := Maths.Vec3_0;
      Max_Velocity     : Vector3 := Maths.Vec3_0;
      Script           : Particle_Script;

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
--        Game_Utils.Game_Log ("Particle System Manager loading " & File_Name);
      Open (Input_File, In_File, "src/particles/" & File_Name);
      Script.Script_Name := To_Unbounded_String (File_Name);
      while not End_Of_File (Input_File) loop
         declare
            aLine          : constant String := Get_Line (Input_File);
            Last           : constant Integer := aLine'Length;
            Pos            : constant Natural := Fixed.Index (aLine, " ");
            Head           : constant String := aLine (1 .. Pos - 1);
            Tail           : constant String := aLine (Pos + 1 .. Last);
            SRGB           : constant Boolean := True;
         begin
--              Game_Utils.Game_Log ("Particle System Manager Head '" & Head & "'");
            if aLine (1 .. 1) = "#" then
               null;
            elsif Head = "total_particles" then
               Script.Particle_Count := Integer'Value (Tail);
            elsif Head = "max_initial_velocity" then
               Max_Velocity := Read_Vec3 (Tail);
            elsif Head = "min_initial_velocity" then
               Min_Velocity := Read_Vec3 (Tail);
            elsif Head = "acceleration" then
               Script.Acceleration := Read_Vec3 (Tail);
            elsif Head = "initial_colour" then
               Script.Initial_Colour := Read_Vec4 (Tail);
            elsif Head = "final_colour" then
               Script.Final_Colour := Read_Vec4 (Tail);
            elsif Head = "total_system_seconds" then
               Script.Total_System_Seconds := Single'Value (Tail);
            elsif Head = "particle_lifetime" then
               Script.Total_System_Seconds := Single'Value (Tail);
            elsif Head = "seconds_between_emissions" then
               Script.Seconds_Between := Single'Value (Tail);
            elsif Head = "initial_scale" then
               Script.Initial_Scale := Single'Value (Tail);
            elsif Head = "final_scale" then
               Script.Final_Scale := Single'Value (Tail);
            elsif Head = "degrees_per_second" then
               Script.Degrees_Per_Second := Single'Value (Tail);
            elsif Head = "rotate_emitter_around_offs" then
               Script.Rotate_Emitter_Around_Offs := Read_Vec3 (Tail);
            elsif Head = "rotate_emitter_around_degs_per_s" then
               Script.Final_Scale := Single'Value (Tail);
            elsif Head = "anim_move_emitter_from" then
               Script.Anim_Move_Emitter_From := Read_Vec3 (Tail);
            elsif Head = "anim_move_emitter_to" then
               Script.Anim_Move_Emitter_To := Read_Vec3 (Tail);
            elsif Head = "bounding_radius" then
               Script.Final_Scale := Single'Value (Tail);
            elsif Head = "texture" then
               Texture_Manager.Load_Image_To_Texture
                 ("src/textures/" & Tail, Script.Texture,
                  Settings.Particle_Mipmaps_Enabled, SRGB);
            elsif Head = "loops" then
               Script.Is_Looping := Tail = "1";
            else
               Put_Line
                 ("Particle_System_Manager.Load_Particle_Script, " &
                    "invalid script line: " & aLine);
            end if;
         end;  --  declare block
      end loop;
      Close (Input_File);

      Load_Attribute_Data (Script, Min_Velocity, Max_Velocity);
      Scripts.Append (Script);
--        Game_Utils.Game_Log ("Particle System Manager loaded " & File_Name);

   exception
      when anError : others =>
         Put_Line
           ("An exception occurred in Particle_System_Manager.Load_Particle_Script!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Load_Particle_Script;

   --  ----------------------------------------------------------------------------

   procedure Load_Attribute_Data (PS                         : in out Particle_Script;
                                  Min_Velocity, Max_Velocity : Singles.Vector3) is
      use GL;
      use GL.Objects.Buffers;
      use GL_Maths;
      Initial_Positions : constant Vector3_Array (1 .. Int (PS.Particle_Count)) :=
                            (others => Maths.Vec3_0);
      Age_Offset        : Single_Array (1 .. Int (PS.Particle_Count));
      Delta_V           : constant Vector3 := Max_Velocity - Min_Velocity;
      Velocity          : Vector3;
   begin
      for index in 1 .. PS.Particle_Count loop
         Velocity (X) := Min_Velocity (X) + Maths.Random_Float / Delta_V (X);
         Velocity (Y) := Min_Velocity (Y) + Maths.Random_Float / Delta_V (Y);
         Velocity (Z) := Min_Velocity (Z) + Maths.Random_Float / Delta_V (Z);
         Age_Offset (Int (index)) := -Single (index) * PS.Seconds_Between;
         PS.Particle_Initial_Velocity.Append (Velocity);
      end loop;

      PS.Particle_World_Positions_VBO.Initialize_Id;
      PS.Particle_World_Positions_VBO :=
      GL_Utils.Create_3D_VBO (Initial_Positions);
--        Array_Buffer.Bind (PS.Particle_World_Positions_VBO);
--        Utilities.Load_Vertex_Buffer (Array_Buffer, Initial_Positions, Static_Draw);

      PS.Particle_Ages_VBO.Initialize_Id;
      PS.Particle_Ages_VBO := GL_Utils.Create_1D_VBO (Age_Offset);
      --        Array_Buffer.Bind (PS.Particle_Ages_VBO);
--        Utilities.Load_Singles_Buffer (Array_Buffer, Age_Offset, Static_Draw);

      PS.VAO.Initialize_Id;
      PS.VAO.Bind;

      GL_Utils.Add_Attribute_To_Array
          (PS.VAO, Shader_Attributes.Attrib_Particle_World,
           PS.Particle_World_Positions_VBO, 3);
      --  VAO_Index means  attribute index!
--        GL.Attributes.Set_Vertex_Attrib_Pointer (PS.VAO_Index, 3, Single_Type, False, 0, 0);
--        GL.Attributes.Enable_Vertex_Attrib_Array (PS.VAO_Index);

      GL_Utils.Add_Attribute_To_Array
          (PS.VAO, Shader_Attributes.Attrib_Particle_Age,
           PS.Particle_Ages_VBO, 1);
--        GL.Attributes.Set_Vertex_Attrib_Pointer (PS.VAO_Index, 1, Single_Type, False, 0, 0);
--        GL.Attributes.Enable_Vertex_Attrib_Array (PS.VAO_Index);

   exception
      when anError : others =>
         Put_Line
           ("An exception occurred in Particle_System_Manager.Load_Attribute_Data!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Load_Attribute_Data;

   --  ----------------------------------------------------------------------------

end Particle_System_Manager;
