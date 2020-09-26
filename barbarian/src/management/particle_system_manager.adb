
with Ada.Exceptions;
with Ada.Streams.Stream_IO; use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

with Game_Utils;
with Settings;
with Texture_Manager;

package body Particle_System_Manager is

   procedure Load_Attribute_Data (PS                         : in out Particle_Script;
                                  Min_Velocity, Max_Velocity : Singles.Vector3);

   -- -------------------------------------------------------------------------

   procedure Load_Particle_Script (File_Name : String;
                                   Scripts   : in out Particle_Script_List) is
      Input_File       : Stream_IO.File_Type;
      Input_Stream     : Stream_IO.Stream_Access;
      aLine            : Unbounded_String;
      Stream_Index     : Stream_IO.Count;
      Min_Velocity     : Singles.Vector3 := Maths.Vec3_0;
      Max_Velocity     : Singles.Vector3 := Maths.Vec3_0;
      Script           : Particle_Script;
   begin
      Game_Utils.Game_Log ("Particle System Manager loading " & File_Name);
      Stream_IO.Open (Input_File, Stream_IO.In_File, "particles/" & File_Name);
      Input_Stream := Stream_IO.Stream (Input_File);
      Script.Script_Name := To_Unbounded_String (File_Name);
      while not Stream_IO.End_Of_File (Input_File) loop
         Stream_Index := Stream_IO.Index (Input_File);
         Unbounded_String'Read (Input_Stream, aLine);
         declare
            use Stream_IO;
            aString        : constant String := To_String (aLine);
            Pos            : constant Natural := Index (aLine, " ");
            Pos_M1         : constant Natural := Pos - 1;
            Pos_P1         : constant Natural := Pos + 1;
            Val_Pos        : constant Stream_IO.Count :=
                               Stream_Index + Stream_IO.Count (Pos_P1);
            S_Length       : constant Integer := aString'Length;
            Texture_String : Unbounded_String;
            SRGB           : constant Boolean := True;
         begin
            if aString (1 .. 1) = "#" then
               null;
            elsif aString (1 .. Pos_M1) = "total_particles" then
               Script.Particle_Count := Int'Value (aString (Pos_P1 .. S_Length));
            elsif aString (1 .. Pos_M1) = "max_initial_velocity" then
               Set_Index (Input_File, Val_Pos);
               Singles.Vector3'Read (Input_Stream, Max_Velocity);
            elsif aString (1 .. Pos_M1) = "min_initial_velocity" then
               Set_Index (Input_File, Val_Pos);
               Singles.Vector3'Read (Input_Stream, Min_Velocity);
            elsif aString (1 .. Pos_M1) = "acceleration" then
               Set_Index (Input_File, Val_Pos);
               Singles.Vector3'Read (Input_Stream, Script.Acceleration);
            elsif aString (1 .. Pos_M1) = "initial_colour" then
               Set_Index (Input_File, Val_Pos);
               Singles.Vector4'Read (Input_Stream, Script.Initial_Colour);
            elsif aString (1 .. Pos_M1) = "final_colour" then
               Set_Index (Input_File, Val_Pos);
               Singles.Vector4'Read (Input_Stream, Script.Final_Colour);
            elsif aString (1 .. Pos_M1) = "total_system_seconds" then
               Script.Total_System_Seconds :=
                 Int'Value (aString (Pos_P1 .. S_Length));
            elsif aString (1 .. Pos_M1) = "particle_lifetime" then
               Script.Total_System_Seconds :=
                 Int'Value (aString (Pos_P1 .. S_Length));
            elsif aString (1 .. Pos_M1) = "seconds_between_emissions" then
               Script.Seconds_Between :=
                 Int'Value (aString (Pos_P1 .. S_Length));
            elsif aString (1 .. Pos_M1) = "initial_scale" then
               Script.Initial_Scale :=
                 Single'Value (aString (Pos_P1 .. S_Length));
            elsif aString (1 .. Pos_M1) = "final_scale" then
               Script.Final_Scale :=
                 Single'Value (aString (Pos_P1 .. S_Length));
            elsif aString (1 .. Pos_M1) = "degrees_per_second" then
               Script.Degrees_Per_Second :=
                 Single'Value (aString (Pos_P1 .. S_Length));
            elsif aString (1 .. Pos_M1) = "rotate_emitter_around_offs" then
               Set_Index (Input_File, Val_Pos);
               Singles.Vector3'Read
                 (Input_Stream, Script.Rotate_Emitter_Around_Offs);
            elsif aString (1 .. Pos_M1) = "rotate_emitter_around_degs_per_s" then
               Set_Index (Input_File, Val_Pos);
               Script.Final_Scale := Single'Value (aString (Pos_P1 .. S_Length));
            elsif aString (1 .. Pos_M1) = "anim_move_emitter_from" then
               Set_Index (Input_File, Val_Pos);
               Singles.Vector3'Read
                 (Input_Stream, Script.Anim_Move_Emitter_From);
            elsif aString (1 .. Pos_M1) = "anim_move_emitter_to" then
               Set_Index (Input_File, Val_Pos);
               Singles.Vector3'Read
                 (Input_Stream, Script.Anim_Move_Emitter_To);
            elsif aString (1 .. Pos_M1) = "bounding_radius" then
               Script.Final_Scale :=
                 Single'Value (aString (Pos_P1 .. S_Length));
            elsif aString (1 .. Pos_M1) = "texture" then
               Set_Index (Input_File, Val_Pos);
               Unbounded_String'Read (Input_Stream, Texture_String);
            elsif not Texture_Manager.Load_Image_To_Texture
              ("textures/" & To_String (Texture_String), Script.Texture,
               Settings.Particle_Mipmaps_Enabled, SRGB) then
               raise Particle_System_Manager_exception with
                 "Particle_System_Manager.Load_Particle_Script Load_Image_To_Texture failed";
            elsif aString (1 .. Pos_M1) = "loops" then
               Script.Is_Looping := aString (Pos_P1 .. Pos_P1) = "1";
            else
               Put_Line
                 ("Particle_System_Manager.Load_Particle_Script, " &
                    "invalid script line: " & aString);
            end if;
         end;  --  declare block
      end loop;
      Stream_IO.Close (Input_File);

      Load_Attribute_Data (Script, Min_Velocity, Max_Velocity);
      Scripts.Append (Script);

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
      Initial_Positions : constant Vector3_Array (1 .. PS.Particle_Count) :=
                            (others => Maths.Vec3_0);
      Age_Offset        : Single_Array (1 .. PS.Particle_Count);
      Delta_V           : constant Vector3 := Max_Velocity - Min_Velocity;
      Velocity          : Vector3;
   begin
      for index in 1 .. PS.Particle_Count loop
         Velocity (X) := Min_Velocity (X) + Maths.Random_Float / Delta_V (X);
         Velocity (Y) := Min_Velocity (Y) + Maths.Random_Float / Delta_V (Y);
         Velocity (Z) := Min_Velocity (Z) + Maths.Random_Float / Delta_V (Z);
         Age_Offset (index) := -Single (index * PS.Seconds_Between);
         PS.Particle_Initial_Velocity.Append (Velocity);
      end loop;

      PS.VAO.Initialize_Id;
      PS.VAO.Bind;

      PS.Particle_World_Positions_VBO.Initialize_Id;
      Array_Buffer.Bind (PS.Particle_World_Positions_VBO);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Initial_Positions, Static_Draw);

      --  VAO_Index means  attribute index!
      GL.Attributes.Set_Vertex_Attrib_Pointer (PS.VAO_Index, 3, Single_Type, False, 0, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (PS.VAO_Index);

      PS.Particle_Ages_VBO.Initialize_Id;
      Array_Buffer.Bind (PS.Particle_Ages_VBO);
      Utilities.Load_Singles_Buffer (Array_Buffer, Age_Offset, Static_Draw);

      GL.Attributes.Set_Vertex_Attrib_Pointer (PS.VAO_Index, 1, Single_Type, False, 0, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (PS.VAO_Index);

   exception
      when anError : others =>
         Put_Line
           ("An exception occurred in Particle_System_Manager.Load_Attribute_Data!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Load_Attribute_Data;

   --  ----------------------------------------------------------------------------

end Particle_System_Manager;
