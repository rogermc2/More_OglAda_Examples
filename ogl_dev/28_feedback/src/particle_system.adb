
with Interfaces.C.Pointers;

with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Low_Level.Enums;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Uniforms;

with Ogldev_Engine_Common;

with Random_Texture;

package body Particle_System is

   type Particle_Type is new Single range 0.0 .. 3.0;

   Max_Particles     : constant GL.Types.UInt := 1000;  --  1000;
   Particle_Launcher : constant Particle_Type := 0.0;
   Shell             : constant Particle_Type := 1.0;
   Secondary_Shell   : constant Particle_Type := 2.0;
   None              : constant Particle_Type := 3.0;

   type Particle is record
      Particle_Kind : Particle_Type := Particle_Launcher;
      Position      : Singles.Vector3 := (1.0, 1.0, 0.0);   --  (0.0, 0.0, 0.0)
      Velocity      : Singles.Vector3 := (0.0, 0.0, 0.0);
      Lifetime_ms   : GL.Types.Single := 0.0;
   end record;
   pragma Convention (C, Particle);
   Particle_Stride  : constant Int := Particle'Size / Single'Size;

   type Particle_Array is array (GL.Types.UInt range <>) of aliased Particle;

   package Particle_Pointers is new Interfaces.C.Pointers
     (GL.Types.UInt, Particle, Particle_Array, Particle'(others => <>));
   procedure Load_Particle_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (Particle_Pointers);

   procedure Render_Particles (PS         : in out Particle_System;
                               View_Point : Singles.Matrix4;
                               Camera_Pos : Singles.Vector3);
   procedure Update_Particles (PS         : in out Particle_System;
                               Delta_Time : GL.Types.UInt);

   --  -------------------------------------------------------------------------

   function Get_TFB_Index (PS  : Particle_System) return Buffer_Index is
      begin
      if PS.Current_VB_Index = 2 then
         return 1;
      else
         return 2;
      end if;
   end  Get_TFB_Index;

   --  -------------------------------------------------------------------------

   procedure Init_Particle_System (PS  : in out Particle_System;
                                   Pos : Singles.Vector3) is
      use GL.Objects.Buffers;
      Particles : Particle_Array (1 .. Max_Particles);
   begin
      Particles (1).Particle_Kind := Particle_Launcher;
      Particles (1).Position := Pos;
      Particles (1).Velocity := (0.0, 0.0001, 0.0);
      Particles (1).Lifetime_ms := 0.0;

      for index in Buffer_Index loop
         PS.Feedback_Buffer (index).Initialize_Id;
         GL.Objects.Buffers.Bind_Transform_Feedback (PS.Feedback_Buffer (index));

         PS.Particle_Buffer (index).Initialize_Id;
         Array_Buffer.Bind (PS.Particle_Buffer (index));

         Load_Particle_Buffer (Array_Buffer, Particles, Dynamic_Draw);
         Transform_Feedback_Buffer.Bind_Buffer_Base (0, PS.Particle_Buffer (index));
      end loop;

      PS_Update_Technique.Init (PS.Update_Method);
      PS_Update_Technique.Use_Program (PS.Update_Method);
      PS_Update_Technique.Set_Random_Texture_Unit
        (PS.Update_Method, Ogldev_Engine_Common.Random_Texture_Unit);
      PS_Update_Technique.Set_Launcher_Lifetime (PS.Update_Method, 2000.0); --  100.0
      PS_Update_Technique.Set_Shell_Lifetime (PS.Update_Method, 10000.0);   --  10000.0)
      PS_Update_Technique.Set_Secondary_Shell_Lifetime (PS.Update_Method, 2500.0);  --  2500.0

      Random_Texture.Init_Random_Texture (PS.Random_Texture, Max_Particles);
      Random_Texture.Bind (PS.Random_Texture,
                           Ogldev_Engine_Common.Random_Texture_Unit);

      Billboard_Technique.Init (PS.Display_Method);
      Billboard_Technique.Use_Program (PS.Display_Method);
      Billboard_Technique.Set_Colour_Texture_Unit
        (PS.Display_Method, Ogldev_Engine_Common.Colour_Texture_Unit);
      Billboard_Technique.Set_Billboard_Size (PS.Display_Method, 0.08);  --  orig 0.01

      if Ogldev_Texture.Init_Texture (PS.Texture, GL.Low_Level.Enums.Texture_2D,
                                      "../Content/fireworks_red.jpg") then
         Ogldev_Texture.Load (PS.Texture);
      end if;

   exception
      when  others =>
         Put_Line ("An exception occurred in Particle_System.Init_Particle_System.");
         raise;
   end Init_Particle_System;

   --  -------------------------------------------------------------------------

   procedure Render (PS         : in out Particle_System; Delta_Time : GL.Types.UInt;
                     View_Point : Singles.Matrix4; Camera_Pos : Singles.Vector3) is
   begin
      Update_Particles (PS, Delta_Time);
      Render_Particles (PS, View_Point, Camera_Pos);
      --  Swap buffers
      PS.Current_VB_Index := Get_TFB_Index (PS);
      PS.PS_Time := PS.PS_Time + Delta_Time;

   exception
      when  others =>
         Put_Line ("An exception occurred in Particle_System.Render.");
         raise;
   end Render;

   --  -------------------------------------------------------------------------

   procedure Render_Particles (PS         : in out Particle_System;
                               View_Point : Singles.Matrix4;
                               Camera_Pos : Singles.Vector3) is
   begin
      Billboard_Technique.Use_Program (PS.Display_Method);
      Billboard_Technique.Set_Camera_Position (PS.Display_Method, Camera_Pos);
      Billboard_Technique.Set_View_Point (PS.Display_Method, View_Point);

      Ogldev_Texture.Bind (PS.Texture, Ogldev_Engine_Common.Colour_Texture_Unit);

      GL.Toggles.Disable (GL.Toggles.Rasterizer_Discard);
      GL.Objects.Buffers.Array_Buffer.Bind (PS.Particle_Buffer (PS.Current_VB_Index));

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => 3,
                                               Kind   => Single_Type,
                                               Stride => 0,
                                               Offset => 1);
      --  Draw_Transform_Feedback is equivalent to calling
      --  GL.Objects.VertexArrays.Draw_Arrays with mode as specified,
      --  first set to zero and count set to the number of vertices
      --  captured on vertex stream zero the last time transform feedback was active on the transform feedback object named by id
      GL.Objects.Buffers.Draw_Transform_Feedback
        (Points, PS.Feedback_Buffer (Get_TFB_Index (PS)));
      GL.Attributes.Disable_Vertex_Attrib_Array (0);

   exception
      when  others =>
         Put_Line ("An exception occurred in Particle_System.Render_Particles.");
         raise;
   end Render_Particles;

   --  -------------------------------------------------------------------------

   procedure Update_Particles (PS         : in out Particle_System;
                               Delta_Time : GL.Types.UInt) is
      VB_Index  : constant Buffer_Index := PS.Current_VB_Index;
      TFB_Index : constant Buffer_Index := Get_TFB_Index (PS);
   begin
      PS_Update_Technique.Use_Program (PS.Update_Method);
      PS_Update_Technique.Set_Time (PS.Update_Method, PS.PS_Time);
      PS_Update_Technique.Set_Delta_Millisec (PS.Update_Method, Delta_Time);
--        Put_Line ("Particle_System.Update_Particles PS.PS_Time, Delta_Time." &
--                    GL.Types.UInt'Image (PS.PS_Time) & GL.Types.UInt'Image (Delta_Time));

      Random_Texture.Bind (PS.Random_Texture,
                           Ogldev_Engine_Common.Random_Texture_Unit);
      --  Rasterizer_Discard causes vertices to be recorded into the
      --  output transform feedback buffers without anything being rasterized.
      GL.Toggles.Enable (GL.Toggles.Rasterizer_Discard);

      --  Bind the current vertex buffer to the Array Buffer
      GL.Objects.Buffers.Array_Buffer.Bind (PS.Particle_Buffer (VB_Index));
      --  Binding a transform feedback object causes the number of vertices
      --  in the buffer to become zero.
      GL.Objects.Buffers.Bind_Transform_Feedback (PS.Feedback_Buffer (TFB_Index));

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Enable_Vertex_Attrib_Array (1);
      GL.Attributes.Enable_Vertex_Attrib_Array (2);
      GL.Attributes.Enable_Vertex_Attrib_Array (3);

      GL.Attributes.Set_Vertex_Attrib_Pointer
        (Index  => 0, Count => 1, Kind => Single_Type,
         Stride => 0, Offset => 0);                                       --  Particle Type
      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, 0, 1);  --  Position
      GL.Attributes.Set_Vertex_Attrib_Pointer (2, 3, Single_Type, 0, 4);  --  Velocity
      GL.Attributes.Set_Vertex_Attrib_Pointer (3, 1, Single_Type, 0, 7);  --  Age

      --  Transform feedback mode captures the values of varying variables
      --  written by the geometry shader).
      GL.Objects.Programs.Begin_Transform_Feedback (Points);
      if PS.Is_First then
         --  Write the current vertex buffer into the currently bound
         --  transform feedback buffer
         GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, 1);
         PS.Is_First := False;
      else
         --  Transform Feedback Varyings were set by PS.Update_Technique.Init
         --  PS.Feedback_Buffer (VB_Index) is processed by the geometry shader
         --  and the results (varyings) are written into the
         --  currently bound transform feedback buffer (TFB_Index):
         GL.Objects.Buffers.Draw_Transform_Feedback
           (Points, PS.Feedback_Buffer (VB_Index));
      end if;
      GL.Objects.Programs.End_Transform_Feedback;
      GL.Flush;

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
      GL.Attributes.Disable_Vertex_Attrib_Array (1);
      GL.Attributes.Disable_Vertex_Attrib_Array (2);
      GL.Attributes.Disable_Vertex_Attrib_Array (3);

   exception
      when  others =>
         Put_Line ("An exception occurred in Particle_System.Update_Particles.");
         raise;
   end Update_Particles;

   --  -------------------------------------------------------------------------

end Particle_System;
