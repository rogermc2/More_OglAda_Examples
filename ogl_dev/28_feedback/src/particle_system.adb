
with Interfaces.C.Pointers;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Low_Level.Enums;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;

with Ogldev_Engine_Common;

with Random_Texture;

package body Particle_System is

   Max_Particles   : constant GL.Types.Int := 1000;
   type Particle_Type is (Type_Launcher);

   type Particle is record
      Particle_Kind : Particle_Type := Type_Launcher;
      Position      : Singles.Vector3;
      Velocity      : Singles.Vector3 := (0.0, 0.0001, 0.0);
      Lifetime      : GL.Types.Single := 0.0;
   end record;

   type Particle_Array is array (GL.Types.Int range <>) of aliased Particle;
   package Particle_Pointers is new Interfaces.C.Pointers
     (GL.Types.Int, Particle, Particle_Array, Particle'(others => <>));
   procedure Load_Particle_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (Particle_Pointers);

   procedure Render_Particles (PS : in out Particle_System;
                               View_Point : Singles.Matrix4;
                               Camera_Pos : Singles.Vector3);
   procedure Update_Particles (PS : in out Particle_System;
                               Delta_Time : GL.Types.Int);

     --  -------------------------------------------------------------------------

   procedure Init_Particle_System (PS : in out Particle_System;
                                   theTechnique : in out
                                     PS_Update_Technique.Update_Technique;
                                   Update_Program : GL.Objects.Programs.Program;
                                   Pos : Singles.Vector3) is
      use GL.Objects.Buffers;
      Particles : Particle_Array (1 .. Max_Particles);
   begin

      Particles (1).Particle_Kind := Type_Launcher;
      Particles (1).Position := Pos;
      Particles (1).Velocity := (0.0, 0.0001, 0.0);
      Particles (1).Lifetime := 0.0;
      for index in UInt range 1 .. 2 loop
         PS.Transform_Feedback (index).Initialize_Id;
         PS.Particle_Buffer (index).Initialize_Id;
         Transform_Feedback_Buffer.Bind (PS.Transform_Feedback (index));
         Transform_Feedback_Buffer.Bind_Buffer_Base
           (0, PS.Transform_Feedback (index));
         Array_Buffer.Bind (PS.Particle_Buffer (index));
         Load_Particle_Buffer (Array_Buffer, Particles, Dynamic_Draw);
      end loop;

      GL.Objects.Programs.Use_Program (Update_Program);
      PS_Update_Technique.Init (theTechnique, Update_Program);
      PS_Update_Technique.Set_Random_Texture_Unit
          (theTechnique, Ogldev_Engine_Common.Random_Texture_Unit_Index'Enum_Rep);
      PS_Update_Technique.Set_Launcher_Lifetime (theTechnique, 100.0);
      PS_Update_Technique.Set_Shell_Lifetime (theTechnique, 10000.0);
      PS_Update_Technique.Set_Secondary_Shell_Lifetime (theTechnique, 2500.0);

      PS_Update_Technique.Set_Random_Texture_Unit
        (theTechnique, Ogldev_Engine_Common.Random_Texture_Unit_Index'Enum_Rep);
      PS_Update_Technique.Set_Launcher_Lifetime (theTechnique, 100.0);
      PS_Update_Technique.Set_Shell_Lifetime (theTechnique, 10000.0);
      PS_Update_Technique.Set_Secondary_Shell_Lifetime (theTechnique, 2500.0);

      Random_Texture.Init_Random_Texture (PS.Random_Texture, 1000);
      Random_Texture.Bind (PS.Random_Texture,
                           Ogldev_Engine_Common.Random_Texture_Unit_Index'Enum_Rep);

      Billboard_Technique.Init (PS.Billboard_Method);
      Billboard_Technique.Set_Colour_Texture_Unit
        (PS.Billboard_Method, Ogldev_Engine_Common.Colour_Texture_Unit_Index'Enum_Rep);
      Billboard_Technique.Set_Billboard_Size (PS.Billboard_Method, 0.01);

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

   procedure Render (PS : in out Particle_System; Delta_Time : GL.Types.Int;
                     View_Point : Singles.Matrix4; Camera_Pos : Singles.Vector3) is

   begin
      PS.PS_Time := PS.PS_Time + Delta_Time;
      Update_Particles (PS, Delta_Time);
      Render_Particles (PS, View_Point, Camera_Pos);
      PS.Current_VB := PS.Current_TFB;
      PS.Current_TFB := ((PS.Current_TFB + 1) / 2) * 2;
   exception
      when  others =>
         Put_Line ("An exception occurred in Particle_System.Render.");
         raise;
   end Render;

   --  -------------------------------------------------------------------------

   procedure Render_Particles (PS : in out Particle_System;
                               View_Point : Singles.Matrix4;
                               Camera_Pos : Singles.Vector3) is
   begin
      Billboard_Technique.Set_Camera_Position (PS.Billboard_Method, Camera_Pos);
      Billboard_Technique.Set_View_Point (PS.Billboard_Method, View_Point);
      Billboard_Technique.Set_Colour_Texture_Unit
        (PS.Billboard_Method, Ogldev_Engine_Common.Colour_Texture_Unit_Index'Enum_Rep);
      GL.Toggles.Disable (GL.Toggles.Rasterizer_Discard);
      GL.Objects.Buffers.Array_Buffer.Bind (PS.Particle_Buffer (PS.Current_TFB));
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => 3,
                                               Kind   => Single_Type,
                                               Stride => Particle'Size,
                                               Offset => 4);
      GL.Objects.Buffers.Draw_Transform_Feedback
        (Points, PS.Transform_Feedback (PS.Current_TFB));

   exception
      when  others =>
         Put_Line ("An exception occurred in Particle_System.Render_Particles.");
         raise;
   end Render_Particles;

   --  -------------------------------------------------------------------------

   procedure Update_Particles (PS : in out Particle_System;
                               Delta_Time : GL.Types.Int) is
      use PS_Update_Technique;
   begin
      Set_Time (PS.Update_Method, PS.PS_Time);
      Set_Delta_Millisec (PS.Update_Method, Delta_Time);
      Random_Texture.Bind (PS.Random_Texture,
                           Ogldev_Engine_Common.Random_Texture_Unit_Index'Enum_Rep);
      GL.Toggles.Disable (GL.Toggles.Rasterizer_Discard);
      GL.Objects.Buffers.Array_Buffer.Bind (PS.Particle_Buffer (PS.Current_VB));
      GL.Objects.Buffers.Transform_Feedback_Buffer.Bind
        (PS.Transform_Feedback (PS.Current_TFB));
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Enable_Vertex_Attrib_Array (1);
      GL.Attributes.Enable_Vertex_Attrib_Array (2);
      GL.Attributes.Enable_Vertex_Attrib_Array (3);
      GL.Attributes.Set_Vertex_Attrib_Pointer
        (Index  => 0, Count => 1, Kind => Single_Type, Stride => 0, Offset => 0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, 0, 4);
      GL.Attributes.Set_Vertex_Attrib_Pointer (2, 3, Single_Type, 0, 16);
      GL.Attributes.Set_Vertex_Attrib_Pointer (3, 1, Single_Type, 0, 28);

      GL.Objects.Programs.Begin_Transform_Feedback (Points);
      if PS.Is_First then
         GL.Objects.Vertex_Arrays.Draw_Arrays (GL.Types.Points, 0, 1);
         PS.Is_First := False;
      else
         GL.Objects.Buffers.Draw_Transform_Feedback
           (Points, PS.Transform_Feedback (PS.Current_VB));
      end if;
      GL.Objects.Programs.End_Transform_Feedback;

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
