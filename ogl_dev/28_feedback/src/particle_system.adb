
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

   type Particle_Type is new Single range 0.0 .. 2.0;

   Max_Particles   : constant GL.Types.Int := 1000;
   Particle_Launcher : constant Particle_Type := 0.0;
   Shell             : constant Particle_Type := 1.0;
   Secondary_Shell   : constant Particle_Type := 2.0;

   type Particle is record
      Particle_Kind : Particle_Type := Particle_Launcher;
      Position      : Singles.Vector3;
      Velocity      : Singles.Vector3 := (0.0, 0.0001, 0.0);
      Lifetime      : GL.Types.Single := 0.0;
   end record;

   type Particle_Array is array (GL.Types.Int range <>) of aliased Particle;
   package Particle_Pointers is new Interfaces.C.Pointers
     (GL.Types.Int, Particle, Particle_Array, Particle'(others => <>));
   procedure Load_Particle_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (Particle_Pointers);

   procedure Render_Particles (PS         : in out Particle_System;
                               View_Point : Singles.Matrix4;
                               Camera_Pos : Singles.Vector3);
   procedure Update_Particles (PS         : in out Particle_System;
                               Delta_Time : GL.Types.Int);

   --  -------------------------------------------------------------------------

   function Get_Billboard_Technique (PS : Particle_System) return
     Billboard_Technique.Technique is
   begin
      return PS.Billboard_Method;
   end Get_Billboard_Technique;

   --  -------------------------------------------------------------------------

   function Get_Update_Technique (PS : Particle_System) return PS_Update_Technique.Update_Technique is
   begin
      return PS.Update_Method;
   end Get_Update_Technique;

   --  -------------------------------------------------------------------------

   procedure Init_Particle_System (PS  : in out Particle_System;
                                   Pos : Singles.Vector3) is
      use GL.Objects.Buffers;
      Particles : Particle_Array (1 .. Max_Particles);
   begin
      Particles (1).Particle_Kind := Particle_Launcher;
      Particles (1).Position := Pos;
      Particles (1).Velocity := (0.0, 0.0001, 0.0);
      Particles (1).Lifetime := 0.0;

      for index in UInt range 1 .. 2 loop
         PS.Feedback_Buffer (index).Initialize_Id;
         Transform_Feedback_Buffer.Bind (PS.Feedback_Buffer (index));

         PS.Particle_Buffer (index).Initialize_Id;
         Array_Buffer.Bind (PS.Particle_Buffer (index));

         Load_Particle_Buffer (Array_Buffer, Particles, Dynamic_Draw);
         Transform_Feedback_Buffer.Bind_Buffer_Base
           (0, PS.Particle_Buffer (index));
      end loop;

      PS_Update_Technique.Init (PS.Update_Method);
      PS_Update_Technique.Use_Program (PS.Update_Method);
      PS_Update_Technique.Set_Random_Texture_Unit
        (PS.Update_Method, Ogldev_Engine_Common.Random_Texture_Unit);
      PS_Update_Technique.Set_Launcher_Lifetime (PS.Update_Method, 100.0);
      PS_Update_Technique.Set_Shell_Lifetime (PS.Update_Method, 10000.0);
      PS_Update_Technique.Set_Secondary_Shell_Lifetime (PS.Update_Method, 2500.0);

      Random_Texture.Init_Random_Texture (PS.Random_Texture, 1000);
      Random_Texture.Bind (PS.Random_Texture,
                           Ogldev_Engine_Common.Random_Texture_Unit);

      Billboard_Technique.Init (PS.Billboard_Method);
      Billboard_Technique.Use_Program (PS.Billboard_Method);
      Billboard_Technique.Set_Colour_Texture_Unit
        (PS.Billboard_Method, Ogldev_Engine_Common.Colour_Texture_Unit);
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

   procedure Render (PS         : in out Particle_System; Delta_Time : GL.Types.Int;
                     View_Point : Singles.Matrix4; Camera_Pos : Singles.Vector3) is

   begin
      PS.PS_Time := PS.PS_Time + Delta_Time;
      Update_Particles (PS, Delta_Time);
      Render_Particles (PS, View_Point, Camera_Pos);

      PS.Current_VB_Index := PS.Current_TFB_Index;
      if PS.Current_TFB_Index = 1 then
         PS.Current_TFB_Index := 2;
      else
         PS.Current_TFB_Index := 1;
      end if;

   exception
      when  others =>
         Put_Line ("An exception occurred in Particle_System.Render.");
         raise;
   end Render;

   --  -------------------------------------------------------------------------

   procedure Render_Particles (PS         : in out Particle_System;
                               View_Point : Singles.Matrix4;
                               Camera_Pos : Singles.Vector3) is
      Billboard_Program : constant GL.Objects.Programs.Program :=
                            Billboard_Technique.Billboard_Program (PS.Billboard_Method);
      Update_Program    : constant GL.Objects.Programs.Program :=
                            PS_Update_Technique.Update_Program (PS.Update_Method);
      TFB_Index         : constant UInt := PS.Current_TFB_Index;
      Buffer_Size       : constant Integer := 100;
      Name_Length       : GL.Types.Size;
      Vertices_Length   : GL.Types.Size;
      V_Type            : GL.Objects.Programs.Buffer_Mode;
      Varyings_Name     : String (1 .. Buffer_Size);
   begin
      Billboard_Technique.Use_Program (PS.Billboard_Method);
      Billboard_Technique.Set_Camera_Position (PS.Billboard_Method, Camera_Pos);
      Billboard_Technique.Set_View_Point (PS.Billboard_Method, View_Point);
      Billboard_Technique.Set_Colour_Texture_Unit
        (PS.Billboard_Method, Ogldev_Engine_Common.Colour_Texture_Unit);
--        GL.Toggles.Disable (GL.Toggles.Rasterizer_Discard);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => 3,
                                               Kind   => Single_Type,
                                               Stride => Particle'Size,
                                               Offset => 1);
      GL.Objects.Buffers.Transform_Feedback_Buffer.Bind
        (PS.Feedback_Buffer (TFB_Index));

      GL.Objects.Programs.Get_Transform_Feedback_Varying
        (Object   => Update_Program,
         Index    => 0,
         Length   => Name_Length,
         V_Length => Vertices_Length,
         V_Type   => V_Type,
         Name     => Varyings_Name);

      GL.Objects.Vertex_Arrays.Draw_Arrays
        (Mode  => Points, First => 0,
         Count => GL.Types.Size (Vertices_Length));

      --  Draw_Transform_Feedback is equivalent to calling Draw_Arrays
      --  with mode as specified, first set to zero, and
      --  count set to the number of vertices captured on vertex stream zero the last time transform feedback was active on the transform feedback object named by id.
      --  Draw_Transform_Feedback generates GL_INVALID_VALUE if
      --  the buffer id is not the name of a transform feedback object.
      --        GL.Objects.Buffers.Draw_Transform_Feedback
      --            (Points, PS.Feedback_Buffer (TFB_Index));
--        GL.Objects.Programs.End_Transform_Feedback;
      --          Put_Line ("Particle_System.Render_Particles Draw_Arrays returned.");
      GL.Attributes.Disable_Vertex_Attrib_Array (0);

   exception
      when  others =>
         Put_Line ("An exception occurred in Particle_System.Render_Particles.");
         raise;
   end Render_Particles;

   --  -------------------------------------------------------------------------

   procedure Update_Particles (PS         : in out Particle_System;
                               Delta_Time : GL.Types.Int) is
      use PS_Update_Technique;
      Update_Technique : constant PS_Update_Technique.Update_Technique :=
                           PS.Update_Method;
      Update_Program   : constant GL.Objects.Programs.Program :=
                           PS_Update_Technique.Get_Update_Program (Update_Technique);
      TFB_Index        : constant UInt := PS.Current_TFB_Index;
      VB_Index         : constant UInt := PS.Current_VB_Index;
      Buffer_Size      : constant Integer := 100;
      Name_Length      : GL.Types.Size;
      Vertices_Length  : GL.Types.Size;
      V_Type           : GL.Objects.Programs.Buffer_Mode;
      Varyings_Name    : String (1 .. Buffer_Size);
   begin
      GL.Objects.Programs.Use_Program (Update_Program);
      Set_Time (Update_Technique, PS.PS_Time);
      Set_Delta_Millisec (Update_Technique, Delta_Time);

      Random_Texture.Bind (PS.Random_Texture,
                           Ogldev_Engine_Common.Random_Texture_Unit);
      GL.Toggles.Enable (GL.Toggles.Rasterizer_Discard);

      GL.Objects.Buffers.Array_Buffer.Bind (PS.Particle_Buffer (VB_Index));
      GL.Objects.Buffers.Transform_Feedback_Buffer.Bind
        (PS.Feedback_Buffer (TFB_Index));

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Enable_Vertex_Attrib_Array (1);
      GL.Attributes.Enable_Vertex_Attrib_Array (2);
      GL.Attributes.Enable_Vertex_Attrib_Array (3);

      GL.Attributes.Set_Vertex_Attrib_Pointer
        (Index  => 0, Count => 1, Kind => Single_Type, Stride => 8, Offset => 0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, 8, 2);
      GL.Attributes.Set_Vertex_Attrib_Pointer (2, 3, Single_Type, 8, 5);
      GL.Attributes.Set_Vertex_Attrib_Pointer (3, 1, Single_Type, 8, 8);

      GL.Objects.Programs.Begin_Transform_Feedback (Points);
      GL.Objects.Programs.Get_Transform_Feedback_Varying
        (Object   => Update_Program,
         Index    => 1,
         Length   => Name_Length,
         V_Length => Vertices_Length,
         V_Type   => V_Type,
         Name     => Varyings_Name);

      if PS.Is_First then
         GL.Objects.Vertex_Arrays.Draw_Arrays (GL.Types.Points, 0, 1);
         PS.Is_First := False;
      else
         GL.Objects.Vertex_Arrays.Draw_Arrays
           (Mode  => Points, First => 0, Count => GL.Types.Size (Vertices_Length));
         --           GL.Objects.Buffers.Draw_Transform_Feedback
         --             (Points, PS.Feedback_Buffer (VB_Index));
         --              Put_Line ("Particle_System.Update_Particles Draw_Arrays returned.");
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
