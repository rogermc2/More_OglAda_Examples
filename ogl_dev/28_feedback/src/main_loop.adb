
with Ada.Text_IO; use Ada.Text_IO;

with GL.Low_Level.Enums;
with GL.Objects;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;

with Ogldev_Basic_Lighting;
with Ogldev_Camera;
with Ogldev_Engine_Common;
with Ogldev_Lights_Common;
with Ogldev_Math;
with Ogldev_Pipeline;
with Ogldev_Texture;

with Meshes_28;
with Particle_System;
with PS_Update_Technique;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   Background             : constant GL.Types.Colors.Color := (0.5, 0.5, 0.5, 0.0);

   VAO                    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Lighting_Technique     : Ogldev_Basic_Lighting.Basic_Lighting_Technique;
   Game_Camera            : Ogldev_Camera.Camera;
   Dir_Light              : Ogldev_Lights_Common.Directional_Light;
   Ground                 : Meshes_28.Mesh_28;
   Bricks                 : Ogldev_Texture.Ogl_Texture;
   Normal_Map             : Ogldev_Texture.Ogl_Texture;
   Perspective_Proj_Info  : Ogldev_Math.Perspective_Projection_Info;
   theParticle_System      : Particle_System.Particle_System;
   Previous_Time_MilliSec : Single;

   --  ------------------------------------------------------------------------

   procedure Init (Window : in out Glfw.Windows.Window; Result : out Boolean) is

      Window_Width        : Glfw.Size;
      Window_Height       : Glfw.Size;

      Position            : constant Singles.Vector3 := (0.0, 0.4, 5.0); --  (0.0, 0.0, 1.0)  Normalized by Camera.Init
      Target              : constant Singles.Vector3 := (0.0, 0.2, 1.0); --  (0.0, 0.0, 1.0)  Normalized by Camera.Init
      Up                  : constant Singles.Vector3 := (0.0, 1.0, 0.0);
      Particle_System_Pos : constant Singles.Vector3 := (0.0, 0.0, 1.0);
   begin
      VAO.Initialize_Id;
      VAO.Bind;
      Previous_Time_MilliSec := Single (Glfw.Time);
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      Utilities.Clear_Background_Colour_And_Depth (Background);
      GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);

      Ogldev_Lights_Common.Init_Directional_Light
        (Light          => Dir_Light,
         Amb_Intensity  => 0.2,
         Diff_Intensity => 0.8,
         theColour      => (1.0, 1.0, 1.0),
         Dir            => (1.0, 0.0, 0.0));

      Ogldev_Math.Set_Perspective_Info
        (Perspective_Proj_Info, 60.0, UInt (Window_Width), UInt (Window_Height),
         1.0, 100.0);
      Ogldev_Camera.Init_Camera (Game_Camera, Window, Position, Target, Up);

      Result := Ogldev_Basic_Lighting.Init (Lighting_Technique);
      if Result then
         GL.Objects.Programs.Use_Program
           (Ogldev_Basic_Lighting.Lighting_Program (Lighting_Technique));
         Ogldev_Basic_Lighting.Set_Directional_Light_Location
           (Lighting_Technique, Dir_Light);
         Ogldev_Basic_Lighting.Set_Color_Texture_Unit_Location
           (Lighting_Technique, UInt (Ogldev_Engine_Common.Colour_Texture_Unit));

         Meshes_28.Load_Mesh (Ground, "src/quad.obj");
         if Ogldev_Texture.Init_Texture (Bricks, GL.Low_Level.Enums.Texture_2D,
                                         "../Content/bricks.jpg") then
            Ogldev_Texture.Load (Bricks);
            Ogldev_Texture.Bind
              (Bricks, Ogldev_Engine_Common.Colour_Texture_Unit);

            if Ogldev_Texture.Init_Texture (Normal_Map, GL.Low_Level.Enums.Texture_2D,
                                            "../Content/normal_map.jpg") then
               Ogldev_Texture.Load (Normal_Map);

               Particle_System.Init_Particle_System
                 (theParticle_System, Particle_System_Pos);
            else
               Put_Line ("Main_Loop.Init, normal_map.jpg failed to load.");
            end if;
         else
            Put_Line ("Main_Loop.Init, bricks.jpg failed to load.");
         end if;
      else
         Put_Line ("Main_Loop.Init, Ogldev_Basic_Lighting failed to initialize.");
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Init.");
         raise;
   end Init;

   --  ------------------------------------------------------------------------

   procedure Render_Scene (Window : in out Glfw.Windows.Window) is
      use GL.Types.Singles;
      use Ogldev_Camera;
      Window_Width     : Glfw.Size;
      Window_Height    : Glfw.Size;
      Pipe             : Ogldev_Pipeline.Pipeline;
      Time_Millisec    : constant Single := 1000.0 * Single (Glfw.Time);
      Delta_Millisec   : UInt;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));

      Delta_Millisec := UInt (Time_Millisec - Previous_Time_MilliSec);
      Previous_Time_MilliSec := Time_Millisec;

      Ogldev_Camera.Update_Camera (Game_Camera, Window);
      Utilities.Clear_Colour_Buffer_And_Depth;

      GL.Objects.Programs.Use_Program
        (Ogldev_Basic_Lighting.Lighting_Program (Lighting_Technique));

      Ogldev_Texture.Bind (Bricks, Ogldev_Engine_Common.Colour_Texture_Unit);
      Ogldev_Texture.Bind (Normal_Map, Ogldev_Engine_Common.Normal_Texture_Unit);

      Ogldev_Pipeline.Set_Scale (Pipe, 20.0, 20.0, 1.0);
      Ogldev_Pipeline.Set_Rotation (Pipe, 90.0, 0.0, 0.0);
      Ogldev_Pipeline.Set_Camera (Pipe, Get_Position (Game_Camera),
                                  Get_Target (Game_Camera), Get_Up (Game_Camera));
      Ogldev_Pipeline.Set_Perspective_Projection (Pipe, Perspective_Proj_Info);
      Ogldev_Pipeline.Init_Transforms (Pipe);

      Ogldev_Basic_Lighting.Set_WVP_Location (Lighting_Technique,
                                              Ogldev_Pipeline.Get_WVP_Transform (Pipe));
      Ogldev_Basic_Lighting.Set_World_Matrix_Location
        (Lighting_Technique, Ogldev_Pipeline.Get_World_Transform (Pipe));

--        Meshes_28.Render (Ground);
      Particle_System.Render (theParticle_System, Delta_Millisec,
                              Ogldev_Pipeline.Get_VP_Transform (Pipe),
                              Get_Position (Game_Camera));
   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Render_Scene.");
         raise;
   end Render_Scene;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean;
begin
   Init (Main_Window, Running);
   while Running loop
      Render_Scene (Main_Window);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Delay(0.5);
      Glfw.Input.Poll_Events;
      Running := Running and not
        (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
   end loop;

exception
   when others =>
      Put_Line ("An exception occurred in Main_Loop.");
      raise;
end Main_Loop;
