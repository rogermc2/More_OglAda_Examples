
with Ada.Text_IO; use Ada.Text_IO;

with GL.Low_Level.Enums;
with GL.Objects;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
--  with GL.Toggles;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;

with Ogldev_Camera;
with Ogldev_Engine_Common;
with Ogldev_Math;
with Ogldev_Pipeline;
with Ogldev_Texture;

with Lighting_Technique_26;
with Meshes_26;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   Background                  : constant GL.Types.Colors.Color := (0.7, 0.7, 0.7, 0.0);

   VAO                         : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   theLighting_Technique       : Lighting_Technique_26.Technique;
   Game_Camera                 : Ogldev_Camera.Camera;
   Dir_Light                   : Lighting_Technique_26.Direct_Light;
   Sphere_Mesh                 : Meshes_26.Mesh_26;
   theTexture                  : Ogldev_Texture.Ogl_Texture;
   Normal_Map                  : Ogldev_Texture.Ogl_Texture;
   Trivial_Normal_Map          : Ogldev_Texture.Ogl_Texture;
   Perspective_Proj_Info       : Ogldev_Math.Perspective_Projection_Info;
   Current_Time_MilliSec       : Single;
   Scale                       : Single := 0.0;
   Bump_Map_Enabled            : Boolean := True;

   --  ------------------------------------------------------------------------

   procedure Init (Window : in out Glfw.Windows.Window; Result : out Boolean) is

      Window_Width        : Glfw.Size;
      Window_Height       : Glfw.Size;
--        Position            : constant Singles.Vector3 := (0.0, 0.0, 1.0); --  Normalized by Camera.Init
--        Target              : constant Singles.Vector3 := (0.0, 0.0, 1.0);  --  Normalized by Camera.Init

      Position            : constant Singles.Vector3 := (0.5, 1.025, 0.25);
      Target              : constant Singles.Vector3 := (0.0, -0.5, -1.0);
      Up                  : constant Singles.Vector3 := (0.0, 1.0, 0.0);
      Particle_System_Pos : constant GL.Types.Singles.Vector3 := (0.0, 0.0, -1.0);
   begin
      VAO.Initialize_Id;
      VAO.Bind;
      Current_Time_MilliSec := Single (Glfw.Time);
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      Lighting_Technique_26.Set_Directional_Light
        (theLighting_Technique, Dir_Light);
      Ogldev_Math.Set_Perspective_Info
        (Perspective_Proj_Info, 60.0, UInt (Window_Width), UInt (Window_Height),
         1.0, 100.0);
      Ogldev_Camera.Init_Camera (Game_Camera,
                                 Int (Window_Width), Int (Window_Height),
                                 Position, Target, Up);
      Result := Lighting_Technique_26.Init (theLighting_Technique);
      if Result then
         GL.Objects.Programs.Use_Program (Lighting_Technique_26.Light_Program (theLighting_Technique));
         Lighting_Technique_26.Set_Directional_Light (theLighting_Technique, Dir_Light);
         Lighting_Technique_26.Set_Colour_Texture_Unit
           (theLighting_Technique, Ogldev_Engine_Common.Colour_Texture_Unit);
         Lighting_Technique_26.Set_Normal_Map_Texture_Unit
           (theLighting_Technique, Ogldev_Engine_Common.Normal_Texture_Unit);

         Meshes_26.Load_Mesh (Sphere_Mesh, "src/box.obj");
         if Ogldev_Texture.Init_Texture (theTexture, GL.Low_Level.Enums.Texture_2D,
                                         "../Content/bricks.jpg") then
            Ogldev_Texture.Load (theTexture);
            Ogldev_Texture.Bind
              (theTexture, Ogldev_Engine_Common.Colour_Texture_Unit);

            if Ogldev_Texture.Init_Texture (Normal_Map, GL.Low_Level.Enums.Texture_2D,
                                            "../Content/normal_map.jpg") then
               Ogldev_Texture.Load (Normal_Map);
               if Ogldev_Texture.Init_Texture (Trivial_Normal_Map, GL.Low_Level.Enums.Texture_2D,
                                            "../Content/normal_up.jpg") then
                   Ogldev_Texture.Load (Trivial_Normal_Map);

                else
                   Put_Line ("Main_Loop.Init, normal_up.jpg failed to load.");
                end if;
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
      Window_Width         : Glfw.Size;
      Window_Height        : Glfw.Size;
      Pipe                 : Ogldev_Pipeline.Pipeline;
      Time_Now_Millisec    : Single;
      Delta_Millisec       : UInt;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));

      Time_Now_Millisec := 1000.0 * Single (Glfw.Time);
      Delta_Millisec := UInt (Time_Now_Millisec - Current_Time_MilliSec);
      Current_Time_MilliSec := Time_Now_Millisec;

      Ogldev_Camera.Update_Camera (Game_Camera, Window);

      Utilities.Clear_Background_Colour_And_Depth (Background);
--        GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);

      Lighting_Technique_26.Use_Program (theLighting_Technique);

      Ogldev_Texture.Bind (theTexture, Ogldev_Engine_Common.Colour_Texture_Unit);
      Ogldev_Texture.Bind (Normal_Map, Ogldev_Engine_Common.Normal_Texture_Unit);

      Ogldev_Pipeline.Set_Scale (Pipe, 0.5, 0.5, 1.0);
--        Ogldev_Pipeline.Set_Scale (Pipe, 20.0, 20.0, 1.0);
      Ogldev_Pipeline.Set_Rotation (Pipe, 90.0, 0.0, 0.0);
      Ogldev_Pipeline.Set_Rotation (Pipe, 45.0, 0.0, 0.0);
      Ogldev_Pipeline.Set_Camera (Pipe, Get_Position (Game_Camera),
                                  Get_Target (Game_Camera), Get_Up (Game_Camera));
      Ogldev_Pipeline.Set_Perspective_Info (Pipe, Perspective_Proj_Info);
      Ogldev_Pipeline.Init_Transforms (Pipe);

--        Utilities.Print_Matrix ("Main_Loop.Render_Scene World_Transform",
--                                Ogldev_Pipeline.Get_World_Transform (Pipe));
      Lighting_Technique_26.Set_World_Matrix
        (theLighting_Technique, Ogldev_Pipeline.Get_World_Transform (Pipe));

--        Utilities.Print_Matrix ("Main_Loop.Render_Scene WVP_Transform",
--                                Ogldev_Pipeline.Get_WVP_Transform (Pipe));
      Lighting_Technique_26.Set_WVP (theLighting_Technique,
                                     Ogldev_Pipeline.Get_WVP_Transform (Pipe));

      Meshes_26.Render (Sphere_Mesh);
--        Particle_System.Render (theParticle_System, Int (Delta_Millisec),
--                                Ogldev_Pipeline.Get_VP_Transform (Pipe),
--                                Get_Position (Game_Camera));
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
--        delay (0.2);
      Render_Scene (Main_Window);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
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
