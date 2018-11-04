
with Ada.Text_IO; use Ada.Text_IO;

with GL.Low_Level.Enums;
with GL.Objects;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Types.Colors;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;

with Ogldev_Basic_Lighting;
--  with Ogldev_Basic_Mesh;
with Ogldev_Camera;
with Ogldev_Lights_Common;
with Ogldev_Math;
with Ogldev_Pipeline;
with Ogldev_Texture;

with Particle_System;
with PS_Update_Technique;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   Background                  : constant GL.Types.Colors.Color := (0.0, 1.0, 0.0, 0.0);

   Buildboard_Program          : GL.Objects.Programs.Program;
   Update_Program              : GL.Objects.Programs.Program;

   theLighting_Technique       : Ogldev_Basic_Lighting.Basic_Lighting_Technique;
   Game_Camera                 : Ogldev_Camera.Camera;
   Dir_Light                   : Ogldev_Lights_Common.Directional_Light;
--     Ground                      : Ogldev_Basic_Mesh.Basic_Mesh;
   theTexture                  : Ogldev_Texture.Ogl_Texture;
   Normal_Map                  : Ogldev_Texture.Ogl_Texture;
   Perspective_Proj_Info       : Ogldev_Math.Perspective_Projection_Info;
   theUpdate_Technique         : PS_Update_Technique.Update_Technique;
   theParticle_System          : Particle_System.Particle_System;
   Current_Time_MilliSec       : Single;

   --  ------------------------------------------------------------------------

   function Build_Shader_Programs return Boolean is
      use GL.Objects.Shaders;
      use Program_Loader;
      OK : Boolean := False;
   begin
      --  Lighting shaders are built by Ogldev_Basic_Lighting.Init
      Buildboard_Program := Program_From
        ((Src ("src/shaders/billboard.vs", Vertex_Shader),
         Src ("src/shaders/billboard.fs", Fragment_Shader),
         Src ("src/shaders/billboard.gs", Geometry_Shader)));
      OK := GL.Objects.Programs.Link_Status (Buildboard_Program);

      Update_Program := Program_From
        ((Src ("src/shaders/ps_update.vs", Vertex_Shader),
         Src ("src/shaders/ps_update.fs", Fragment_Shader),
         Src ("src/shaders/ps_update.gs", Geometry_Shader)));

      OK := OK and GL.Objects.Programs.Link_Status (Update_Program);
      if not GL.Objects.Programs.Link_Status (Update_Program) then
         Put_Line ("Build_Shader_Programs, Update_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (Update_Program));
      else
         Put_Line ("Build_Shader_Programs, Update_Program Link ok");
      end if;
      return OK;

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Build_Shader_Programs.");
         raise;
   end Build_Shader_Programs;

   --  ------------------------------------------------------------------------

   procedure Init (Window : in out Glfw.Windows.Window; Result : out Boolean) is

      Window_Width        : Glfw.Size;
      Window_Height       : Glfw.Size;
      Position            : constant Singles.Vector3 := (0.0, 0.4, 0.5);
      Target              : constant Singles.Vector3 := (0.0, 2.0, 1.5);
      Up                  : constant Singles.Vector3 := (0.0, 1.0, 0.0);
      Particle_System_Pos : constant GL.Types.Singles.Vector3 := (0.0, 0.0, -1.0);
   begin
      Result := Build_Shader_Programs;
      if Result then
         Current_Time_MilliSec := Single (Glfw.Time);
         Window.Get_Framebuffer_Size (Window_Width, Window_Height);
         Ogldev_Camera.Init_Camera (Game_Camera,
                                    Int (Window_Width), Int (Window_Height),
                                    Position, Target, Up);
         Result := Ogldev_Basic_Lighting.Init (theLighting_Technique);
         Ogldev_Basic_Lighting.Set_Directional_Light (theLighting_Technique, Dir_Light);
         Ogldev_Basic_Lighting.Set_Color_Texture_Unit
           (theLighting_Technique, 0);

         Put_Line (" Main_Loop.Init, Color_Texture_Unit set.");

         --  Ogldev_Basic_Mesh.Load_Mesh fails due to GNAT bug
--           Ogldev_Basic_Mesh.Load_Mesh (Ground, "src/quad.obj");
--           Put_Line (" Main_Loop.Init, Ground loaded.");
         if  Ogldev_Texture.Init_Texture (theTexture, GL.Low_Level.Enums.Texture_2D,
                                      "../Content/bricks.jpg") then
                Ogldev_Texture.Load (theTexture);
                Ogldev_Texture.Bind (theTexture, 0);

                if Ogldev_Texture.Init_Texture (Normal_Map, GL.Low_Level.Enums.Texture_2D,
                                             "../Content/normal_map.jpg") then
                Ogldev_Texture.Load (Normal_Map);

                Particle_System.Init_Particle_System
                  (theParticle_System, theUpdate_Technique, Update_Program,
                   Particle_System_Pos);
                end if;
            end if;
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

      Ogldev_Texture.Bind (theTexture, 0);

      Ogldev_Pipeline.Set_Scale (Pipe, 20.0, 20.0, 1.0);
      Ogldev_Pipeline.Set_Rotation (Pipe, 90.0, 0.0, 0.0);
      Ogldev_Pipeline.Set_Camera (Pipe, Get_Position (Game_Camera),
                                  Get_Target (Game_Camera), Get_Up (Game_Camera));
      Ogldev_Pipeline.Set_Perspective_Info (Pipe, Perspective_Proj_Info);

      Ogldev_Basic_Lighting.Set_WVP (theLighting_Technique,
                                     Ogldev_Pipeline.Get_WVP_Transform (Pipe));

      Ogldev_Basic_Lighting.Set_World_Matrix (theLighting_Technique,
                               Ogldev_Pipeline.Get_World_Transform (Pipe));
--        Ogldev_Basic_Mesh.Render (Ground);
      Particle_System.Render (theParticle_System, Int (Delta_Millisec),
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
      --           delay (0.03);
      Render_Scene (Main_Window);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      delay (0.03);
      Running := Running and not
        (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
   end loop;

exception
   when others =>
      Put_Line ("An exception occurred in Main_Loop.");
      raise;
end Main_Loop;
