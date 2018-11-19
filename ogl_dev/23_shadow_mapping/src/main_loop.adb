
with Ada.Text_IO; use Ada.Text_IO;

with GL.Low_Level.Enums;
with GL.Objects;
with GL.Objects.Framebuffers;
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

with Ogldev_Lights_Common;
with Ogldev_Camera;
with Ogldev_Engine_Common;
with Ogldev_Lights_Common;
with Ogldev_Math;
with Ogldev_Pipeline;
with Ogldev_Texture;

with Shadow_Map_FBO;
with Shadow_Map_Technique;
with Meshes_23;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
   use GL.Types;

   Background             : constant GL.Types.Colors.Color := (0.7, 0.7, 0.7, 0.0);

   VAO                    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Shadow_Technique       : Shadow_Map_Technique.Technique;
   theShadow_Map          : Shadow_Map_FBO.Shadow_Map;
   Game_Camera            : Ogldev_Camera.Camera;
   Shadow_Mesh            : Meshes_23.Mesh_23;
   Quad_Mesh              : Meshes_23.Mesh_23;
   Perspective_Proj_Info  : Ogldev_Math.Perspective_Projection_Info;
   Spot                   : Ogldev_Lights_Common.Spot_Light;
   Scale                  : Single := 0.0;

   --  ------------------------------------------------------------------------

   procedure Init (Window : in out Glfw.Windows.Window) is
      use Ogldev_Lights_Common;
      Window_Width        : Glfw.Size;
      Window_Height       : Glfw.Size;

      Camera_Position     : constant Singles.Vector3 := (-1.0, 1.0, 0.25);
      Target              : constant Singles.Vector3 := (0.0, -0.5, 1.0);
      Up                  : constant Singles.Vector3 := (0.0, 1.0, 0.0);
   begin
      VAO.Initialize_Id;
      VAO.Bind;

      Utilities.Clear_Background_Colour_And_Depth (Background);
      Set_Diffuse_Intensity (Spot, 0.9);
      Set_Spot_Light (Spot, (-20.0, 20.0, 5.0), Colour_White);
      Set_Direction (Spot, (1.0, -1.0, 0.0));
      Set_Linear_Attenuation (Spot, 0.01);
      Set_Cut_Off (Spot, 20.0);

      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      Shadow_Map_FBO.Init (theShadow_Map, Int (Window_Width), Int (Window_Height));
      Ogldev_Math.Set_Perspective_Info
        (Perspective_Proj_Info, 60.0, UInt (Window_Width), UInt (Window_Height),
         1.0, 50.0);
      Ogldev_Camera.Init_Camera (Game_Camera,
                                 Int (Window_Width), Int (Window_Height),
                                 Camera_Position, Target, Up);
      Shadow_Map_Technique.Init (Shadow_Technique);
      Shadow_Map_Technique.Use_Program (Shadow_Technique);

      Meshes_23.Load_Mesh (Quad_Mesh, "../Content/quad.obj");
      Meshes_23.Load_Mesh (Shadow_Mesh, "../Content/phoenix_ugv.md2");

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Init.");
         raise;
   end Init;

   --  ------------------------------------------------------------------------

   procedure Shadow_Map_Pass is
      use GL.Types.Singles;
      use Ogldev_Camera;
      use Ogldev_Lights_Common;
      Pipe  : Ogldev_Pipeline.Pipeline;
   begin
      Shadow_Map_FBO.Bind_For_Writing (theShadow_Map);
      Utilities.Clear_Depth;

      Ogldev_Pipeline.Set_Scale (Pipe, 0.01);
      Ogldev_Pipeline.Set_Rotation (Pipe, 0.0, Scale, 0.0);
      Ogldev_Pipeline.Set_World_Position (Pipe, 0.0, 0.0, -5.0);
      Ogldev_Pipeline.Set_Camera (Pipe, Position (Spot),
                                  Direction (Spot), (0.0, 1.0, 0.0));
      Ogldev_Pipeline.Set_Perspective_Info (Pipe, Perspective_Proj_Info);
      Ogldev_Pipeline.Init_Transforms (Pipe);
      Shadow_Map_Technique.Set_WVP (Shadow_Technique,
                                    Ogldev_Pipeline.Get_WVP_Transform (Pipe));

      --        Utilities.Print_Matrix ("Main_Loop.Render_Scene WVP_Transform",
      --                                Ogldev_Pipeline.Get_WVP_Transform (Pipe));;

      Meshes_23.Render (Shadow_Mesh);
      GL.Objects.Framebuffers.Draw_Target.Bind (GL.Objects.Framebuffers.Default_Framebuffer);

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Shadow_Map_Pass.");
         raise;
   end Shadow_Map_Pass;

   --  ------------------------------------------------------------------------

   procedure Render_Pass is
      use GL.Types.Singles;
      use Ogldev_Camera;
      Pipe     : Ogldev_Pipeline.Pipeline;
   begin
      Shadow_Map_Technique.Set_Shadow_Map_Texture_Unit (Shadow_Technique, 0);
      Shadow_Map_FBO.Bind_For_Reading (theShadow_Map, 0);

      Ogldev_Pipeline.Set_Scale (Pipe, 5.0);
      Shadow_Map_FBO.Bind_For_Reading (theShadow_Map, 0);
      Ogldev_Pipeline.Set_World_Position (Pipe, 0.0, 0.0, -10.0);
      Ogldev_Pipeline.Set_Camera (Pipe, Get_Position (Game_Camera),
                                  Get_Target (Game_Camera), Get_Up (Game_Camera));
      Ogldev_Pipeline.Set_Perspective_Info (Pipe, Perspective_Proj_Info);
      Ogldev_Pipeline.Init_Transforms (Pipe);

      Shadow_Map_Technique.Set_WVP (Shadow_Technique,
                                    Ogldev_Pipeline.Get_WVP_Transform (Pipe));

      --        Utilities.Print_Matrix ("Main_Loop.Render_Scene WVP_Transform",
      --                                Ogldev_Pipeline.Get_WVP_Transform (Pipe));;

      Meshes_23.Render (Quad_Mesh);

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Render_Pass.");
         raise;
   end Render_Pass;

   --  ------------------------------------------------------------------------

   procedure Render_Scene (Window : in out Glfw.Windows.Window) is
      use GL.Types.Singles;
      use Ogldev_Camera;
      Window_Width         : Glfw.Size;
      Window_Height        : Glfw.Size;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));

      Ogldev_Camera.Update_Camera (Game_Camera, Window);
      Utilities.Clear_Background_Colour_And_Depth (Background);

      --        GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);

      Shadow_Map_Technique.Use_Program (Shadow_Technique);
      Shadow_Map_Pass;
      Render_Pass;

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Render_Scene.");
         raise;
   end Render_Scene;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   Init (Main_Window);
   while Running loop
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
