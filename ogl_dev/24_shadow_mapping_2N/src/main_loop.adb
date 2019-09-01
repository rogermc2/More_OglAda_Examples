
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects;
with GL.Objects.Framebuffers;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types.Colors;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Utilities;

with Ogldev_Lights_Common;
with Ogldev_Camera;
with Ogldev_Lights_Common;
with Ogldev_Math;
with Ogldev_Pipeline;
with Ogldev_Shadow_Map_FBO;

with Shadow_Map_Technique;
with Meshes_24N;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
   use GL.Types;

   Background             : constant GL.Types.Colors.Color := (0.5, 0.5, 0.5, 0.0);

   VAO                    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Shadow_Technique       : Shadow_Map_Technique.Technique;
   theShadow_Map          : Ogldev_Shadow_Map_FBO.Shadow_Map_FBO;
   Game_Camera            : Ogldev_Camera.Camera;
   Shadow_Mesh            : Meshes_24N.Mesh_24;
   Quad_Mesh              : Meshes_24N.Mesh_24;
   Perspective_Proj_Info  : Ogldev_Math.Perspective_Projection_Info;
   Spot                   : Ogldev_Lights_Common.Spot_Light;
   Scale                  : Single := 0.0;

   procedure Render_Pass;
   procedure Shadow_Map_Pass;

   --  ------------------------------------------------------------------------

   procedure Init (Window : in out Glfw.Windows.Window) is
      use Ogldev_Lights_Common;

      Window_Width    : Glfw.Size;
      Window_Height   : Glfw.Size;

      Camera_Position : constant Singles.Vector3 := (3.0, 2.0, 10.0);
      Target          : constant Singles.Vector3 := (0.0, -0.2, 1.0);
      Up              : constant Singles.Vector3 := (0.0, 1.0, 0.0);
   begin
      VAO.Initialize_Id;
      VAO.Bind;

      GL.Toggles.Enable (GL.Toggles.Depth_Test);

      Set_Ambient_Intensity (Spot, 0.0);
      Set_Diffuse_Intensity (Spot, 0.9);
      Set_Spot_Light (Spot, (0.0, 10.0, 30.0), Colour_White);
      Set_Direction (Spot, (1.0, -1.0, 1.0));
      Set_Linear_Attenuation (Spot, 0.01);
      Set_Cut_Off (Spot, 20.0);

      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      Utilities.Clear_Background_Colour_And_Depth (Background);

      Ogldev_Shadow_Map_FBO.Init
        (theShadow_Map, Int (Window_Width), Int (Window_Height));
      Ogldev_Math.Set_Perspective_Info
        (Perspective_Proj_Info, 60.0, UInt (Window_Width), UInt (Window_Height),
         1.0, 50.0);
      Ogldev_Camera.Init_Camera (Game_Camera, Window, Camera_Position, Target, Up);
      Shadow_Map_Technique.Init (Shadow_Technique);

      Meshes_24N.Load_Mesh (Quad_Mesh, "../Content/quad.obj");
      Meshes_24N.Load_Mesh (Shadow_Mesh, "../Content/phoenix_ugv.md2");

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Init.");
         raise;
   end Init;

   --  ------------------------------------------------------------------------
   --  A light point of view WVP matrix is used in the first pass
   --  A camera point of view WVP matrix is used in the second pass.
   --  In the first pass the Z buffer will be populated by the closest
   --  Z values from the light point of view and on the second pass
   --  from the camera point of view.
   procedure Render (Window : in out Glfw.Windows.Window) is
   begin
      Ogldev_Camera.Update_Camera (Game_Camera, Window);
      Scale := Scale + 0.05;

      Shadow_Map_Technique.Use_Program (Shadow_Technique);
      --  First, render the closest depth values into the
      --  application created depth buffer
      Shadow_Map_Pass;
      Render_Pass;

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   --  To get texture mapping working:
   --  1. load a texture into OpenGL,
   --  2. supply texture coordinates with the vertices (to map the texture to them),
   --  3. perform a sampling operation from the texture using the texture
   --     coordinates to get the pixel color.
   --  Texturing involves manipulating the connections between four concepts:
   --  1. the texture object which contains the data of the texture image (the texels),
   --     the texture object contains the texture data and the
   --     parameters that configure the sampling operation.
   --  2. a texture unit to which the texture object is bound,
   --  3. the sampler object (in the fragment shader),
   --  4. and the sampler uniform in the shader.

   --  Only the default framebuffer can be used to display something on the screen.
   --  The framebuffers created by the application can only be used for "offscreen rendering".
   procedure Render_Pass is
      use GL.Types.Singles;
      use Ogldev_Camera;
      use Ogldev_Pipeline;
      Pipe : Ogldev_Pipeline.Pipeline;
   begin
      Utilities.Clear_Colour_Buffer_And_Depth;
      Ogldev_Shadow_Map_FBO.Bind_For_Reading (theShadow_Map, 0);

      Set_Scale (Pipe, 10.0);
      Set_World_Position (Pipe, 0.0, 0.0, -1.0);
      Set_Rotation (Pipe, -90.0, 0.0, 0.0);
      Set_Camera (Pipe, Get_Position (Game_Camera),
                                  Get_Target (Game_Camera), Get_Up (Game_Camera));
--        Set_Camera (Pipe, Ogldev_Lights_Common.Position (Spot),
--                                    Ogldev_Lights_Common.Direction (Spot),  (0.0, 1.0, 0.0));
      Set_Perspective_Projection (Pipe, Perspective_Proj_Info);
      Init_Transforms (Pipe);

      Shadow_Map_Technique.Set_WVP (Shadow_Technique,
                                    Ogldev_Pipeline.Get_WVP_Transform (Pipe));
      Meshes_24N.Render (Quad_Mesh);

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Render_Pass.");
         raise;
   end Render_Pass;

   --  ------------------------------------------------------------------------

   procedure Shadow_Map_Pass is
      use GL.Types.Singles;
      use Ogldev_Lights_Common;
      use  Ogldev_Pipeline;
      Pipe : Ogldev_Pipeline.Pipeline;
   begin
       --  Bind the Shadow_Map frame buffer (FBO) to the Draw_Target
      Ogldev_Shadow_Map_FBO.Bind_For_Writing (theShadow_Map);
      Utilities.Clear_Depth;

      Set_Scale (Pipe, 0.1);  --  0.1
      Set_Rotation (Pipe, 0.0, Scale, 0.0);
      Set_World_Position (Pipe, 0.0, 0.0, -3.0);
      Set_Camera (Pipe, Position (Spot),
                  Direction (Spot), (0.0, 1.0, 0.0));
      Set_Perspective_Projection (Pipe, Perspective_Proj_Info);
      Init_Transforms (Pipe);

      Shadow_Map_Technique.Set_WVP (Shadow_Technique, Get_WVP_Transform (Pipe));
      Meshes_24N.Render (Shadow_Mesh);

      GL.Objects.Framebuffers.Draw_Target.Bind
        (GL.Objects.Framebuffers.Default_Framebuffer);

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Shadow_Map_Pass.");
         raise;
   end Shadow_Map_Pass;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   Init (Main_Window);
   while Running loop
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Render (Main_Window);
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
