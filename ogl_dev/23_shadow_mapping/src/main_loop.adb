
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Low_Level.Enums;
with GL.Objects;
with GL.Objects.Framebuffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
WITH GL.Objects.Textures.Targets;
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

   Background             : constant GL.Types.Colors.Color := (0.0, 0.6, 0.0, 0.0);

   VAO                    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Shadow_Technique       : Shadow_Map_Technique.Technique;
   theShadow_Map          : Shadow_Map_FBO.Shadow_Map;
   Draw_Buffer_List       : GL.Buffers.Explicit_Color_Buffer_List (1 .. 1);
   Game_Camera            : Ogldev_Camera.Camera;
   Shadow_Mesh            : Meshes_23.Mesh_23;
   Quad_Mesh              : Meshes_23.Mesh_23;
   Perspective_Proj_Info  : Ogldev_Math.Perspective_Projection_Info;
   Spot                   : Ogldev_Lights_Common.Spot_Light;
   Scale                  : Single := 0.0;

   procedure Render_Pass(Window : in out Glfw.Windows.Window);
   procedure Shadow_Map_Pass(Window : in out Glfw.Windows.Window);

   --  ------------------------------------------------------------------------

   procedure Init (Window : in out Glfw.Windows.Window) is
      use Ogldev_Lights_Common;

      Window_Width    : Glfw.Size;
      Window_Height   : Glfw.Size;

      Camera_Position : constant Singles.Vector3 := (0.0, 0.0, 0.0);
      Target          : constant Singles.Vector3 := (0.0, 0.0, -1.0);
      Up              : constant Singles.Vector3 := (0.0, 1.0, 0.0);
   begin
      VAO.Initialize_Id;
      VAO.Bind;

      Set_Diffuse_Intensity (Spot, 0.9);
      Set_Ambient_Intensity (Spot, 0.0);
      Set_Spot_Light (Spot, (-20.0, 20.0, 5.0), Colour_Cyan);
      Set_Direction (Spot, (1.0, -1.0, 0.0));
      Set_Linear_Attenuation (Spot, 0.01);
      Set_Cut_Off (Spot, 20.0);

      Shadow_Map_Technique.Init (Shadow_Technique);
      GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);

      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
--        GL.Window.Set_Viewport (10, 10, GL.Types.Int (Window_Width) - 10,
--                                GL.Types.Int (Window_Height) - 10);
      Shadow_Map_FBO.Init
        (theShadow_Map, Int (Window_Width), Int (Window_Height), Draw_Buffer_List);
      Ogldev_Math.Set_Perspective_Info
        (Perspective_Proj_Info, 60.0, UInt (Window_Width), UInt (Window_Height),
         0.1, 1000.0);  --  1.0, 50.0);
      Ogldev_Camera.Init_Camera (Game_Camera,
                                 Int (Window_Width), Int (Window_Height),
                                 Camera_Position, Target, Up);
      Shadow_Map_Technique.Use_Program (Shadow_Technique);

      Meshes_23.Load_Mesh (Quad_Mesh, "../Content/quad.obj");
      Meshes_23.Load_Mesh (Shadow_Mesh, "../Content/phoenix_ugv.md2");

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
      use GL.Types.Singles;
      use Ogldev_Camera;

      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
   begin
      Scale := Scale + 0.05;
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width) - 10,
                              GL.Types.Int (Window_Height) - 10);

      Ogldev_Camera.Update_Camera (Game_Camera, Window);

--        Shadow_Map_Technique.Use_Program (Shadow_Technique);
      --  First, render the closest depth values into the
      --  application created depth buffer
      Shadow_Map_Pass (Window);
      Render_Pass (Window);

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
   procedure Render_Pass (Window : in out Glfw.Windows.Window) is
      use GL.Types.Singles;
      use Ogldev_Camera;
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
      Pipe          : Ogldev_Pipeline.Pipeline;
   begin
      Window.Get_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, Int (Window_Width), Int (Window_Height));
--        Utilities.Clear_Colour_Buffer_And_Depth;

      Utilities.Clear_Background_Colour_And_Depth (Ogldev_Lights_Common.Colour_White);
      Shadow_Map_FBO.Bind_For_Reading (theShadow_Map, 0);
      Put ("Main_Loop.Render_Pass, Width, Height: ");
      Put_Line (Int'Image (GL.Objects.Textures.Targets.Texture_2D.Width (0)) & "  " &
                Int'Image (GL.Objects.Textures.Targets.Texture_2D.Height (0)));

      Ogldev_Pipeline.Set_Scale (Pipe, 5.0);
      Ogldev_Pipeline.Set_World_Position (Pipe, 0.0, 0.0,10.0);
      Ogldev_Pipeline.Set_Camera (Pipe, Get_Position (Game_Camera),
                                  Get_Target (Game_Camera), Get_Up (Game_Camera));
      Ogldev_Pipeline.Set_Perspective_Info (Pipe, Perspective_Proj_Info);
      Ogldev_Pipeline.Init_Transforms (Pipe);

      Shadow_Map_Technique.Set_WVP (Shadow_Technique,
                                    Ogldev_Pipeline.Get_WVP_Transform (Pipe));

--        Utilities.Print_Matrix ("Main_Loop.Render_Pass WVP_Transform",
--                                      Ogldev_Pipeline.Get_WVP_Transform (Pipe));

      Utilities.Clear_Background_Colour_And_Depth (Ogldev_Lights_Common.Colour_Red);
      Meshes_23.Render (Quad_Mesh);
--        GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, 1);
      New_Line;

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Render_Pass.");
         raise;
   end Render_Pass;

   --  ------------------------------------------------------------------------

   procedure Shadow_Map_Pass (Window : in out Glfw.Windows.Window) is
      use GL.Types.Singles;
      use Ogldev_Camera;
      use Ogldev_Lights_Common;
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
      Pipe          : Ogldev_Pipeline.Pipeline;
   begin
      Window.Get_Size (Window_Width, Window_Height);

      Ogldev_Pipeline.Set_Scale (Pipe, 0.001);  --  0.1
      Ogldev_Pipeline.Set_Rotation (Pipe, 0.0, Scale, 0.0);
      Ogldev_Pipeline.Set_World_Position (Pipe, 0.0, 0.0, 15.0);
      Ogldev_Pipeline.Set_Camera (Pipe, Position (Spot),
                                  Direction (Spot), (0.0, 1.0, 0.0));
      Ogldev_Pipeline.Set_Perspective_Info (Pipe, Perspective_Proj_Info);
      Ogldev_Pipeline.Init_Transforms (Pipe);

      --  Bind the Shadow_Map frame buffer (FBO) to the Draw_Target
      Shadow_Map_FBO.Bind_For_Writing (theShadow_Map);
      Put ("Main_Loop.Shadow_Map_Pass, Width, Height: ");
      Put_Line (Int'Image (GL.Objects.Textures.Targets.Texture_2D.Width (0)) & "  " &
                Int'Image (GL.Objects.Textures.Targets.Texture_2D.Height (0)));
      GL.Buffers.Set_Active_Buffers (Draw_Buffer_List);
--        GL.Window.Set_Viewport (0, 0, Int (Window_Width), Int (Window_Height));
--        Utilities.Clear_Depth;
--        Utilities.Clear_Colour_Buffer_And_Depth;
      Utilities.Clear_Background_Colour_And_Depth (Colour_Blue);

      GL.Buffers.Clear_Stencil_Buffer (0);
      GL.Buffers.Set_Stencil_Clear_Value (1);

      Shadow_Map_Technique.Use_Program (Shadow_Technique);
      Shadow_Map_Technique.Set_WVP (Shadow_Technique,
                                    Ogldev_Pipeline.Get_WVP_Transform (Pipe));
--         Utilities.Print_Matrix ("Main_Loop.Shadow_Map_Pass WVP_Transform",
--                                      Ogldev_Pipeline.Get_WVP_Transform (Pipe));

      Meshes_23.Render (Shadow_Mesh);
--        GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, 1);

      GL.Objects.Framebuffers.Draw_Target.Bind
        (GL.Objects.Framebuffers.Default_Framebuffer);
      New_Line;

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
--        Delay (2.0);
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
