
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Low_Level.Enums;
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

with Ogldev_Camera;
with Ogldev_Shadow_Map_FBO;
with Ogldev_Math;
with Ogldev_Pipeline;
with Ogldev_Texture;

with Lighting_Technique_24;
with Shadow_Map_Technique;
with Meshes_24;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
   use GL.Types;

   Background             : constant GL.Types.Colors.Color := (0.6, 0.6, 0.6, 0.0);

   VAO                    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Lighting_Technique     : Lighting_Technique_24.Technique;
   Shadow_Technique       : Shadow_Map_Technique.Technique;
   theShadow_Map          : Ogldev_Shadow_Map_FBO.Shadow_Map_FBO;
   Game_Camera            : Ogldev_Camera.Camera;
   Shadow_Mesh            : Meshes_24.Mesh_24;
   Quad_Mesh              : Meshes_24.Mesh_24;
   Ground_Texture         : Ogldev_Texture.Ogl_Texture;
   World_Position         : constant GL.Types.Singles.Vector3 := (0.0, 0.0, -3.0);
   Perspective_Proj_Info  : Ogldev_Math.Perspective_Projection_Info;
   Spot_Lights            : Lighting_Technique_24.Spot_Lights_Array (1 .. 1);
   Scale                  : Single := 0.0;

   procedure Render_Pass;
   procedure Shadow_Map_Pass;

   --  ------------------------------------------------------------------------

   procedure Init (Window : in out Glfw.Windows.Window) is
      Window_Width    : Glfw.Size;
      Window_Height   : Glfw.Size;
      Camera_Position : constant Singles.Vector3 := (2.0, 4.0, 10.0);  -- (3.0, 8.0, -10.0);
      Target          : constant Singles.Vector3 := (0.0, -0.2, -1.0);  --  (0.0, -0.2, 1.0)
      Up              : constant Singles.Vector3 := (0.0, 1.0, 0.0);
   begin
      VAO.Initialize_Id;
      VAO.Bind;

      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      GL.Buffers.Set_Depth_Function (Always);

      Lighting_Technique_24.Set_Spot_Light (Light     => Spot_Lights (1),
                                            Ambient   => 0.02,  -- 0.1
                                            Diffuse   => 0.1,
                                            Colour    => (1.0, 1.0, 1.0),
                                            Pos       => (-20.0, 20.0, 1.0),  --  (-20.0, 20.0, -1.0)
                                            Direction => (1.0, -1.0, 0.0),
                                            Atten     =>  (0.0, 0.01, 0.0),
                                            Cut_Off   => 20.0);

      Glfw.Windows.Get_Framebuffer_Size (Window'Access, Window_Width, Window_Height);
      Ogldev_Math.Set_Perspective_Info
        (Perspective_Proj_Info, 60.0, UInt (Window_Width), UInt (Window_Height),
         1.0, 50.0);

      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      Utilities.Clear_Background_Colour_And_Depth (Background);

      Ogldev_Shadow_Map_FBO.Init
        (theShadow_Map, Int (Window_Width), Int (Window_Height));

      Ogldev_Camera.Init_Camera (Game_Camera, Window, Camera_Position, Target, Up);
      Lighting_Technique_24.Init (Lighting_Technique);
      Lighting_Technique_24.Use_Program (Lighting_Technique);
      Lighting_Technique_24.Set_Spot_Light_Locations (Lighting_Technique, Spot_Lights);
      Lighting_Technique_24.Set_Texture_Unit (Lighting_Technique, 0);
      Lighting_Technique_24.Set_Shadow_Texture_Unit (Lighting_Technique, 1);

      Shadow_Map_Technique.Init (Shadow_Technique);

      Meshes_24.Load_Mesh (Quad_Mesh, "../content/quad.obj");
      if Ogldev_Texture.Init_Texture (Ground_Texture, GL.Low_Level.Enums.Texture_2D,
            "../content/test.png") then
         Ogldev_Texture.Load (Ground_Texture);
      else
         Put_Line ("Main_Loop.Init Ground_Texture failed to initialize.");
      end if;
      Meshes_24.Load_Mesh (Shadow_Mesh, "../content/phoenix_ugv.md2");

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

      --  First, render the closest depth values into the
      --  application created depth buffer
      Shadow_Map_Pass;
      GL.Objects.Framebuffers.Draw_Target.Bind
        (GL.Objects.Framebuffers.Default_Framebuffer);
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

      Lighting_Technique_24.Use_Program (Lighting_Technique);
      Lighting_Technique_24.Set_Eye_World_Pos_Location
        (Lighting_Technique, Get_Position (Game_Camera));

      Ogldev_Shadow_Map_FBO.Bind_For_Reading (theShadow_Map, 1);

      Set_Perspective_Projection (Pipe, Perspective_Proj_Info);
      Set_Scale (Pipe, 6.0);  --  10
      Set_World_Position (Pipe, World_Position);
      Set_Rotation (Pipe, -90.0, 0.0, 0.0);
      Set_Camera (Pipe, Get_Position (Game_Camera),
                  Get_Target (Game_Camera), Get_Up (Game_Camera));
      Init_Transforms (Pipe);

      Lighting_Technique_24.Set_WVP_Location (Lighting_Technique,
                                              Get_WVP_Transform (Pipe));
      Lighting_Technique_24.Set_World_Matrix_Location (Lighting_Technique,
                                                       Get_World_Transform (Pipe));

      Set_Camera (Pipe, Lighting_Technique_24.Get_Position (Spot_Lights (1)),
                  Lighting_Technique_24.Get_Direction (Spot_Lights (1)),
                  (0.0, 1.0, 0.0));
      Init_Transforms (Pipe);
      Lighting_Technique_24.Set_Light_WVP_Location (Lighting_Technique,
                                                    Get_WVP_Transform (Pipe));
      Ogldev_Texture.Bind (Ground_Texture, 0);
      Meshes_24.Render (Quad_Mesh);

      Set_Scale (Pipe, 0.05);  --  0.1
      Set_Rotation (Pipe, 0.0, Scale, 0.0);
      Set_World_Position (Pipe, 0.0, 0.0, 3.0);
      Set_Camera (Pipe, Get_Position (Game_Camera),
                  Get_Target (Game_Camera), Get_Up (Game_Camera));
      Init_Transforms (Pipe);
      Lighting_Technique_24.Set_WVP_Location (Lighting_Technique,
                                              Get_WVP_Transform (Pipe));
      Lighting_Technique_24.Set_World_Matrix_Location (Lighting_Technique,
                                                       Get_World_Transform (Pipe));

      Set_Camera (Pipe, Lighting_Technique_24.Get_Position (Spot_Lights (1)),
                  Lighting_Technique_24.Get_Direction (Spot_Lights (1)),
                  (0.0, 1.0, 0.0));
      Init_Transforms (Pipe);
      Lighting_Technique_24.Set_Light_WVP_Location (Lighting_Technique,
                                                    Get_WVP_Transform (Pipe));
      Meshes_24.Render (Shadow_Mesh);

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Render_Pass.");
         raise;
   end Render_Pass;

   --  ------------------------------------------------------------------------

   procedure Shadow_Map_Pass is
      use GL.Types.Singles;
      use Ogldev_Pipeline;
      Pipe : Pipeline;
   begin
       --  Bind the Shadow_Map frame buffer (FBO) to the Draw_Target
      Ogldev_Shadow_Map_FBO.Bind_For_Writing (theShadow_Map);
      Utilities.Clear_Depth;
      Shadow_Map_Technique.Use_Program (Shadow_Technique);

      Set_Scale (Pipe, 1.0);  --  0.1
      Set_Rotation (Pipe, 0.0, Scale, 0.0);
      Set_World_Position (Pipe, World_Position);
      Ogldev_Pipeline.Set_Camera (Pipe,
                  Lighting_Technique_24.Get_Position (Spot_Lights(1)),
                  Lighting_Technique_24.Get_Direction (Spot_Lights(1)),
                  (0.0, 1.0, 0.0));
      Set_Perspective_Projection (Pipe, Perspective_Proj_Info);
      Init_Transforms (Pipe);

      Shadow_Map_Technique.Set_WVP (Shadow_Technique, Get_WVP_Transform (Pipe));
      Meshes_24.Render (Shadow_Mesh);

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