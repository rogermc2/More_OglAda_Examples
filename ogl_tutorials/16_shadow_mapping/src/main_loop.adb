
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Culling;
with GL.Objects.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows;
with Glfw.Windows.Context;

with Controls;
with Load_DDS;
with Program_Loader;
with Maths;
with Utilities;

with Buffers_Manager;
with Textures_Manager;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Dark_Blue                : constant GL.Types.Colors.Color := (0.0, 0.0, 0.4, 0.0);

   Depth_Program            : GL.Objects.Programs.Program;
   Render_Program           : GL.Objects.Programs.Program;
   Vertices_Array_Object    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Frame_Buffer             : GL.Objects.Framebuffers.Framebuffer;
   Normals_Buffer           : GL.Objects.Buffers.Buffer;
   UVs_Buffer               : GL.Objects.Buffers.Buffer;
   Vertex_Buffer            : GL.Objects.Buffers.Buffer;
   Element_Buffer           : GL.Objects.Buffers.Buffer;
   Indices_Size             : GL.Types.Int;
   Vertex_Count             : GL.Types.Int;
   Depth_Texture            : GL.Objects.Textures.Texture;
   UV_Map                   : GL.Objects.Textures.Texture;
   Depth_Bias_ID            : GL.Uniforms.Uniform;
   Depth_Matrix_ID          : GL.Uniforms.Uniform;
   MVP_Matrix_ID            : GL.Uniforms.Uniform;
   Shadow_Map_ID            : GL.Uniforms.Uniform;
   Texture_ID               : GL.Uniforms.Uniform;
   Model_Matrix             : GL.Types.Singles.Matrix4 :=
                                 GL.Types.Singles.Identity4;

   --  ------------------------------------------------------------------------

   procedure Render (Window : in out Glfw.Windows.Window) is
      use GL.Types;
      use GL.Types.Singles;
      use GL.Objects.Textures.Targets;
      use Maths;
      Window_Width            : Glfw.Size;
      Window_Height           : Glfw.Size;
      Inv_Light_Direction     : constant Vector3 := (0.5, 2.0, -2.0);
      Camera_Position         : constant Vector3 := (-2.0, -1.0, 5.0);
      Up                      : constant Vector3 := (0.0, 1.0, 0.0);
      Depth_Model_Matrix      : constant Matrix4 := Identity4;
      Depth_MVP_Matrix        : Matrix4;
      Depth_Projection_Matrix : Matrix4;
      Depth_View_Matrix       : Matrix4;
      MVP_Matrix              : Matrix4;
      Projection_Matrix       : Matrix4;
      View_Matrix             : Matrix4;
      Bias_Matrix             : constant  Matrix4 := ((0.5, 0.0, 0.0, 0.0),
                                                      (0.0, 0.5, 0.0, 0.0),
                                                      (0.0, 0.0, 0.5, 0.0),
                                                      (0.5, 0.5, 0.5, 1.0));
      Depth_Bias_MVP_Matrix   : Matrix4;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind (Frame_Buffer);
      GL.Window.Set_Viewport (0, 0, 1024, 1024);

      --  Bias is'nt used in the shader. Instead back faces,
      --  which are already separated from the front faces by a small distance,
      --  are drawn (if your geometry is made this way).
      GL.Toggles.Enable (GL.Toggles.Cull_Face);
      GL.Culling.Set_Cull_Face (GL.Culling.Back);

      Utilities.Clear_Background_Colour_And_Depth (Dark_Blue);

      GL.Objects.Programs.Use_Program (Depth_Program);
      Init_Orthographic_Transform
          (-10.0, 10.0, -10.0, 10.0, -10.0, 20.0, Depth_Projection_Matrix);
      Init_Lookat_Transform (Position => Camera_Position,
                             Target   => Inv_Light_Direction,
                             Up       => Up,
                             Look_At  => Depth_View_Matrix);
      Depth_MVP_Matrix := Depth_Projection_Matrix * Depth_View_Matrix * Depth_Model_Matrix;
      GL.Uniforms.Set_Single (Depth_Matrix_ID, Depth_MVP_Matrix);

      --  First attribute buffer : vertices
      GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False, 0, 0);

      GL.Objects.Buffers.Element_Array_Buffer.Bind (Element_Buffer);
      GL.Objects.Buffers.Draw_Elements (Triangles, Indices_Size, UInt_Type, 0);

      GL.Attributes.Disable_Vertex_Attrib_Array (0);

      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind (GL.Objects.Framebuffers.Default_Framebuffer);

      GL.Window.Set_Viewport (0, 0, Int (Window_Width), Int (Window_Height));
      GL.Toggles.Enable (GL.Toggles.Cull_Face);
      GL.Culling.Set_Cull_Face (GL.Culling.Back);

      Utilities.Clear_Background_Colour_And_Depth (Dark_Blue);

      GL.Objects.Programs.Use_Program (Render_Program);
      Controls.Compute_Matrices_From_Inputs (Window, Projection_Matrix, View_Matrix);
      MVP_Matrix := Projection_Matrix * View_Matrix * Model_Matrix;
      Depth_Bias_MVP_Matrix := Bias_Matrix * Depth_MVP_Matrix;

      GL.Uniforms.Set_Single (MVP_Matrix_ID, MVP_Matrix);
      GL.Uniforms.Set_Single (Depth_Bias_ID, Depth_Bias_MVP_Matrix);

      GL.Objects.Textures.Set_Active_Unit (0);
      Texture_2D.Bind (UV_Map);
      GL.Uniforms.Set_Int (Texture_ID, 0);

      GL.Objects.Textures.Set_Active_Unit (1);
      Texture_2D.Bind (Depth_Texture);
      GL.Uniforms.Set_Int (Shadow_Map_ID, 1);

      --  First attribute buffer : vertices
      GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False, 0, 0);
      --  Second attribute buffer : UVs
      GL.Objects.Buffers.Array_Buffer.Bind (UVs_Buffer);
      GL.Attributes.Enable_Vertex_Attrib_Array (1);
      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, False, 0, 0);
      --  Third attribute buffer : Normals
      GL.Objects.Buffers.Array_Buffer.Bind (Normals_Buffer);
      GL.Attributes.Enable_Vertex_Attrib_Array (2);
      GL.Attributes.Set_Vertex_Attrib_Pointer (2, 3, Single_Type, False, 0, 0);

      GL.Objects.Buffers.Element_Array_Buffer.Bind (Element_Buffer);
      GL.Objects.Buffers.Draw_Elements (Triangles, Indices_Size, UInt_Type, 0);

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
      GL.Attributes.Disable_Vertex_Attrib_Array (1);
      GL.Attributes.Disable_Vertex_Attrib_Array (2);

   exception
      when others =>
         Put_Line ("An exception occurred in Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Setup (Window : in out Glfw.Windows.Window) is
      use GL.Objects.Framebuffers;
      use GL.Objects.Shaders;
      use GL.Types;
      use GL.Types.Singles;
      use Glfw.Input;
      Window_Width    : Glfw.Size := 1024;
      Window_Height   : Glfw.Size := 768;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      Window.Set_Input_Toggle (Sticky_Keys, True);
      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      GL.Buffers.Set_Depth_Function (GL.Types.Less);
      GL.Toggles.Enable (GL.Toggles.Cull_Face);
      Window.Set_Cursor_Mode (Mouse.Disabled);
      Glfw.Input.Poll_Events;

      Window'Access.Set_Size (Window_Width, Window_Height);
      Window'Access.Set_Cursor_Pos (Mouse.Coordinate (0.5 * Single (Window_Width)),
                                    Mouse.Coordinate (0.5 * Single (Window_Height)));
      Model_Matrix := Maths.Rotation_Matrix
          (Maths.Degrees (90.0), (0.0, 1.0, 0.0)) * Model_Matrix;
      Model_Matrix := Maths.Translation_Matrix ((-1.0, -3.0, -12.0)) * Model_Matrix;

      Vertices_Array_Object.Initialize_Id;
      Vertices_Array_Object.Bind;

      Frame_Buffer.Initialize_Id;
      Read_And_Draw_Target.Bind (Frame_Buffer);

      Depth_Program := Program_Loader.Program_From
        ((Program_Loader.Src ("src/shaders/depth_rtt_vertex_shader.glsl",
         Vertex_Shader),
         Program_Loader.Src ("src/shaders/depth_rtt_fragment_shader.glsl",
           Fragment_Shader)));
      Depth_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Depth_Program, "depthMVP");

      Load_DDS ("src/textures/uvmap.DDS", UV_Map);
      Render_Program := Program_Loader.Program_From
        ((Program_Loader.Src ("src/shaders/shadow_mapping_simple_version_vertex_shader.glsl",
         Vertex_Shader),
         Program_Loader.Src ("src/shaders/shadow_mapping_simple_version_fragment_shader.glsl",
           Fragment_Shader)));
      GL.Objects.Programs.Use_Program (Render_Program);
      Texture_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "myTextureSampler");
      MVP_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "MVP");
      Depth_Bias_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "DepthBiasMVP");
      Shadow_Map_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "shadowMap");

        Buffers_Manager.Load_Buffers (Vertex_Buffer, UVs_Buffer,
                                      Normals_Buffer, Element_Buffer,
                                      Vertex_Count, Indices_Size);
        Textures_Manager.Init (Frame_Buffer, Depth_Texture);

   exception
      when others =>
         Put_Line ("An exception occurred in Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running         : Boolean := True;
begin
   Setup (Main_Window);
   while Running loop
      Render (Main_Window);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and then
        not (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and then not Main_Window.Should_Close;
   end loop;

exception
   when others =>
      Put_Line ("An exception occurred in Main_Loop.");
      raise;
end Main_Loop;
