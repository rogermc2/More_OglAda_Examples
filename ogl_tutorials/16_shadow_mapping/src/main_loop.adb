
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types.Colors;
with GL.Uniforms;

with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows;
with Glfw.Windows.Context;

with Controls;
with Program_Loader;
with Load_DDS;
with Maths;
with Utilities;

with Buffers_Manager;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Dark_Blue                : constant GL.Types.Colors.Color := (0.0, 0.0, 0.4, 0.0);

   Render_Program           : GL.Objects.Programs.Program;
   Vertices_Array_Object    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Normals_Buffer           : GL.Objects.Buffers.Buffer;
   UVs_Buffer               : GL.Objects.Buffers.Buffer;
   Vertex_Buffer            : GL.Objects.Buffers.Buffer;
   Indexed_Normals_Buffer   : GL.Objects.Buffers.Buffer;
   Indexed_UVs_Buffer       : GL.Objects.Buffers.Buffer;
   Indexed_Vertex_Buffer    : GL.Objects.Buffers.Buffer;
   Element_Buffer           : GL.Objects.Buffers.Buffer;
   Indices_Size             : GL.Types.Int;
   UV_Map                   : GL.Objects.Textures.Texture;
   Vertex_Count             : GL.Types.Int;
   Indexed_Vertex_Count     : GL.Types.Int;
   Depth_Matrix_ID          : GL.Uniforms.Uniform;
   MVP_Matrix_ID            : GL.Uniforms.Uniform;
   Texture_ID               : GL.Uniforms.Uniform;
   Model_Matrix             : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;

   --  ------------------------------------------------------------------------

   procedure Load_Texture (Window : in out Glfw.Windows.Window;
                           Light_Map : GL.Objects.Textures.Texture) is
      use GL.Types;
      use GL.Types.Singles;
      View_Matrix       : Singles.Matrix4;
      Projection_Matrix : Singles.Matrix4;
      MVP_Matrix        : Singles.Matrix4;
   begin
      Controls.Compute_Matrices_From_Inputs (Window, Projection_Matrix, View_Matrix);

      MVP_Matrix :=  Projection_Matrix * View_Matrix * Model_Matrix;
      GL.Uniforms.Set_Single (MVP_Matrix_ID, MVP_Matrix);

      --  Bind the texture to Texture Unit 0
      GL.Objects.Textures.Set_Active_Unit (0);
      GL.Objects.Textures.Targets.Texture_2D.Bind (Light_Map);
      GL.Uniforms.Set_Int (Texture_ID, 0);

   exception
      when others =>
         Put_Line ("An exception occurred in Load_Texture.");
         raise;
   end Load_Texture;

   --  ------------------------------------------------------------------------

   procedure Render (Window : in out Glfw.Windows.Window) is
      use GL.Objects.Buffers;
      use GL.Types;
      begin
      Utilities.Clear_Background_Colour_And_Depth (Dark_Blue);
      GL.Objects.Programs.Use_Program (Render_Program);
      Load_Texture (Window, UV_Map);

      --  First attribute buffer : vertices
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, True, 0, 0);
      --  Second attribute buffer : UVs
      GL.Attributes.Enable_Vertex_Attrib_Array (1);
      GL.Objects.Buffers.Array_Buffer.Bind (UVs_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, True, 0, 0);
      --  Third attribute buffer : Normals
--        GL.Attributes.Enable_Vertex_Attrib_Array (2);
--        GL.Objects.Buffers.Array_Buffer.Bind (Normals_Buffer);
--        GL.Attributes.Set_Vertex_Attrib_Pointer (2, 3, Single_Type, True, 0, 0);

      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, Vertex_Count);

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
      GL.Attributes.Disable_Vertex_Attrib_Array (1);
--        GL.Attributes.Disable_Vertex_Attrib_Array (2);

   exception
      when others =>
         Put_Line ("An exception occurred in Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Setup (Window : in out Glfw.Windows.Window) is
      use GL.Objects.Shaders;
      use GL.Types;
      use GL.Types.Singles;
      use Glfw.Input;
      Window_Width    : constant Glfw.Size := 1024;
      Window_Height   : constant Glfw.Size := 768;
   begin
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
      Model_Matrix := Maths.Translation_Matrix ((-1.0, -2.0, -8.0)) * Model_Matrix;

      Vertices_Array_Object.Initialize_Id;
      Vertices_Array_Object.Bind;

      Render_Program := Program_Loader.Program_From
        ((Program_Loader.Src ("src/shaders/depth_rtt_vertex_shader.glsl",
         Vertex_Shader),
         Program_Loader.Src ("src/shaders/depth_rtt_fragment_shader",
           Fragment_Shader)));

      GL.Objects.Programs.Use_Program (Render_Program);
      Depth_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "depthMVP");
      MVP_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "MVP");

      Load_DDS ("src/textures/uvmap.DDS", UV_Map);
      Texture_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "myTextureSampler");

        Buffers_Manager.Load_Buffers
          (Vertex_Buffer, UVs_Buffer, Normals_Buffer, Element_Buffer, Indices_Size);

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
