
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
with Load_Object_File;
with Maths;
with Utilities;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Dark_Blue                : constant GL.Types.Colors.Color := (0.0, 0.0, 0.4, 0.0);

   Vertices_Array_Object    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   UV_Buffer                : GL.Objects.Buffers.Buffer;
   Vertex_Buffer            : GL.Objects.Buffers.Buffer;
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

   procedure Render (Window : in out Glfw.Windows.Window;
                     Render_Program : GL.Objects.Programs.Program;
                     Vertex_Count   : GL.Types.Int;
                     Light_Map      : GL.Objects.Textures.Texture) is
      use GL.Objects.Buffers;
      use GL.Types;
   begin
      Utilities.Clear_Background_Colour_And_Depth (Dark_Blue);
      GL.Objects.Programs.Use_Program (Render_Program);
      Load_Texture (Window, Light_Map);

      --  First attribute buffer : vertices
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, True, 0, 0);
      --  Second attribute buffer : UVs
      GL.Attributes.Enable_Vertex_Attrib_Array (1);
      GL.Objects.Buffers.Array_Buffer.Bind (UV_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, True, 0, 0);

      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, Vertex_Count);

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
      GL.Attributes.Disable_Vertex_Attrib_Array (1);

   exception
      when others =>
         Put_Line ("An exception occurred in Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Setup (Window : in out Glfw.Windows.Window;
                    Render_Program : out GL.Objects.Programs.Program;
                    Vertex_Count   : out GL.Types.Int;
                    UV_Map         : out GL.Objects.Textures.Texture) is
      use GL.Objects.Buffers;
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
        ((Program_Loader.Src ("src/shaders/transform_vertex_shader.glsl",
         Vertex_Shader),
         Program_Loader.Src ("src/shaders/texture_fragment_shader_LOD.glsl",
           Fragment_Shader)));

      GL.Objects.Programs.Use_Program (Render_Program);
      MVP_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "MVP");

      Load_DDS ("src/textures/lightmap.DDS", UV_Map);
      Texture_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "myTextureSampler");

      Vertex_Count := Load_Object_File.Mesh_Size ("src/textures/room.obj");
      declare
         Vertices  : Singles.Vector3_Array (1 .. Vertex_Count);
         UVs       : Singles.Vector2_Array (1 .. Vertex_Count);
         Normals   : Singles.Vector3_Array (1 .. Vertex_Count);
      begin
         Load_Object_File.Load_Object ("src/textures/room.obj", Vertices, UVs, Normals);

         Vertex_Buffer.Initialize_Id;
         Array_Buffer.Bind (Vertex_Buffer);
         Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);

         UV_Buffer.Initialize_Id;
         Array_Buffer.Bind (UV_Buffer);
         Utilities.Load_Vertex_Buffer (Array_Buffer, UVs, Static_Draw);
      end;

   exception
      when others =>
         Put_Line ("An exception occurred in Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Render_Program  : GL.Objects.Programs.Program;
   Running         : Boolean := True;
   Vertex_Count    : GL.Types.Int;
   UV_Map          : GL.Objects.Textures.Texture;
begin
   Setup (Main_Window, Render_Program, Vertex_Count, UV_Map);
   while Running loop
      Render (Main_Window, Render_Program, Vertex_Count, UV_Map);
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
