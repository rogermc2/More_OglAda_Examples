

with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

with Sprite_Textures;
with Vertex_Data;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is

   Quad_VAO          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer     : GL.Objects.Buffers.Buffer;
   theTexture        : GL.Objects.Textures.Texture;
   Sprite_SP         : GL.Objects.Programs.Program;
   Model_ID          : GL.Uniforms.Uniform;
   Projection_ID     : GL.Uniforms.Uniform;
   Image_ID          : GL.Uniforms.Uniform;
   Colour_ID         : GL.Uniforms.Uniform;
   Projection_Matrix : GL.Types.Singles.Matrix4;

   --  ------------------------------------------------------------------------

   procedure Draw_Sprite (aTexture       : GL.Objects.Textures.Texture;
                          Position, Size : GL.Types.Singles.Vector2;
                          Rotate         : Maths.Radian;
                          Colour         : GL.Types.Singles.Vector3) is
      use GL.Objects.Buffers;
      use GL.Objects.Programs;
      use GL.Objects.Textures.Targets;
      use GL.Types;
      use Singles;
      use Maths;
      Model_Matrix : Singles.Matrix4;
   begin

      Model_Matrix :=
        Scaling_Matrix ((Size (GL.X), Size (GL.Y), 1.0)) * Model_Matrix;
      Model_Matrix := Translation_Matrix
        ((-0.5 * Size (GL.X), -0.5 * Size (GL.Y), 0.0)) * Model_Matrix;
      Model_Matrix := Rotation_Matrix (Rotate, (0.0, 0.0, 1.0)) * Model_Matrix;
      Model_Matrix := Translation_Matrix
        ((0.5 * Size (GL.X), 0.5 * Size (GL.Y), 0.0)) * Model_Matrix;
      Model_Matrix := Translation_Matrix ((Position (GL.X), Position (GL.Y), 0.0));

      Use_Program (Sprite_SP);
      GL.Uniforms.Set_Single (Model_ID, Model_Matrix);
      GL.Uniforms.Set_Single (Colour_ID, Colour);

      GL.Objects.Textures.Set_Active_Unit (0);
      Texture_2D.Bind (aTexture);
      Quad_VAO.Bind;

      GL.Objects.Vertex_Arrays.Draw_Arrays(Triangles, 0, 6);

   end Draw_Sprite;

   --  ------------------------------------------------------------------------

   procedure Init_Render_Data (Window : in out Glfw.Windows.Window) is
      use GL.Objects.Buffers;
      use GL.Types;
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
      Stride        : constant Int := Maths.Vector4'Size / 8;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      Quad_VAO.Initialize_Id;
      Quad_VAO.Bind;

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Utilities.Load_Vertex_Buffer
        (Array_Buffer, Vertex_Data.Vertices, Static_Draw);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 4, Single_Type, True, Stride, 0);
   end Init_Render_Data;

   --  ------------------------------------------------------------------------

   procedure Render  (Window  : in out Glfw.Windows.Window) is
      use GL.Types;
      Background    : constant GL.Types.Colors.Color := (0.2, 0.3, 0.3, 1.0);
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Utilities.Clear_Background_Colour (Background);

      GL.Objects.Buffers.Draw_Elements (Triangles, 6, UInt_Type, 0);

   exception
      when  others =>
         Put_Line ("An exception occurred in Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Setup (Window : in out Glfw.Windows.Window) is
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use GL.Types;
      use Program_Loader;
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
      Stride        : constant Int := Maths.Vector4'Size / 8;
   begin
      Init_Render_Data (Window);

      Sprite_SP := Program_From
        ((Src ("src/shaders/sprite.vs", Vertex_Shader),
         Src ("src/shaders/sprite.fs", Fragment_Shader)));

      GL.Objects.Programs.Use_Program (Sprite_SP);
      Model_ID := GL.Objects.Programs.Uniform_Location (Sprite_SP, "model");
      Projection_ID := GL.Objects.Programs.Uniform_Location (Sprite_SP, "projection");
      Image_ID := GL.Objects.Programs.Uniform_Location (Sprite_SP, "image");
      Colour_ID := GL.Objects.Programs.Uniform_Location (Sprite_SP, "spriteColour");

      Maths.Init_Orthographic_Transform
        (Single (Window_Height), 0.0, 0.0, Single (Window_Width), -1.0, 1.0,
         Projection_Matrix);

      Sprite_Textures.Load_Texture (theTexture, "../../textures/awesomeface.png");

      GL.Uniforms.Set_Int (Image_ID, 0);
      Sprite_Textures.Load_Texture (theTexture,
                                    "../resources/textures/container.jpg");
   exception
      when others =>
         Put_Line ("An exception occurred in Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   Setup (Main_Window);
   while Running loop
      Render (Main_Window);
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
