
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows;
with Glfw.Windows.Context;

with GL.Buffers;
with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Types.Colors;
with GL.Toggles;
with GL.Window;

with Maths;
with Utilities;

with Shader_Manager_Texture;
with Textures_Manager;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Input_Callback.Callback_Window) is

   Back             : constant GL.Types.Colors.Color := (0.6, 0.6, 0.6, 0.0);
   Rotation_Vector  : GL.Types.Singles.Vector3 := (1.0, 1.0, 1.0);
   Texture_Marble   : GL.Objects.Textures.Texture;

   procedure Resize_GL_Scene (Screen : in out Input_Callback.Callback_Window);
   procedure Start_Game;

   --  -------------------------------------------------------------------------

   procedure Draw_Textured_Cube is
      use Maths;
      use GL.Types;
      use GL.Types.Singles;
      use Shader_Manager_Texture;
      View_Matrix       : constant Matrix4 := Identity4;
      Rot_Matrix        : Matrix4 := Identity4;
      Model_Matrix      : Matrix4 := Identity4;
      Projection_Matrix : Matrix4 := Identity4;
   begin
      Init_Rotation_Transform (Rotation_Vector, Rot_Matrix);
      Model_Matrix := Translation_Matrix ((0.0, 0.0, 5.0)) * Rot_Matrix *
        Model_Matrix;
      Init_Orthographic_Transform
        (5.0, -5.0, -5.0, 5.0, 0.0, 10.0, Projection_Matrix);
      Use_Texture_Program;
      Set_Model_Matrix (Model_Matrix);
      Set_View_Matrix (View_Matrix);
      Set_Projection_Matrix (Projection_Matrix);

      Textures_Manager.Bind (Texture_Marble);
      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 36);

      Rotation_Vector := Rotation_Vector + (0.1, 0.2, 0.3);

   end Draw_Textured_Cube;

   --  ------------------------------------------------------------------------

   procedure Initialize_3D is
      Ambient_Colour  : constant GL.Types.Singles.Vector4 :=
                          (0.0, 0.0, 1.0, 1.0);
      use Shader_Manager_Texture;
   begin
      Init_Shaders;
      Use_Texture_Program;
      Set_Ambient_Light (Ambient_Colour);
      Textures_Manager.Init (Texture_Marble);
   end Initialize_3D;

   --  ------------------------------------------------------------------------

   procedure Render_3D is
   begin
      Draw_Textured_Cube;
   end Render_3D;

   --  ------------------------------------------------------------------------

   procedure Render (Screen : in out Input_Callback.Callback_Window) is
   begin
      Utilities.Clear_Colour_Buffer_And_Depth;
      Resize_GL_Scene (Screen);
      Render_3D;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Resize_GL_Scene (Screen : in out Input_Callback.Callback_Window) is
      Screen_Width      : Glfw.Size;
      Screen_Height     : Glfw.Size;
   begin
      Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
      if Integer (Screen_Height) = 0 then
         Screen_Height := 1;
      end if;
      GL.Window.Set_Viewport (0, 0, GL.Types.Size (Screen_Width),
                              GL.Types.Size (Screen_Height));

   end Resize_GL_Scene;

   --  ------------------------------------------------------------------------

   procedure Start_Game is
      use GL.Types;
   begin
      Utilities.Clear_Background_Colour_And_Depth (Back);
      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      GL.Buffers.Set_Depth_Function (GL.Types.LEqual);
      Initialize_3D;

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Start_Game.");
         Put_Line (Exception_Information (anError));
         raise;
   end Start_Game;

   --  -------------------------------------------------------------------------

   use Glfw.Input;
   Running  : Boolean := True;
begin
   Start_Game;
   while Running loop
      Render (Main_Window);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and not
        (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
   end loop;

exception
   when anError : others =>
      Put_Line ("An exception occurred in Main_Loop.");
      Put_Line (Exception_Information (anError));
      raise;

end Main_Loop;
