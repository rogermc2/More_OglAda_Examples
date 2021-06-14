
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Input.Mouse;
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
with Shader_Manager_UI;
with Textures_Manager;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Input_Callback.Callback_Window) is

   Back             : constant GL.Types.Colors.Color :=
                        (0.6, 0.6, 0.6, 0.0);
   Rotation_Vector  : GL.Types.Singles.Vector3 := (1.0, 1.0, 1.0);
   Texture_Marble   : GL.Objects.Textures.Texture;

   procedure Initialize_2D;
   procedure Resize_GL_Scene (Screen : in out Input_Callback.Callback_Window);
   procedure Start_Game  (Screen : in out Input_Callback.Callback_Window);

   --  -------------------------------------------------------------------------

   procedure Draw_Textured_Cube
      (Screen : in out Input_Callback.Callback_Window) is
      use Maths;
      use GL.Types;
      use GL.Types.Singles;
      use Shader_Manager_Texture;
       View_Matrix       : constant Matrix4 := Identity4;
       Rot_Matrix        : Matrix4 := Identity4;
       Model_Matrix      : Matrix4 := Identity4;
       Projection_Matrix : constant Matrix4 := Identity4;
       Screen_Width      : Glfw.Size;
       Screen_Height     : Glfw.Size;
   begin
      Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
      GL.Window.Set_Viewport (0, 0, Int (Screen_Width), Int (Screen_Height));
      Init_Rotation_Transform (Rotation_Vector, Rot_Matrix);
      Model_Matrix := Translation_Matrix ((-0.0, -0.0, 0.0)) * Rot_Matrix *
          Scaling_Matrix (0.6) * Model_Matrix;
--        Init_Orthographic_Transform
--            (2.0, 0.0, 0.0, 2.0, 0.0, 10.0, Projection_Matrix);
--        Utilities.Print_Matrix ("Projection_Matrix", Projection_Matrix);
      Use_Texture_Program;
      Set_Model_Matrix (Model_Matrix);
      Set_View_Matrix (View_Matrix);
      Set_Projection_Matrix (Projection_Matrix);

      Textures_Manager.Bind (Texture_Marble);
      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 36);

      Rotation_Vector := Rotation_Vector + (0.1, 0.2, 0.3);

   end Draw_Textured_Cube;

   --  ------------------------------------------------------------------------

   procedure Enable_2D (Screen : in out Input_Callback.Callback_Window) is
      use GL.Types;
      use Shader_Manager_UI;
      Screen_Width      : Glfw.Size;
      Screen_Height     : Glfw.Size;
      Projection_Matrix : Singles.Matrix4;
   begin
      Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
      Use_2D_Program;
      Maths.Init_Orthographic_Transform
        (Top => Single (Screen_Height), Bottom => 0.0,
         Left => 0.0, Right => Single (Screen_Width),
         Z_Near => 0.0, Z_Far => 1.0, Transform => Projection_Matrix);
      Shader_Manager_UI.Set_Projection_Matrix (Projection_Matrix);
   end Enable_2D;

   --  -------------------------------------------------------------------------

   procedure Enable_Mouse_Callbacks
     (Window : in out Input_Callback.Callback_Window;
      Enable : Boolean) is
      use Glfw.Windows.Callbacks;
   begin
      if Enable then
         Window.Enable_Callback (Mouse_Position);
         Window.Enable_Callback (Mouse_Enter);
         Window.Enable_Callback (Mouse_Button);
         Window.Enable_Callback (Mouse_Scroll);
      else
         Window.Disable_Callback (Mouse_Position);
         Window.Disable_Callback (Mouse_Enter);
         Window.Disable_Callback (Mouse_Button);
         Window.Disable_Callback (Mouse_Scroll);
      end if;
   end Enable_Mouse_Callbacks;

   ----------------------------------------------------------------------------

   procedure Initialize_2D is
   begin
      Shader_Manager_UI.Init_Shaders;
        null;
   end Initialize_2D;

   --  ------------------------------------------------------------------------

   procedure Render_2D (Screen : in out Input_Callback.Callback_Window) is
   begin
      Enable_2D (Screen);
        null;
   end Render_2D;

   --  ------------------------------------------------------------------------

   procedure Render_3D (Screen : in out Input_Callback.Callback_Window) is
   begin
      Draw_Textured_Cube (Screen);
   end Render_3D;

   --  ------------------------------------------------------------------------

   procedure Render (Screen : in out Input_Callback.Callback_Window) is
   begin
      Utilities.Clear_Colour_Buffer_And_Depth;
      Resize_GL_Scene (Screen);
      Render_3D (Screen);
      Render_2D (Screen);
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

   procedure Start_Game  (Screen : in out Input_Callback.Callback_Window) is
      use GL.Types;
      use Glfw.Input;
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
   begin
      Screen.Set_Cursor_Mode (Mouse.Normal);
      Screen'Access.Get_Size (Window_Width, Window_Height);
      Screen'Access.Set_Cursor_Pos
        (Mouse.Coordinate (0.5 * Single (Window_Width)),
         Mouse.Coordinate (0.5 * Single (Window_Height)));

      Utilities.Clear_Background_Colour_And_Depth (Back);
        GL.Toggles.Enable (GL.Toggles.Depth_Test);
        GL.Buffers.Set_Depth_Function (GL.Types.LEqual);
      Input_Callback.Clear_All_Keys;
      --          GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);
      Shader_Manager_Texture.Init_Shaders;
      Textures_Manager.Init (Texture_Marble);
      Initialize_2D;
      Enable_Mouse_Callbacks (Screen, True);

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Start_Game.");
         Put_Line (Exception_Information (anError));
         raise;
   end Start_Game;

   --  -------------------------------------------------------------------------

   procedure Update is
   begin
      null;
   end Update;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running  : Boolean := True;
begin
   Start_Game (Main_Window);
   while Running loop
      Update;
      if Running then
         Render (Main_Window);
      end if;
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
