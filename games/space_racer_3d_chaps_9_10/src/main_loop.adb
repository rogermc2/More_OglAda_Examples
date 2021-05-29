
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows;
with Glfw.Windows.Context;

with GL.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Types.Colors;
with GL.Window;

with Maths;
with Utilities;

with Buffers_Manager;
with Shader_Manager;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Back                     : constant GL.Types.Colors.Color :=
                                (0.6, 0.6, 0.6, 0.0);
   Border_Width             : constant GL.Types.Size := 2;
   Game_Program             : GL.Objects.Programs.Program;
   Rotation                 : Maths.Degree := 0.0;
   --      Full_Screen                  : Boolean := False;

   procedure Resize_GL_Scene  (Screen : in out Glfw.Windows.Window);

   --  ------------------------------------------------------------------------

   procedure Draw_Pyramid is
      use GL.Types;
      use GL.Types.Singles;
      use Maths;
      Model_Matrix : Singles.Matrix4 := Singles.Identity4;
      Trans_Matrix : constant Singles.Matrix4 := Translation_Matrix ((-1.5, 0.0, -6.0));
--        Rot_Matrix   : constant Singles.Matrix4 := Singles.Identity4;
   begin
      Utilities.Clear_Colour_Buffer_And_Depth;

      Model_Matrix := Trans_Matrix;
      GL.Objects.Programs.Use_Program (Game_Program);
      Shader_Manager.Set_Model_Matrix (Model_Matrix);

      Buffers_Manager.Cube_VAO.Bind;
      GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Triangles,
                                            First => 0,
                                            Count => 4 * 3);
      Rotation := Rotation - 0.5;
   end Draw_Pyramid;

   --  ------------------------------------------------------------------------

   procedure Render (Screen : in out Glfw.Windows.Window) is
   begin
      Utilities.Clear_Colour;
      Resize_GL_Scene (Screen);
      Draw_Pyramid;

   end Render;

   --  ------------------------------------------------------------------------

   procedure Resize_GL_Scene (Screen : in out Glfw.Windows.Window) is
      use GL.Objects.Programs;
      use GL.Types;
      Screen_Width      : Glfw.Size;
      Screen_Height     : Glfw.Size;
      VP_Width          : Size;
      VP_Height         : Size;
      Projection_Matrix : Singles.Matrix4 := Singles.Identity4;
   begin
      Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
      VP_Width := Size (Screen_Width) - 2 * Border_Width;
      VP_Height := Size (Screen_Height) - 2 * Border_Width;
      GL.Window.Set_Viewport (Border_Width, Border_Width, VP_Width, VP_Height);

      Maths.Init_Perspective_Transform
        (Maths.Degree (45.0), Single (Screen_Width), Single (Screen_Height), 0.1, 100.0,
         Projection_Matrix);
      Use_Program (Game_Program);
      Shader_Manager.Set_Projection_Matrix (Projection_Matrix);

   end Resize_GL_Scene;

   --  ------------------------------------------------------------------------

   procedure Start_Game (Screen : in out Glfw.Windows.Window) is
      use GL.Types;
      Window_Width       : Glfw.Size;
      Window_Height      : Glfw.Size;
   begin
      Screen'Access.Get_Size (Window_Width, Window_Height);

      Utilities.Clear_Background_Colour_And_Depth (Back);
      GL.Buffers.Set_Depth_Function (LEqual);

      Shader_Manager.Init_Shaders (Game_Program);
      Buffers_Manager.Setup_Buffers;

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Start_Game.");
         Put_Line (Exception_Information (anError));
         raise;
   end Start_Game;

   --  -------------------------------------------------------------------------

   procedure Update (Window : in out Glfw.Windows.Window) is
   begin
      null;
   end Update;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running  : Boolean := True;
begin
   Start_Game (Main_Window);
   while Running loop
      Update (Main_Window);
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
