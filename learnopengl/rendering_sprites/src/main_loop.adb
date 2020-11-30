
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Types;
with GL.Types.Colors;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows;
with Glfw.Windows.Context;

with Maths;
with Utilities;

with Sprite_Render;
with Sprite_Textures;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use Sprite_Render;
   theTexture    : GL.Objects.Textures.Texture;
   Sprite_SP     : GL.Objects.Programs.Program;

   --  ------------------------------------------------------------------------

   procedure Render  (Window  : in out Glfw.Windows.Window) is
      use GL.Types;
      Background    : constant Colors.Color := (0.0, 0.0, 0.0, 1.0);
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, Int (Window_Width),
                              Int (Window_Height));
      Utilities.Clear_Background_Colour (Background);

      Draw_Sprite (Sprite_SP, theTexture, (200.0, 200.0), (300.0, 400.0),
                   45.0, (0.0, 1.0, 0.0));

   exception
      when  others =>
         Put_Line ("An exception occurred in Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Init (Window : in out Glfw.Windows.Window) is
      use GL.Types;
      Window_Width      : Glfw.Size;
      Window_Height     : Glfw.Size;
      Projection_Matrix : Singles.Matrix4 := GL.Types.Singles.Identity4;
   begin
      Load_Shaders (Sprite_SP);
      Init_Render_Data (Window);

      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      Maths.Init_Orthographic_Transform
        (Single (Window_Height), 0.0, 0.0, Single (Window_Width), -1.0, 1.0,
         Projection_Matrix);
      Utilities.Print_Matrix ("Projection_Matrix", Projection_Matrix);
      GL.Objects.Programs.Use_Program (Sprite_SP);
      Sprite_Render.Set_Image (0);
      Sprite_Render.Set_Perspective (Projection_Matrix);

      Sprite_Textures.Load_Texture
        (theTexture, "../resources/textures/awesomeface.png");
   exception
      when others =>
         Put_Line ("An exception occurred in Setup.");
         raise;
   end Init;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   Init (Main_Window);
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
