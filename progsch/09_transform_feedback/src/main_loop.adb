
with Ada.Text_IO; use Ada.Text_IO;

with GL.Blending;
with GL.Toggles;
with GL.Types.Colors;

with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows;
with Glfw.Windows.Context;

with Utilities;

with Particle_System;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Background           : constant GL.Types.Colors.Color := (0.3, 0.3, 0.8, 0.0);
   --  ------------------------------------------------------------------------

   procedure Render (Window : in out Glfw.Windows.Window) is
   begin
      Utilities.Clear_Background_Colour_And_Depth (Background);

      Particle_System.Render_Particles (Window);

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Setup (Window : in out Glfw.Windows.Window) is
      use GL.Types;
      use Glfw.Input;
      Window_Width    : constant Glfw.Size := 1024;
      Window_Height   : constant Glfw.Size := 768;

   begin
      Window.Set_Input_Toggle (Sticky_Keys, True);
      Window.Set_Cursor_Mode (Mouse.Disabled);
      Glfw.Input.Poll_Events;
      GL.Toggles.Disable (GL.Toggles.Depth_Test);
      GL.Toggles.Enable (GL.Toggles.Blend);
      GL.Blending.Set_Blend_Func (GL.Blending.One, GL.Blending.One);

      Window'Access.Set_Size (Window_Width, Window_Height);
      Window'Access.Set_Cursor_Pos (Mouse.Coordinate (0.5 * Single (Window_Width)),
                                    Mouse.Coordinate (0.5 * Single (Window_Height)));
      Utilities.Clear_Background_Colour_And_Depth (Background);

      Particle_System.Init;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Setup.");
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
