
with Glfw;
with Glfw.Input;
with Glfw.Windows.Context;

with GL.Types.Colors;
with GL.Window;
with Utilities;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    Red   : constant GL.Types.Colors.Color := (1.0, 0.0, 0.0, 1.0);
    Green : constant GL.Types.Colors.Color := (0.0, 1.0, 0.0, 1.0);
    Blue  : constant GL.Types.Colors.Color := (0.0, 0.0, 1.0, 1.0);

    procedure Display (Tone : GL.Types.Colors.Color) is
    begin
        Utilities.Clear_Background_Colour (Tone);
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Delay (1.2);
    end Display;

--  ----------------------------------------------------------------------------
begin
    GL.Window.Set_Viewport (0, 0, 512, 512);
    Display (Red);
    Display (Green);
    Display (Blue);
end Main_Loop;
