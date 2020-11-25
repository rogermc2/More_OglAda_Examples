--  Author Roger Mc Murtrie
--  Based on OpenGL Programming/Scientific OpenGL Tutorial tut06_textures-sdl2
--  25 November 2020

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Cube_6 is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "Modern Tutorial 6 Textures SDL";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Cube_6 returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Cube_6.");
        Put_Line (Exception_Information (anError));

end Cube_6;
