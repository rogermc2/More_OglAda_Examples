--  https://en.wikibooks.org/wiki/OpenGL_Programming/Modern_OpenGL_Tutorial_07
--  13 December 1918

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Teapot is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "Modern Tutorial 07 - Teapot";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Teapot returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Teapot.");
        Put_Line (Exception_Information (anError));

end Teapot;
