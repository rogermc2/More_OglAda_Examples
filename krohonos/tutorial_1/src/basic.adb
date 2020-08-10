-- Program A_Dot
-- Author Roger Mc Murtrie
-- Created 16 December 2016

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Tutorial_1 is
    Main_Window : Glfw.Windows.Window;
    Window_Title : constant String := "OpenGL Tutorial_1";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Tutorial_1 returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Tutorial_1.");
        Put_Line (Exception_Information (anError));
end Tutorial_1;
