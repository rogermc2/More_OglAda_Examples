-- Program OpenGL_Quad
-- Author Roger Mc Murtrie
-- Created 7 May 2021

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure OpenGL_Quad is
    Main_Window : Glfw.Windows.Window;
    Window_Title : constant String := "OpenGL Quad";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("OpenGL_Quad returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in OpenGL_Quad.");
        Put_Line (Exception_Information (anError));
end OpenGL_Quad;
