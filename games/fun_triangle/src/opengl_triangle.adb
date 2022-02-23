-- Program OpenGL_Triangle
-- Author Roger Mc Murtrie
-- Created 7 May 2021

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure OpenGL_Triangle is
    Main_Window : Glfw.Windows.Window;
    Window_Title : constant String := "OpenGL Triangle";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("OpenGL_Triangle returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in OpenGL_Triangle.");
        Put_Line (Exception_Information (anError));
end OpenGL_Triangle;
