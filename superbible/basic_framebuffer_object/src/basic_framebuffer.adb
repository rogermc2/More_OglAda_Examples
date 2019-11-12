--  Program Basic Framebuffer
--  Author Roger Mc Murtrie
--  Created 23 December 2016

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Basic_Framebuffer is
    Main_Window :  Glfw.Windows.Window;
begin
    Glfw.Init;
    Initialize (Main_Window, "OpenGL SupeBible - Basic Frame Buffer Example");
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Basic_Framebuffer returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Basic_Framebuffer.");
        Put_Line (Exception_Information (anError));
end Basic_Framebuffer;
