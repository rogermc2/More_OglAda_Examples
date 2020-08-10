--  Program Basic
--  Author Roger Mc Murtrie
--  Created 10 August 2020
--  Based on khronos.org OpenGL Tutorial 1

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Basic is
    Main_Window : Glfw.Windows.Window;
    Window_Title : constant String := "Khronous OpenGL Tutorial_1";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Basic returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Basic.");
        Put_Line (Exception_Information (anError));
end Basic;
