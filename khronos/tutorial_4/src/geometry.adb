--  Program Geometry
--  Author Roger Mc Murtrie
--  Created 11 August 2020
--  Based on khronos.org OpenGL Tutorial 4

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Geometry is
    Main_Window : Glfw.Windows.Window;
    Window_Title : constant String :=
                     "Khronous OpenGL Tutorial 4 Indices and Geometry Shaders";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Geometry returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Geometry.");
        Put_Line (Exception_Information (anError));
end Geometry;
