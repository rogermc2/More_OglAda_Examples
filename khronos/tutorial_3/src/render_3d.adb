--  Program Render_3D
--  Author Roger Mc Murtrie
--  Created 10 August 2020
--  Based on khronos.org OpenGL Tutorial 3

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Render_3D is
    Main_Window : Glfw.Windows.Window;
    Window_Title : constant String :=
                     "Khronous OpenGL Tutorial 3 Rendering 3D Objects";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Render_3D returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Render_3D.");
        Put_Line (Exception_Information (anError));
end Render_3D;
