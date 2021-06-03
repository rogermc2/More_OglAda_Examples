--  Program Space_Racer_3D Chapter 10
--  Author Roger Mc Murtrie
--  Created 29 May 2021

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Space_Racer_3D is
    Main_Window : Glfw.Windows.Window;
    Window_Title : constant String := "Space Racer 3D Chapter 10";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Space_Racer_3D returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Space_Racer_3D.");
        Put_Line (Exception_Information (anError));
end Space_Racer_3D;
