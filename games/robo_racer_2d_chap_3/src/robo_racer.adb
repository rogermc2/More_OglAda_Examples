-- Program Robo_Racer
-- Author Roger Mc Murtrie
-- Created 8 May 2021

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Robo_Racer is
    Main_Window : Glfw.Windows.Window;
    Window_Title : constant String := "Robo Racer 2D";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Robo_Racer returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Robo_Racer.");
        Put_Line (Exception_Information (anError));
end Robo_Racer;
