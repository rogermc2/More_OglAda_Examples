-- Program Vibrator
-- Author Roger Mc Murtrie
-- Created 29 October 2019

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Vibrator is
    Main_Window : Glfw.Windows.Window;
    Window_Title : constant String := "OpenGL SuperBible Spring Mass Simulator";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Vibrator returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exceptiom occurred in Vibrator.");
        Put_Line (Exception_Information (anError));
end Vibrator;
