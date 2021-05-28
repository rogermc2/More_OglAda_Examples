--  Program Robo_Racer Chapter 8
--  Author Roger Mc Murtrie
--  Created 28 May 2021

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;

with Initialize_With_Callbacks;
with Input_Callback;
with Main_Loop;

procedure Robo_Racer_3D is
    Main_Window : Input_Callback.Callback_Window;
    Window_Title : constant String := "Robo Racer 2D Chapter 8";
begin
    Glfw.Init;
    Initialize_With_Callbacks (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Robo_Racer_3D returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Robo_Racer_3D.");
        Put_Line (Exception_Information (anError));
end Robo_Racer_3D;
