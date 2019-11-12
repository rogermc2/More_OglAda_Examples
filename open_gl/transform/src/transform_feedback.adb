--  Program Transform Feedback
--  Author Roger Mc Murtrie
--  Created 12 November 2019

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Transform_Feedback is
    Main_Window : Glfw.Windows.Window;
    Window_Title : constant String := "Transform Feedback Example";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Transform_Feedback returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Transform_Feedback.");
        Put_Line (Exception_Information (anError));
end Transform_Feedback;
