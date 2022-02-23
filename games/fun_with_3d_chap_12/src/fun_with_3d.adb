--  Program Fun_With_3D Chapter 12
--  Author Roger Mc Murtrie
--  Created 12 June 2021

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;

with Initialize_With_Callbacks;
with Input_Callback;
with Main_Loop;

procedure Fun_With_3D is
    Main_Window : Input_Callback.Callback_Window;
    Window_Title : constant String := "Fun_With_3D Chapter 12";
begin
    Glfw.Init;
    Initialize_With_Callbacks (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Fun_With_3D returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Fun_With_3D.");
        Put_Line (Exception_Information (anError));
end Fun_With_3D;
