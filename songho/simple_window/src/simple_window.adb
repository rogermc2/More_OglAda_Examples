--  Program Simple_Window
--  Author Roger Mc Murtrie
--  Created 22 June 2020
--  Based on Songho's glWinSinmple

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Simple_Window is
    Main_Window   : aliased Glfw.Windows.Window;
begin
    Glfw.Init;
    Initialize (Main_Window, "Songho Simple_Window");
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Simple_Window returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Simple_Window.");
        Put_Line (Exception_Information (anError));
end Simple_Window;
