--  Author Roger Mc Murtrie
--  Based on OpenGL Programming/Scientific OpenGL Tutorial graph03-sdl2
--  1 December 2020

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Main_Loop;
with Initialize_With_Callbacks;
with Input_Callback;

procedure Graph_3 is
    Main_Window  : Input_Callback.Callback_Window;
    Window_Title : constant String := "Modern Tutorial Graph 3";
begin
    Glfw.Init;
    Initialize_With_Callbacks (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Graph_3 returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Graph_3.");
        Put_Line (Exception_Information (anError));

end Graph_3;
