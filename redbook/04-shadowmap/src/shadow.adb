--  Shadow implements Redbook example 04-shadowmap from OGLPG-9th-Edition
--  Author: R Mc Murtrie
--  3rd March 1918

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Shadow is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "Shadow Map";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Shadow returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Shadow.");
        Put_Line (Exception_Information (anError));

end Shadow;
