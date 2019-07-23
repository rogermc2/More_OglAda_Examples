--  OGLDev Tutorial 15, Camera Control 2
--  Ada implemented by Roger Mc Murtrie
--  23 July 2019

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Camera_Control is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "OGLDev Tutorial 15 - Camera Control 2";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Camera_Control returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Camera_Control.");
        Put_Line (Exception_Information (anError));

end Camera_Control;
