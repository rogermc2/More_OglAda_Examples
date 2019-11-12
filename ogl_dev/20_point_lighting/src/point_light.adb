--  OGLDev Tutorial 20, Point Lighting
--  Ada implemented by Roger Mc Murtrie
--  26 November 1918

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Point_Light is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "OGLDev Tutorial 20 - Point Light";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Point_Light returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Point_Light.");
        Put_Line (Exception_Information (anError));

end Point_Light;
