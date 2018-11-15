--  OGLDev Tutorial 26, Normal Mapping
--  Ada implemented by Roger Mc Murtrie
--  15 November 1918

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Normal_Map is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "OGLDev Tutorial 26 - Normal Mapping";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Normal_Map returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Normal_Map.");
        Put_Line (Exception_Information (anError));

end Normal_Map;
