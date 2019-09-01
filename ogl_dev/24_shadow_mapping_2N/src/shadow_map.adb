--  OGLDev Tutorial 24, Shadow Mapping Part 2
--  Ada implemented by Roger Mc Murtrie
--  1 Sepember 2019

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Shadow_Map is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "OGLDev Tutorial 24 - Shadow Mapping Part 2";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Shadow_Map returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Shadow_Map.");
        Put_Line (Exception_Information (anError));
end Shadow_Map;
