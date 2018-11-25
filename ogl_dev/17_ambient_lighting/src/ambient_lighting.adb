--  OGLDev Tutorial 17, Ambient Lighting
--  Ada implemented by Roger Mc Murtrie
--  26 November 1918

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Ambient_Lighting is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "OGLDev Tutorial 17 - Ambient Lighting";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Ambient_Lighting returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Ambient_Lighting.");
        Put_Line (Exception_Information (anError));

end Ambient_Lighting;
