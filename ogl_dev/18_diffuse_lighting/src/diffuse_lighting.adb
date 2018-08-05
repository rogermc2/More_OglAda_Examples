--  OGLDev Tutorial 18, Diffuse Lighting
--  Ada implemented by Roger Mc Murtrie
--  3 August 1918

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Diffuse_Lighting is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "OGLDev Tutorial 18 - Diffuse Lighting";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Diffuse_Lighting returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Diffuse_Lighting.");
        Put_Line (Exception_Information (anError));

end Diffuse_Lighting;
