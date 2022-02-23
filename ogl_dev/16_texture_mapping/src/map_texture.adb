--  OGLDev Tutorial 28, Particle System using Transform Feedback
--  Ada implemented by Roger Mc Murtrie
--  7 March 1918

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Map_Texture is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "OGLDev Tutorial 16 - Basic_Texture_Mapping";
begin
   pragma Warnings (Off);
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Map_Texture returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Map_Texture.");
        Put_Line (Exception_Information (anError));

end Map_Texture;
