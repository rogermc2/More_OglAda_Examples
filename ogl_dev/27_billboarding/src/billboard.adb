--  OGLDev Tutorial 27, Billboarding and the Geometry Shader
--  Ada implemented by Roger Mc Murtrie
--  26 September 1919

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Billboard is
    Main_Window  : Glfw.Windows.Window;
   Window_Title : constant String :=
                    "OGLDev Tutorial 27 - Billboarding and the Geometry Shader";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Billboard returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Billboard.");
        Put_Line (Exception_Information (anError));

end Billboard;
