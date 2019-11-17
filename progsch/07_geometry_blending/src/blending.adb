--  Geometry Shader Blending Example
--  Author Roger Mc Murtrie
--  Created 16 November 2019
--  Based on https://github.com/progschj/OpenGL-Examples
--  A geometry shader is used to expand points to billboard quads.
--  The billboards are blended while drawing to create a galaxy of particles.

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Blending is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "Example 7 Geometry Shader Blending";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Blending returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Blending.");
        Put_Line (Exception_Information (anError));
end Blending;
