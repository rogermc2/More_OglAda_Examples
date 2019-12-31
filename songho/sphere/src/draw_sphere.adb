--  Program Draw Sphere
--  Author Roger Mc Murtrie
--  Created 31 December 2019

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Draw_Sphere is
    Main_Window   : aliased Glfw.Windows.Window;
begin
    Glfw.Init;
    Initialize (Main_Window, "Songho Draw_Sphere");
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Draw_Sphere returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Draw_Sphere.");
        Put_Line (Exception_Information (anError));
end Draw_Sphere;
