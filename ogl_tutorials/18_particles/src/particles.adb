--  Program Particles
--  Based on Tutorial 18 from OpenGl Tutorial.org
--  10 November 2019

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Particles is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "Tutorial 18 - Particles";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;
exception
    when anError : Constraint_Error =>
        Put ("Particles returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Particles.");
        Put_Line (Exception_Information (anError));
end Particles;
