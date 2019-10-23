
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Particle_Simulator is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "Red Book Tutorial 12 - Particle Simulation";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Particle_Simulator returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Particle_Simulator.");
        Put_Line (Exception_Information (anError));

end Particle_Simulator;