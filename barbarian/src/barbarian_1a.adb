--  Program Barbarian 1a
--  Derived from Crongdor the Barbarian; author Anton Gerdelan
--  Author Roger Mc Murtrie
--  Created 20 August 2020

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;

with Main_Loop;

procedure Barbarian_1a is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "Barbarian 1a";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Barbarian_1a returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Barbarian_1a.");
        Put_Line (Exception_Information (anError));

end Barbarian_1a;
