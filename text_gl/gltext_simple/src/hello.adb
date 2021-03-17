--  Program Hello
--  Author Roger Mc Murtrie
--  Created 27 June 2020
--  Based on gltext example Simple

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Hello is
   Main_Window : Glfw.Windows.Window;
   Window_Title : constant String := "Simple GL Text Example";
begin
   Glfw.Init;
   Initialize (Main_Window, Window_Title);
   Main_Loop (Main_Window);
   Glfw.Shutdown;

exception
   when anError : Constraint_Error =>
      Put ("Hello returned constraint error: ");
      Put_Line (Exception_Information (anError));

   when anError :  others =>
      Put_Line ("An exception occurred in Hello.");
      Put_Line (Exception_Information (anError));
end Hello;
