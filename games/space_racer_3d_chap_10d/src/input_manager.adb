
--  with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input.Keys;

with Input_Callback;

package body Input_Manager is

   Current_Command : Command := Command_None;

   --  ------------------------------------------------------------------------

   function Get_Current_Command return Command is
   begin
      return Current_Command;
   end Get_Current_Command;

   --  ------------------------------------------------------------------------

   procedure Set_Command_Invalid is
   begin
      Current_Command := Command_Invalid;
   end Set_Command_Invalid;

   --  ------------------------------------------------------------------------

   procedure Set_Command_None is
   begin
      Current_Command := Command_None;
   end Set_Command_None;

   --  ------------------------------------------------------------------------

   procedure Update_Command is
      use Glfw.Input.Keys;
      use Input_Callback;

   begin
      if Key_Pressed and then Current_Command /= Command_GUI then
         if Is_Key_Down (Q) then
            Current_Command := Command_Quit;
         elsif Is_Key_Down (Left) or Is_Key_Down (A) then
            Current_Command := Command_Left;
         elsif Is_Key_Down (Right) or Is_Key_Down (D) then
            Current_Command := Command_Right;
         elsif Is_Key_Down (Up)  or Is_Key_Down (W) then
            Current_Command := Command_Up;
         elsif Is_Key_Down (Down) or Is_Key_Down (S) then
            Current_Command := Command_Down;
         elsif Is_Key_Down (Space) then
            Current_Command := Command_Stop;
         else
            Current_Command := Command_Invalid;
         end if;
      else
         Current_Command := Command_None;
      end if;

   end Update_Command;

   --  ------------------------------------------------------------------------

end Input_Manager;
