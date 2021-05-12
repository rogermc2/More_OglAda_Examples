
--  with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input.Keys;

with Input_Callback;

package body Input_Manager is

    Current_Command : Command;

    --  ------------------------------------------------------------------------

    function Get_Command return Command is
    begin
        return Current_Command;
    end Get_Command;

    --  ------------------------------------------------------------------------

    procedure Update is
        use Glfw.Input.Keys;
        use Input_Callback;
    begin
         if Is_Key_Down (Left) or Is_Key_Down (A) then
            Current_Command := Command_Left;
         elsif Is_Key_Down (Right) or Is_Key_Down (D) then
            Current_Command := Command_Right;
         elsif Is_Key_Down (Up) then
            Current_Command := Command_Up;
         elsif Is_Key_Down (Down) then
            Current_Command := Command_Down;
         else
            Current_Command := Command_Stop;
         end if;
--        Put_Line ("Input_Manager.Update Command: " &
--                    Command'Image (Current_Command));
    end Update;

    --  ------------------------------------------------------------------------

end Input_Manager;
