
package Input_Manager is
   type Command is (Command_None, Command_Left, Command_Right, Command_Stop, Command_Up,
                    Command_Down, Command_GUI, Command_Quit, Command_Invalid);

   function Get_Current_Command return Command;
   procedure Set_Command_Invalid;
   procedure Set_Command_None;
   procedure Update_Command;

end Input_Manager;
