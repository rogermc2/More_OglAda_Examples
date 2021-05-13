
package Input_Manager is

    type Command is (Command_Left, Command_Right, Command_Stop, Command_Up,
                     Command_Down);

    function Get_Command return Command;
    procedure Update_Command;

end Input_Manager;
