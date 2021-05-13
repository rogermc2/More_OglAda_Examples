
with Input_Callback;

package Input_Manager is

    type Command is (Command_Left, Command_Right, Command_Stop, Command_Up,
                     Command_Down, Command_UI);

    function Get_Command return Command;
    procedure Update_Command (Window : in out Input_Callback.Callback_Window);

end Input_Manager;
