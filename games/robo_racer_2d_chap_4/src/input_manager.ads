
with Input_Callback;

package Input_Manager is

    type Command is (Command_Left, Command_Right, Command_Stop, Command_Up,
                    Command_Down, Command_Quit);

    function Get_Command return Command;
    procedure Update (Window : in out Input_Callback.Callback_Window;
                      Delta_Time : Float);

end Input_Manager;
