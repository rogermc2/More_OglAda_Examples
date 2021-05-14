
with Input_Callback;
with Sprite_Manager;

package Input_Manager is

    type Command is (Command_Left, Command_Right, Command_Stop, Command_Up,
                     Command_Down, Command_UI, Command_Invalid);

    procedure Add_UI_Element (Element : Sprite_Manager.Sprite);
    function Get_Current_Command return Command;
    procedure Set_Command_Invalid;
    procedure Update_Command (Window : in out Input_Callback.Callback_Window);

end Input_Manager;
