
with Input_Callback;

package Input_Manager is

   type Button_Index is (Pause_Button, Resume_Button, Play_Button, Menu_Button,
                         Credits_Button, Exit_Button);
    type Command is (Command_Left, Command_Right, Command_Stop, Command_Up,
                     Command_Down, Command_UI, Command_Invalid);

    function Get_Current_Command return Command;
    function Is_Active (Button : Button_Index) return Boolean;
    function Is_Clicked (Button : Button_Index) return Boolean;
    function Is_Visible (Button : Button_Index) return Boolean;
    procedure Init_Buttons;
    procedure Render_Button (Button : Button_Index);
    procedure Set_Active (Button : Button_Index; State : Boolean);
    procedure Set_Clicked (Button : Button_Index; Clicked : Boolean);
    procedure Set_Command_Invalid;
    procedure Set_Visible (Button : Button_Index; State : Boolean);
    procedure Update (Delta_Time : Float);
    procedure Update (Button : Button_Index; Delta_Time : Float);
    procedure Update_Command (Window : in out Input_Callback.Callback_Window);

end Input_Manager;
