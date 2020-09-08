
with Game_Utils;

package body Input_Handler is

    Input_Actions : Input_Actions_Data;
    Input_State   : Input_State_Data;

    procedure Set_Joy_Axis_For_Action (Action_Name : String; Key_Code : Integer;
                                      Sign : Character);
    procedure Set_Joy_Button_For_Action (Action_Name : String; Key_Code : Integer);
    procedure Set_Key_For_Action (Action_Name : String; Key_Code : Integer);

    --  ------------------------------------------------------------------------

    procedure Default_Key_Configuration is
    begin
        Set_Key_For_Action ("Move_Left", 263);
        Set_Key_For_Action ("Move_Right", 262);
        Set_Key_For_Action ("Move_Fwd", 265);
        Set_Key_For_Action ("Move_Bk", 264);
        Set_Key_For_Action ("Attack", 81);
        Set_Key_For_Action ("Open_Door/Buy", 87);
        Set_Key_For_Action ("Wipe_Screen", 69);
        Set_Key_For_Action ("Select_Sword", 49);
        Set_Key_For_Action ("Select_Javelin", 50);
        Set_Key_For_Action ("Select_Hammer", 51);
        Set_Key_For_Action ("Cycle_Weapons", 84); -- T
        Set_Key_For_Action ("Open_Menu", 256);
        Set_Key_For_Action ("Menu_Back", 256);
        Set_Key_For_Action ("Ok", 257);
        Set_Key_For_Action ("Clear_Binding", 259);
        Set_Joy_Button_For_Action ("Move_Left", 13); -- Dpad left on Xbox
        Set_Joy_Button_For_Action ("Move_Right", 11);
        Set_Joy_Button_For_Action ("Move_Fwd", 10);
        Set_Joy_Button_For_Action ("Move_Bk", 12);
        Set_Joy_Button_For_Action ("Attack", 0); -- A
        Set_Joy_Button_For_Action ("Open_Door/Buy", 1); -- B
        Set_Joy_Button_For_Action ("Wipe_Screen", 2); -- X
        Set_Joy_Button_For_Action ("Select_Sword", 4); -- Lshldr
        Set_Joy_Button_For_Action ("Select_Javelin", 5); -- Rshdr
        Set_Joy_Button_For_Action ("Select_Hammer", -1); -- Invalid/Nothing
        Set_Joy_Button_For_Action ("Cycle_Weapons", 3); -- Y
        Set_Joy_Button_For_Action ("Open_Menu", 6); -- B
        Set_Joy_Button_For_Action ("Menu_Back", 1); -- <
        Set_Joy_Button_For_Action ("Ok", 7); -- Start
        Set_Joy_Button_For_Action ("Clear_Binding", 3); -- Start
        Set_Joy_Axis_For_Action ("Move_Left", 0, '-');
        Set_Joy_Axis_For_Action ("Move_Right", 0, '+');
        Set_Joy_Axis_For_Action ("Move_Fwd", 1, '-');
        Set_Joy_Axis_For_Action ("Move_Bk", 1, '+');
        Set_Joy_Axis_For_Action ("Attack", 2, '-');
        Set_Joy_Axis_For_Action ("Open_Door/Buy", 2, '+');
        Set_Joy_Axis_For_Action ("Select_Sword", 4, '-');
        Set_Joy_Axis_For_Action ("Select_Javelin", 3, '-');
        Set_Joy_Axis_For_Action ("Select_Hammer", 4, '+');
        --Set_Joy_Axis_For_Action ("Attack", 1, '+'); -- Right Trigger
    end Default_Key_Configuration;

    --  ------------------------------------------------------------------------

    procedure Init is
--          Hide_Cursor : Boolean := True;
    begin
        Game_Utils.Game_Log ("--- Init Input Handler ---");
--          glfwSetInputMode (g_window, GLFW_CURSOR, GLFW_CURSOR_HIDDEN);
	Input_State.Mouse_Cursor_Hidden := True;
    end Init;

    --  ------------------------------------------------------------------------

    function Is_Action_Down (Action : Integer) return Boolean is
    begin
        --  Joystick processing
        return Is_Key_Down (Input_Actions.Key_Bindings (Action));
    end Is_Action_Down;

    --  ------------------------------------------------------------------------

    function Is_Key_Down (aKey : Key) return Boolean is
        Key_Val : constant Integer := Key'Enum_Rep (aKey);
    begin
        if Key_Val < 0 or Key_Val >= Max_Keys then
            raise Input_Handler_Exception with
              "Input_Handler.Is_Key_Down, invalid key code " &
              Integer'Image (Key_Val) & " detected.";
        end if;
        return Input_State.Keys_Down (Key_Val);
    end Is_Key_Down;

    --  ------------------------------------------------------------------------

    procedure Set_Key_For_Action (Action_Name : String; Key_Code : Integer) is
    begin
        null;
    end Set_Key_For_Action;

    --  ------------------------------------------------------------------------

    procedure Set_Joy_Axis_For_Action (Action_Name : String; Key_Code : Integer;
                                       Sign : Character) is
    begin
        null;
    end Set_Joy_Axis_For_Action;

    --  ------------------------------------------------------------------------

    procedure Set_Joy_Button_For_Action (Action_Name : String; Key_Code : Integer) is
    begin
        null;
    end Set_Joy_Button_For_Action;

    --  ------------------------------------------------------------------------

    function Was_Attack_Action_Pressed return Boolean is
    begin
        return Input_Actions.Attack_Action /= 0;
    end Was_Attack_Action_Pressed;

    --  ------------------------------------------------------------------------

    function Was_Key_Pressed (aKey : Key) return Boolean is
        Key_Val : constant Integer := Key'Enum_Rep (aKey);
        Pressed : Boolean := False;
    begin
        if Key_Val < 0 or Key_Val >= Max_Keys then
            raise Input_Handler_Exception with
              "Input_Handler.Was_Key_Pressed, invalid key code " &
              Integer'Image (Key_Val) & " detected.";
        end if;
        Pressed := Input_State.Keys_Down (Key_Val) and
          not Input_State.Keys_Locked (Key_Val);
        if Pressed then
            Input_State.Keys_Locked (Key_Val) := True;
        end if;
        return Pressed;
    end Was_Key_Pressed;

    --  ------------------------------------------------------------------------

    function Was_Menu_Back_Action_Pressed return Boolean is
    begin
        return Input_Actions.Menu_Back_Action /= 0;
    end Was_Menu_Back_Action_Pressed;

    --  ------------------------------------------------------------------------

    function Was_OK_Action_Pressed return Boolean is
    begin
        return Input_Actions.Ok_Action /= 0;
    end Was_OK_Action_Pressed;

    --  ------------------------------------------------------------------------

    function Was_Open_Menu_Action_Pressed return Boolean is
    begin
        return Input_Actions.Open_Menu_Action /= 0;
    end Was_Open_Menu_Action_Pressed;

    --  ------------------------------------------------------------------------

end Input_Handler;
