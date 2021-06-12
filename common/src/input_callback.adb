
package body Input_Callback is
    use Glfw.Input.Keys;

    type Key_State is array (Key'Range) of Boolean;
    type Button_State is array (Glfw.Input.Mouse.Button'Range) of Boolean;
    type Joystick_Axes_State is array (Integer range <>) of Float;
    type Joystick_State  is array (Integer range <>) of Boolean;
    type Character_Array is array (Integer range <>) of Character;
    type Integer_Array is array (Integer range <>) of Integer;

    type Input_State_Data is record
    --  localised name of each key - not supporting wchar_t to protect 256 sz atlas
        Key_Names                      : Key_String (1 .. Max_Keys) :=
                                           (others => To_Unbounded_String (""));
        Last_Key_Down                  : Key := Unknown;
        Key_Pressed                    : Boolean := False;
        Keys_Down                      : Key_State := (others => False);
        Keys_Locked                    : Key_State := (others => False);
        Button_Pressed                 : Boolean := False;
        Button_Down                    : Button_State := (others => False);
        Button_Locked                  : Button_State := (others => False);
        Last_Button_Down               : Glfw.Input.Mouse.Button;
        --  Joystick status
        Num_Connected_Joysticks        : Integer := 0;
        Joystick_Axes                  : Joystick_Axes_State (1 .. 8) :=
                                           (others => 0.0);
        Num_Joy_Axes                   : Integer := 0;
        Num_Joy_Buttons                : Integer := 0;
        Joystick_Axis_Locked           : Joystick_State (1 .. 8) :=
                                           (others => False);
        Joystick_Axis_Locked_Sign      : Character_Array (1 .. 8);
        Joystick_Buttons_Locked        : Joystick_State (1 .. 32) :=
                                           (others => False);
        Joystick_Buttons               : Integer_Array (1 .. 32) :=
                                           (others => 0);
        Joystick_Connected             : Boolean := False;
        Mouse_Cursor_Hidden            : Boolean := True;
    end record;

    Input_State : Input_State_Data;

    --  see http://flyx.github.io/OpenGLAda/glfw-v3.html for handling
    --  Glfw callbacks

    --  ------------------------------------------------------------------------

    procedure Mouse_Button_Changed (Object  : not null access Callback_Window;
                                    Button  : Glfw.Input.Mouse.Button;
                                    State   : Glfw.Input.Button_State;
                                    Mods    : Glfw.Input.Keys.Modifiers) is
        use Glfw.Input;
    begin
        if State = Pressed then
            if not Input_State.Button_Locked (Button) then
                Input_State.Button_Down (Button) := True;
                Input_State.Button_Pressed := True;
                Input_State.Button_Locked (Button) := True;
                Input_State.Last_Button_Down := Button;
            end if;
        elsif State = Released then
            Input_State.Button_Down (Button) := False;
            Input_State.Button_Locked (Button) := False;
        end if;

    end Mouse_Button_Changed;

    --  ------------------------------------------------------------------------

    procedure Clear_All_Keys is
    begin
        for index in Input_State.Keys_Down'Range loop
            Input_State.Keys_Down (index) := False;
        end loop;
    end Clear_All_Keys;

    --  ------------------------------------------------------------------------

    function Is_Button_Down (aButton : Glfw.Input.Mouse.Button) return Boolean is
    begin
        --          Put_Line ("Input_Callback.Is_Button_Down " &
        --                      Glfw.Input.Mouse.Button'Image (aButton)
        --                    & Boolean'Image (Input_State.Button_Down (1)));
        return Input_State.Button_Down (aButton);
    end Is_Button_Down;

    --  ------------------------------------------------------------------------

    function Is_Key_Down (aKey : Key) return Boolean is
    begin
        return Input_State.Keys_Down (aKey);
    end Is_Key_Down;

    --  ------------------------------------------------------------------------

    function Joystick_Connected return Boolean is
    begin
        return Input_State.Joystick_Connected;
    end Joystick_Connected;

    --  ------------------------------------------------------------------------

    procedure Key_Changed (Object   : not null access Callback_Window;
                           Key      : Glfw.Input.Keys.Key;
                           Scancode : Glfw.Input.Keys.Scancode;
                           Action   : Glfw.Input.Keys.Action;
                           Mods     : Glfw.Input.Keys.Modifiers) is
        use type Glfw.Input.Keys.Key;
        use type Glfw.Input.Keys.Action;
    begin
        if Key = Glfw.Input.Keys.Escape then
            Object.Set_Should_Close (True);
        end if;
        if Action = Press then
            if not Input_State.Keys_Locked (Key) then
                Input_State.Keys_Down (Key) := True;
                Input_State.Key_Pressed := True;
                Input_State.Keys_Locked (Key) := True;
                Input_State.Last_Key_Down := Key;
            end if;
        elsif Action = Release then
            Input_State.Keys_Down (Key) := False;
            Input_State.Keys_Locked (Key) := False;
        end if;

    end Key_Changed;

    --  ------------------------------------------------------------------------

    function Key_Name (Index : Integer) return Unbounded_String is
    begin
        return Input_State.Key_Names (Index);
    end Key_Name;

    --  ------------------------------------------------------------------------

    function Key_Pressed return Boolean is
    begin
        return Input_State.Key_Pressed;
    end Key_Pressed;

    --  ------------------------------------------------------------------------

    function Last_Key_Down return Key is
    begin
        return Input_State.Last_Key_Down;
    end Last_Key_Down;

    --  ------------------------------------------------------------------------

    procedure Lock_All_Keys is
    begin
        for index in Input_State.Keys_Down'Range loop
            if Input_State.Keys_Down (index) then
                Input_State.Keys_Locked (index) := True;
            end if;
        end loop;
    end Lock_All_Keys;

    --  ------------------------------------------------------------------------

    procedure Set_Key_Pressed (Pressed : Boolean) is
    begin
        Input_State.Key_Pressed := Pressed;
    end Set_Key_Pressed;

    --  ------------------------------------------------------------------------
    --
    --     function Was_Joy_Y_Pressed return Boolean is
    --        Result : Boolean := False;
    --     begin
    --        Result := Input_State.Joystick_Connected and then
    --          Input_State.Joystick_Buttons (3) > 0 and then
    --          not Input_State.Joystick_Buttons_Locked (3);
    --
    --        if Result then
    --           Input_State.Joystick_Buttons_Locked (3) := True;
    --        end if;
    --
    --        return Result;
    --     end Was_Joy_Y_Pressed;

    --  ------------------------------------------------------------------------
    --
    --     function Was_Key_Pressed (Window : in out Callback_Window;
    --                               aKey   : Key) return Boolean is
    --        use Glfw.Input;
    --        Key_Pressed : constant Boolean :=
    --                        Window'Access.Key_State (aKey) = Pressed and
    --                          not Input_State.Keys_Locked (aKey);
    --     begin
    --        if Key_Pressed then
    --           Input_State.Keys_Locked (aKey) := True;
    --        end if;
    --        return Key_Pressed;
    --     end Was_Key_Pressed;

    --  ------------------------------------------------------------------------

end Input_Callback;
