
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glfw.Windows;
with Glfw.Input.Keys; use Glfw.Input.Keys;

with GL_Maths;

package Input_Handler is

   Max_Actions : constant Integer := 256;
   Max_Keys    : constant Integer := 348;

   type Joystick_Axes_State is array (Integer range <>) of Float;
   type Joystick_State  is array (Integer range <>) of Boolean;
   type Key_String is array (Integer range <>) of Unbounded_String;
   type Key_Characters is array (Integer range <>, Integer range <>) of Character;
   type Key_State is array (1 .. Max_Keys) of Boolean;
   type Key_Binding_State is array (1 .. Max_Actions) of Key;

   Input_Handler_Exception : Exception;

   function Action_Name (Index: Integer) return String;
   function Attack_Action return Integer;
   function Clear_Binding_Action return Integer;
   function Down_Action return Integer;
   procedure Init (Window  : in out Glfw.Windows.Window);
   function Is_Key_Down (aKey : Key) return Boolean;
   function Is_Action_Down (Action : Integer) return Boolean;
   function Joy_Axis_Bindings (Index: Integer) return Integer;
   function Joy_Axis_Sign (Index: Integer) return Character;
   function Joy_Button_Bindings (Index: Integer) return Integer;
   function Joystick_Connected return Boolean;
   function Key_Binding (Index: Integer) return Integer;
   function Key_Name (Index: Integer) return Unbounded_String;
   function Key_Pressed return Boolean;
   function Last_Key_Down return Key;
   function Left_Action return Integer;
   procedure Lock_All_Keys;
   function Menu_Back_Action return Integer;
   function OK_Action return Integer;
   function Open_Menu_Action return Integer;
   function Num_Actions return Integer;
   procedure Read_Key_Config (aLine : String);
   procedure Register_Input_Actions;
   function Right_Action return Integer;
   procedure Set_Joy_Axis_For_Action (Action_Name : String; Key_Code : Natural;
                                      Sign        : Character);
   procedure Set_Joy_Button_For_Action (Action_Name : String; Key_Code : Natural);
   procedure Set_Key_For_Action (Action_Name : String; Key_Code : Natural);
   function Set_Key_For_Action (Action_Name : String; Key_Code : Natural)
                                return Integer;
   procedure Set_Key_Pressed (Pressed : Boolean);
   function Up_Action return Integer;
   function Was_Action_Pressed (Window : in out Glfw.Windows.Window;
                                Action : Integer) return Boolean;
   function Was_Joy_Y_Pressed return Boolean;
   function Was_Key_Pressed (Window : in out Glfw.Windows.Window; aKey : Key)
                             return Boolean;
private
   type Input_State_Data is record
      -- localised name of each key - not supporting wchar_t to protect 256 sz atlas
      Key_Names                      : Key_String (1 .. Max_Keys) :=
                                         (others => To_Unbounded_String (""));
      Last_Key_Down                  : Key := Unknown;
      Key_Pressed                    : Boolean := False;
      Keys_Down                      : Key_State := (others => False);
      Keys_Locked                    : Key_State := (others => False);
      --  Joystick status
      Num_Connected_Joysticks        : Integer := 0;
      Joystick_Axes                  : Joystick_Axes_State (1 .. 8) :=
                                         (others => 0.0);
      Num_Joy_Axes                   : Integer := 0;
      Num_Joy_Buttons                : Integer := 0;
      Joystick_Axis_Locked           : Joystick_State (1 .. 8) :=
                                         (others => False);
      Joystick_Axis_Locked_Sign      : GL_Maths.Character_Array (1 .. 8);
      Joystick_Buttons_Locked        : Joystick_State (1 .. 32) :=
                                         (others => False);
      Joystick_Buttons               : GL_Maths.Integer_Array (1 .. 32) :=
                                         (others => 0);
      Joystick_Connected             : Boolean := False;
      --  Steam needs this or the overlay falls to bits
      Mouse_Cursor_Hidden            : Boolean := False;
   end record;

   type Input_Actions_Data is record
      -- Names of game's registered actions. "jump" "shoot" etc.
      Action_Names          : Key_String (1 .. Max_Actions);
      Num_Actions           : Integer := 0;
      Left_Action           : Integer := 0;
      Right_Action          : Integer := 0;
      Up_Action             : Integer := 0;
      Down_Action           : Integer := 0;
      Attack_Action         : Integer := 0;
      Open_Door_Action      : Integer := 0;
      Wipe_Screen_Action    : Integer := 0;
      Sel_Sword_Action      : Integer := 0;
      Sel_Javelin_Action    : Integer := 0;
      Sel_Hammer_Action     : Integer := 0;
      Cycle_Weapons_Action  : Integer := 0;
      Open_Menu_Action      : Integer := 0;
      Menu_Back_Action      : Integer := 0;
      Ok_Action             : Integer := 0;
      Clear_Binding_Action  : Integer := 0;
      -- Actual key code for each registered action ingame
      Key_Bindings          : Key_Binding_State;
      Joy_Button_Bindings   : GL_Maths.Integer_Array (1 .. Max_Actions) :=
                                (others => 0);
      Joy_Axis_Bindings     : GL_Maths.Integer_Array (1 .. Max_Actions) :=
                                (others => 0);
      Joy_Axis_Sign         : GL_Maths.Character_Array (1 .. Max_Actions) :=
                                (others => ' ');
   end record;

end Input_Handler;
