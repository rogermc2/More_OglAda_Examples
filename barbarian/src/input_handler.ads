
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glfw.Windows;
with Glfw.Input.Keys; use Glfw.Input.Keys;

with Input_Callback;

with GL_Maths;

package Input_Handler is

   Max_Actions : constant Integer := 256;

   type Key_Characters is array (Integer range <>, Integer range <>) of Character;
   type Key_Binding_State is array (1 .. Max_Actions) of Key;

   Input_Handler_Exception : Exception;

   function Action_Name (Index: Integer) return String;
   function Attack_Action return Integer;
   function Clear_Binding_Action return Integer;
   function Down_Action return Integer;
   procedure Init (Window : in out Input_Callback.Barbarian_Window);
   function Is_Action_Down (Action : Integer) return Boolean;
   function Joy_Axis_Bindings (Index: Integer) return Integer;
   function Joy_Axis_Sign (Index: Integer) return Character;
   function Joy_Button_Bindings (Index: Integer) return Integer;
   function Key_Binding (Index: Integer) return Integer;
   function Left_Action return Integer;
   function Menu_Back_Action return Integer;
   function OK_Action return Integer;
   function Menu_Open_Action return Integer;
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
   function Up_Action return Integer;
   function Was_Action_Pressed (Window : in out Input_Callback.Barbarian_Window;
                                Action_ID : Integer) return Boolean;

private

   type Input_Actions_Data is record
      -- Names of game's registered actions. "jump" "shoot" etc.
      Action_Names          : Input_Callback.Key_String (1 .. Max_Actions);
      Num_Actions           : Integer := 0;
      Left_Action_ID        : Integer := 0;
      Right_Action_ID       : Integer := 0;
      Up_Action_ID          : Integer := 0;
      Down_Action_ID        : Integer := 0;
      Attack_Action_ID      : Integer := 0;
      Open_Door_Action_ID   : Integer := 0;
      Wipe_Screen_Action_ID : Integer := 0;
      Sel_Sword_Action_ID   : Integer := 0;
      Sel_Javelin_Action_ID : Integer := 0;
      Sel_Hammer_Action_ID  : Integer := 0;
      Cycle_Weapons_Action_ID : Integer := 0;
      Open_Menu_Action_ID    : Integer := 0;
      Menu_Back_Action_ID   : Integer := 0;
      Ok_Action_ID         : Integer := 0;
      Clear_Binding_Action_ID : Integer := 0;
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
