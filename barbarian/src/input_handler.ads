
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glfw.Windows;
with Glfw.Input.Keys; use Glfw.Input.Keys;

with Input_Callback;

with GL_Maths;

package Input_Handler is

   Max_Actions : constant Integer := 256;
   subtype Action_Range is Integer range 1 .. Max_Actions;

   type Key_Characters is array (Integer range <>, Integer range <>) of Character;
   type Key_Binding_State is array (Action_Range) of Key;

   Input_Handler_Exception : Exception;

   function Action_Name (Index: Action_Range) return String;
   function Attack_Action return Integer;
   function Clear_Binding_Action return Integer;
   function Cycle_Weapons_Action return Integer;
   function Down_Action return Integer;
   function Hammer_Action return Integer;
   procedure Init (Window : in out Input_Callback.Callback_Window);
   function Is_Action_Down (Action : Action_Range) return Boolean;
   function Javelin_Action return Integer;
   function Joy_Axis_Bindings (Index: Action_Range) return Integer;
   function Joy_Axis_Sign (Index: Action_Range) return Character;
   function Joy_Button_Bindings (Index: Action_Range) return Integer;
   function Key_Binding (Index: Action_Range) return Integer;
   function Left_Action return Integer;
   function Menu_Back_Action return Integer;
   function Menu_Open_Action return Integer;
   function Num_Actions return Integer;
   function OK_Action return Integer;
   function Open_Door_Action return Integer;
   procedure Read_Key_Config (aLine : String);
   procedure Register_Input_Actions;
   function Right_Action return Integer;
   procedure Set_Joy_Axis_For_Action (Action_Name : String; Key_Code : Natural;
                                      Sign        : Character);
   procedure Set_Joy_Button_For_Action (Action_Name : String; Key_Code : Natural);
   procedure Set_Key_For_Action (Action_Name : String; Key_Code : Natural);
   function Set_Key_For_Action (Action_Name : String; Key_Code : Natural)
                                return Integer;
   function Sword_Action return Integer;
   function Up_Action return Integer;
   function Was_Action_Pressed (Window : in out Input_Callback.Callback_Window;
                                Action_ID : Action_Range) return Boolean;
   function Wipe_Screen_Action return Integer;

private

   type Input_Actions_Data is record
      -- Names of game's registered actions. "jump" "shoot" etc.
      Action_Names            : Input_Callback.Key_String (Action_Range);
      Num_Actions             : Integer := 0;
      Left_Action_ID          : Action_Range;
      Right_Action_ID         : Action_Range;
      Up_Action_ID            : Action_Range;
      Down_Action_ID          : Action_Range;
      Attack_Action_ID        : Action_Range;
      Open_Door_Action_ID     : Action_Range;
      Wipe_Screen_Action_ID   : Action_Range;
      Sel_Sword_Action_ID     : Action_Range;
      Sel_Javelin_Action_ID   : Action_Range;
      Sel_Hammer_Action_ID    : Action_Range;
      Cycle_Weapons_Action_ID : Action_Range;
      Open_Menu_Action_ID     : Action_Range;
      Menu_Back_Action_ID     : Action_Range;
      Ok_Action_ID            : Action_Range;
      Clear_Binding_Action_ID : Action_Range;
      -- Actual key code for each registered action ingame
      Key_Bindings            : Key_Binding_State;
      Joy_Button_Bindings     : GL_Maths.Integer_Array (Action_Range) :=
                                  (others => 0);
      Joy_Axis_Bindings       : GL_Maths.Integer_Array (Action_Range) :=
                                  (others => 0);
      Joy_Axis_Sign           : GL_Maths.Character_Array (Action_Range) :=
                                  (others => ' ');
   end record;

end Input_Handler;
