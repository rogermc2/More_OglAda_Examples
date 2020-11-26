
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glfw.Windows;
with Glfw.Input.Keys; use Glfw.Input.Keys;

with Input_Callback;

package Input_Handler is

   Max_Actions : constant Integer := 256;

   type Key_Characters is array (Integer range <>, Integer range <>) of Character;
   type Key_Binding_State is array (1 .. Max_Actions) of Key;

   Input_Handler_Exception : Exception;

   function Action_Name (Index: Integer) return String;
   function Clear_Binding_Action return Integer;
   function Down_Action return Integer;
   procedure Init (Window : in out Input_Callback.Callback_Window);
   function Is_Action_Down (Action : Integer) return Boolean;
   function Key_Binding (Index: Integer) return Integer;
   function Left_Action return Integer;
   function OK_Action return Integer;
   function Num_Actions return Integer;
   procedure Read_Key_Config (aLine : String);
   procedure Register_Input_Actions;
   function Right_Action return Integer;
   procedure Set_Key_For_Action (Action_Name : String; Key_Code : Natural);
   function Set_Key_For_Action (Action_Name : String; Key_Code : Natural)
                                return Integer;
   function Up_Action return Integer;
   function Was_Action_Pressed (Window : in out Input_Callback.Callback_Window;
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
      Ok_Action_ID         : Integer := 0;
      Clear_Binding_Action_ID : Integer := 0;
      -- Actual key code for each registered action ingame
      Key_Bindings          : Key_Binding_State;
   end record;

end Input_Handler;
