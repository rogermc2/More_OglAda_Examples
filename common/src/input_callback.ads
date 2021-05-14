
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glfw;
with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows;

package Input_Callback is

   --  see http://flyx.github.io/OpenGLAda/glfw-v3.html for handling
   --  Glfw callbacks
   type Callback_Window is new Glfw.Windows.Window with null record;
   type Key_String is array (Integer range <>) of Unbounded_String;

   Max_Keys : constant Integer := 348;

   Input_Callback_Exception : Exception;

   overriding
   procedure Mouse_Button_Changed (Object   : not null access Callback_Window;
                                   Button   : Glfw.Input.Mouse.Button;
                                   State   : Glfw.Input.Button_State;
                                   Mods    : Glfw.Input.Keys.Modifiers);
   procedure Clear_All_Keys;
   function Is_Key_Down (aKey : Glfw.Input.Keys.Key) return Boolean;
   function Is_Button_Down (aButton : Glfw.Input.Mouse.Button) return Boolean;
   function Joystick_Connected return Boolean;

   overriding
   procedure Key_Changed (Object   : not null access Callback_Window;
                          Key      : Glfw.Input.Keys.Key;
                          Scancode : Glfw.Input.Keys.Scancode;
                          Action   : Glfw.Input.Keys.Action;
                          Mods     : Glfw.Input.Keys.Modifiers);

   function Key_Name (Index : Integer) return Unbounded_String;
   function Key_Pressed return Boolean;
   function Last_Key_Down return Glfw.Input.Keys.Key;
   procedure Lock_All_Keys;
   procedure Set_Key_Pressed (Pressed : Boolean);
--     function Was_Joy_Y_Pressed return Boolean;
--     function Was_Key_Pressed (Window : in out Callback_Window;
--                               aKey   : Glfw.Input.Keys.Key) return Boolean;

end Input_Callback;
