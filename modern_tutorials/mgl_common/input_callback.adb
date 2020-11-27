
with Ada.Text_IO; use Ada.Text_IO;

package body Input_Callback is
   use Glfw.Input.Keys;

   type Key_State is array (Key'Range) of Boolean;

   type Input_State_Data is record
      -- localised name of each key - not supporting wchar_t to protect 256 sz atlas
      Key_Names                      : Key_String (1 .. Max_Keys) :=
                                         (others => To_Unbounded_String (""));
      Last_Key_Down                  : Key := Unknown;
      Key_Pressed                    : Boolean := False;
      Keys_Down                      : Key_State := (others => False);
      Keys_Locked                    : Key_State := (others => False);
      Mouse_Cursor_Hidden            : Boolean := True;
   end record;

   Input_State : Input_State_Data;

   --  see http://flyx.github.io/OpenGLAda/glfw-v3.html for handling
   --  Glfw callbacks

   --  ------------------------------------------------------------------------

   function Is_Key_Down (aKey : Key) return Boolean is
   begin
      return Input_State.Keys_Down (aKey);
   end Is_Key_Down;

   --  ------------------------------------------------------------------------

   procedure Key_Changed (Object   : not null access Callback_Window;
                          Key      : Glfw.Input.Keys.Key;
                          Scancode : Glfw.Input.Keys.Scancode;
                          Action   : Glfw.Input.Keys.Action;
                          Mods     : Glfw.Input.Keys.Modifiers) is
      use type Glfw.Input.Keys.Key;
      use type Glfw.Input.Keys.Action;
   begin
      If Key = Glfw.Input.Keys.Escape then
         Object.Set_Should_Close (True);
      end if;
      if Action = Press then
         if not Input_State.Keys_Locked (Key) then
            Input_State.Keys_Down (Key) := True;
            Input_State.Key_Pressed := True;
            Input_State.Last_Key_Down := Key;
         end if;
      elsif Action = Release then
         Input_State.Keys_Down (Key) := False;
         Input_State.Keys_Locked (Key) := False;
      end if;

   end Key_Changed;

   --  ------------------------------------------------------------------------

   function Key_Name (Index: Integer) return Unbounded_String is
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

   function Was_Key_Pressed (Window : in out Callback_Window;
                             aKey   : Key) return Boolean is
      use Glfw.Input;
      Key_Pressed : constant Boolean :=
                      Window'Access.Key_State (aKey) = Pressed and
                        not Input_State.Keys_Locked (aKey);
   begin
      if Key_Pressed then
         Input_State.Keys_Locked (aKey) := True;
      end if;
      return Key_Pressed;
   end Was_Key_Pressed;

   --  ------------------------------------------------------------------------

end Input_Callback;
