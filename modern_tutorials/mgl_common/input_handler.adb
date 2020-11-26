
with Glfw.Input.Mouse;

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

package body Input_Handler is

   Input_Actions : Input_Actions_Data;

   function Register_Key_Action (Name : String) return Integer;

   --  ------------------------------------------------------------------------

   function Action_Name (Index: Integer) return String is
   begin
      return To_String (Input_Actions.Action_Names (Index));
   end Action_Name;

   --  ------------------------------------------------------------------------

   function Clear_Binding_Action return Integer is
   begin
      return Input_Actions.Clear_Binding_Action_ID;
   end Clear_Binding_Action;

   --  ------------------------------------------------------------------------

   function Down_Action return Integer is
   begin
      return Input_Actions.Down_Action_ID;
   end Down_Action;

   --  ------------------------------------------------------------------------

   procedure Default_Key_Configuration is
   begin
      Set_Key_For_Action ("Move_Left", 263);
      Set_Key_For_Action ("Move_Right", 262);
      Set_Key_For_Action ("Move_Fwd", 265);
      Set_Key_For_Action ("Move_Bk", 264);
      Set_Key_For_Action ("Ok", 257);
      Set_Key_For_Action ("Clear_Binding", 259);
   end Default_Key_Configuration;

   --  ------------------------------------------------------------------------

   procedure Init (Window  : in out Input_Callback.Callback_Window) is
      --          Hide_Cursor : Boolean := True;
   begin
      Window.Set_Cursor_Mode (Glfw.Input.Mouse.Hidden);
   end Init;

   --  ------------------------------------------------------------------------

   function Is_Action_Down (Action : Integer) return Boolean is
   begin
      --  Joystick processing
      return Input_Callback.Is_Key_Down (Input_Actions.Key_Bindings (Action));
   end Is_Action_Down;

   --  ------------------------------------------------------------------------

   function Key_Binding (Index: Integer) return Integer is
   begin
      return Key'Enum_Rep (Input_Actions.Key_Bindings (Index));
   end Key_Binding;

   --  ------------------------------------------------------------------------

   function Left_Action return Integer is
   begin
      return Input_Actions.Left_Action_ID;
   end Left_Action;

   --  ------------------------------------------------------------------------

   function Num_Actions return Integer is
   begin
      return Input_Actions.Num_Actions;
   end Num_Actions;

   --  ------------------------------------------------------------------------

   function OK_Action return Integer is
   begin
      return Input_Actions.Ok_Action_ID;
   end OK_Action;

   --  ------------------------------------------------------------------------

   procedure Read_Key_Config (aLine : String) is
      use Ada.Strings;
      Start   : constant Positive := aLine'First;
      Last    : constant Positive := aLine'Last;
      Pos1    : Positive := Fixed.Index (aLine, "_");
      Pos2    : constant Positive := Fixed.Index (aLine, " ");
      Code    : Integer;
      Sign    : Character := '+';
      Axis    : Integer := 0;
      Index   : Integer := 0;
      J_Index : Integer := -1;
      Found   : Boolean := False;
   begin
      if Last < 1 then
         raise Input_Handler_Exception with
           "Read_Key_Config called with empty string.";
      end if;

      if aLine (Start) = 'K' then
         declare
            Action : constant String := aLine (Start .. Pos2 - 1);
         begin
            Code := Integer'Value (aLine (Pos2 + 1 .. Last));
            Set_Key_For_Action (Action, Code);
         end;
      end if;

      end Read_Key_Config;

   --  ------------------------------------------------------------------------

   procedure Register_Input_Actions is
   begin
      Input_Actions.Num_Actions := 0;
      Input_Actions.Left_Action_ID := Register_Key_Action ("Move_Left");
      Input_Actions.Right_Action_ID := Register_Key_Action ("Move_Right");
      Input_Actions.Up_Action_ID := Register_Key_Action ("Move_Fwd");
      Input_Actions.Down_Action_ID := Register_Key_Action ("Move_Bk");
      Input_Actions.Ok_Action_ID := Register_Key_Action ("Ok");
      Input_Actions.Clear_Binding_Action_ID := Register_Key_Action("Clear_Binding");

      Default_Key_Configuration;

   end Register_Input_Actions;

   --  ------------------------------------------------------------------------

   function Register_Key_Action (Name : String) return Integer is
   begin
      if Name = "" then
         raise Input_Handler_Exception with
           "Input_Handler.Register_Key_Actions key name is blank.";
      elsif Input_Actions.Num_Actions >= Max_Actions then
         raise Input_Handler_Exception with
           "Input_Handler.Register_Key_Actions too many key actions";
      end if;

      Input_Actions.Num_Actions := Input_Actions.Num_Actions + 1;
      Input_Actions.Action_Names (Input_Actions.Num_Actions) :=
        To_Unbounded_String (Name);
      return Input_Actions.Num_Actions;
   end Register_Key_Action;

   --  ------------------------------------------------------------------------

   function Right_Action return Integer is
   begin
      return Input_Actions.Right_Action_ID;
   end Right_Action;

   --  ------------------------------------------------------------------------

   procedure Set_Key_For_Action (Action_Name : String; Key_Code : Natural) is
      Dump : constant Integer := Set_Key_For_Action (Action_Name, Key_Code);
   begin
      null;
   end Set_Key_For_Action;

   --  ------------------------------------------------------------------------

   function Set_Key_For_Action (Action_Name : String; Key_Code : Natural)
                                return Integer is
      use Input_Callback;
      Result : Integer := -1;
   begin
      if Action_Name'Length < 1 then
         raise Input_Handler_Exception with
           " Input_Handler.Set_Key_For_Action, Action_Name is empty.";
      elsif Key_Code > Max_Keys then
         raise Input_Handler_Exception with
           " Input_Handler.Set_Key_For_Action, invalid Key_Code: ." &
           Natural'Image (Key_Code);
      end if;

      for index in 1 .. Input_Actions.Num_Actions loop
         if Input_Actions.Action_Names (index) = Action_Name then
            Input_Actions.Key_Bindings (index) := Key'Enum_Val (Key_Code);
            Result := index;
         end if;
      end loop;

      for index in 1 .. Input_Actions.Num_Actions loop
         if Input_Actions.Action_Names (index) /= Action_Name  and
           Input_Actions.Key_Bindings (index) = Key'Enum_Val (Key_Code) then
            Result := -2;
         end if;
      end loop;

      return Result;

   end Set_Key_For_Action;

   --  ------------------------------------------------------------------------

   function Up_Action return Integer is
   begin
      return Input_Actions.Up_Action_ID;
   end Up_Action;

   --  ------------------------------------------------------------------------

   function Was_Action_Pressed (Window : in out Input_Callback.Callback_Window;
                                Action_ID : Integer) return Boolean is
      Result : Boolean := False;
   begin
      if Action_ID <= 0 or Action_ID > Max_Actions then
         raise Input_Handler_Exception with
           "Input_Handler.Was_Action_Pressed invalid action code: " &
           Integer'Image (Action_ID);
      end if;

      Result := Input_Callback.Was_Key_Pressed
        (Window, Input_Actions.Key_Bindings (Action_ID));
      return Result;
   end Was_Action_Pressed;

   --  ------------------------------------------------------------------------

end Input_Handler;
