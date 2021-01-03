
with Glfw.Input.Mouse;

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Game_Utils;
with Settings;

package body Input_Handler is

   Actions : Input_Actions_Data;
--     Input_State   : Input_State_Data;

   function Register_Key_Action (Name : String) return Integer;

   --  ------------------------------------------------------------------------

   function Action_Name (Index: Action_Range) return String is
   begin
      return To_String (Actions.Action_Names (Index));
   end Action_Name;

   --  ------------------------------------------------------------------------

   function Attack_Action return Integer is
   begin
      return Actions.Attack_Action_ID;
   end Attack_Action;

   --  ------------------------------------------------------------------------

   procedure Config_Joystick (aLine : String) is
      use Ada.Strings;
      Start   : constant Positive := aLine'First;
      Last    : constant Positive := aLine'Last;
      Pos1    : Positive := Fixed.Index (aLine, "_");
      Pos2    : constant Positive := Fixed.Index (aLine, " ");
      Action  : constant String := aLine (Start .. Pos2 - 1);
      Code    : Integer;
      Sign    : Character := '+';
      Axis    : Integer := 0;
      J_Index : Action_Range;
      Found   : Boolean := False;
   begin
      -- Find index of Action
      for index in Action_Range loop
         if Found then
            J_Index := Index;
         else
            Found := Actions.Action_Names (index) = Action;
         end if;
      end loop;

      if Found then
         if aLine (Pos2 + 1) /= 'B' then
            Sign := aLine (Pos2 + 1);
            if aLine (Pos2 + 2 .. Pos2 + 6) /= "AXIS" then
               raise Input_Handler_Exception with
                 "Read_Key_Config joy config line.";
            end if;
            Axis := Integer'Value (aLine (Pos2 + 7 .. Last));
            if Axis < 0 or Axis > 7 then
               Actions.Joy_Axis_Bindings (Action_Range (J_Index)) := -1;
               Actions.Joy_Axis_Sign (Action_Range (J_Index)) := ' ';
            else
               if Sign /= '-' and Sign /= '+' then
                  raise Input_Handler_Exception with
                    "Read_Key_Config joystick axis sign " & Sign & "invalid.";
               end if;
               Actions.Joy_Axis_Bindings (Action_Range (J_Index)) := Axis;
               Actions.Joy_Axis_Sign (Action_Range (J_Index)) := Sign;
            end if;
         end if;
         Code := Integer'Value (aLine (Pos2 + 1 .. Last));
         Set_Key_For_Action (Action, Code);
      end if;

   end Config_Joystick;

   --  ------------------------------------------------------------------------

   function Clear_Binding_Action return Integer is
   begin
      return Actions.Clear_Binding_Action_ID;
   end Clear_Binding_Action;

   --  ------------------------------------------------------------------------

   function Cycle_Weapons_Action return Integer is
   begin
      return Actions.Cycle_Weapons_Action_ID;
   end Cycle_Weapons_Action;

   --  -------------------------------------------------------------------------

   function Down_Action return Integer is
   begin
      return Actions.Down_Action_ID;
   end Down_Action;

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
      Set_Joy_Button_For_Action ("Select_Hammer", 0); -- Invalid/Nothing
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

   function Hammer_Action return Integer is
   begin
      return Actions.Sel_Hammer_Action_ID;
   end Hammer_Action;

   --  -------------------------------------------------------------------------

   procedure Init (Window  : in out Input_Callback.Barbarian_Window) is
      --          Hide_Cursor : Boolean := True;
   begin
      Game_Utils.Game_Log ("--- Init Input Handler ---");
      Window.Set_Cursor_Mode (Glfw.Input.Mouse.Hidden);
--        Input_State.Mouse_Cursor_Hidden := True;
   end Init;

   --  ------------------------------------------------------------------------

   function Is_Action_Down (Action : Action_Range) return Boolean is
   begin
      --  Joystick processing
      return Input_Callback.Is_Key_Down (Actions.Key_Bindings (Action));
   end Is_Action_Down;

   --  ------------------------------------------------------------------------

   function Javelin_Action return Integer is
   begin
      return Actions.Sel_Javelin_Action_ID;
   end Javelin_Action;

   --  -------------------------------------------------------------------------

   function Joy_Axis_Bindings (Index: Action_Range) return Integer is
   begin
      return Actions.Joy_Axis_Bindings (Index);
   end Joy_Axis_Bindings;

   --  ------------------------------------------------------------------------

   function Joy_Axis_Sign (Index: Action_Range) return Character is
   begin
      return Actions.Joy_Axis_Sign (Index);
   end Joy_Axis_Sign;

   --  ------------------------------------------------------------------------

   function Joy_Button_Bindings (Index: Action_Range) return Integer is
   begin
      return Actions.Joy_Button_Bindings (Index);
   end Joy_Button_Bindings;

   --  ------------------------------------------------------------------------

   function Key_Binding (Index: Action_Range) return Integer is
   begin
      return Key'Enum_Rep (Actions.Key_Bindings (Index));
   end Key_Binding;

   --  ------------------------------------------------------------------------

   function Left_Action return Integer is
   begin
      return Actions.Left_Action_ID;
   end Left_Action;

   --  ------------------------------------------------------------------------

   function Menu_Back_Action return Integer is
   begin
      return Actions.Menu_Back_Action_ID;
   end Menu_Back_Action;

   --  ------------------------------------------------------------------------

   function Menu_Open_Action return Integer is
   begin
      --  Joystick processing
      return Actions.Open_Menu_Action_ID;
   end Menu_Open_Action;

   --  ------------------------------------------------------------------------

   function Num_Actions return Integer is
   begin
      return Actions.Num_Actions;
   end Num_Actions;

   --  ------------------------------------------------------------------------

   function OK_Action return Integer is
   begin
      return Actions.Ok_Action_ID;
   end OK_Action;

   --  ------------------------------------------------------------------------

   function Open_Door_Action return Integer is
   begin
      return Actions.Open_Door_Action_ID;
   end Open_Door_Action;

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
         end ;

      elsif aLine (Start) = 'J' then
         Config_Joystick (aLine);
      end if;

   end Read_Key_Config;

   --  ------------------------------------------------------------------------

   procedure Register_Input_Actions is
   begin
      --        if false then
      --           raise Input_Handler_Exception with
      --             "Input_Handler.Register_Input_Actions could not read key names file: " &
      --           Integer'Image (Action);
      --        end if;
      Game_Utils.Game_Log ("Register_Input_Actions");
      Actions.Num_Actions := 0;
      Actions.Left_Action_ID := Register_Key_Action ("Move_Left");
      Actions.Right_Action_ID := Register_Key_Action ("Move_Right");
      Actions.Up_Action_ID := Register_Key_Action ("Move_Fwd");
      Actions.Down_Action_ID := Register_Key_Action ("Move_Bk");
      Actions.Attack_Action_ID := Register_Key_Action ("Attack");
      Actions.Open_Door_Action_ID := Register_Key_Action ("Open_Door/Buy");
      Actions.Wipe_Screen_Action_ID := Register_Key_Action ("Wipe_Screen");
      Actions.Sel_Sword_Action_ID := Register_Key_Action ("Select_Sword");
      Actions.Sel_Javelin_Action_ID := Register_Key_Action ("Select_Javelin");
      Actions.Sel_Hammer_Action_ID := Register_Key_Action ("Select_Hammer");
      Actions.Cycle_Weapons_Action_ID := Register_Key_Action ("Cycle_Weapons");
      Actions.Open_Menu_Action_ID := Register_Key_Action ("Open_Menu");
      Actions.Menu_Back_Action_ID := Register_Key_Action ("Menu_Back");
      Actions.Ok_Action_ID := Register_Key_Action ("Ok");
      Actions.Clear_Binding_Action_ID := Register_Key_Action("Clear_Binding");

      Default_Key_Configuration;

   end Register_Input_Actions;

   --  ------------------------------------------------------------------------

   function Register_Key_Action (Name : String) return Integer is
   begin
      if Name = "" then
         raise Input_Handler_Exception with
           "Input_Handler.Register_Key_Actions key name is blank.";
      elsif Actions.Num_Actions >= Max_Actions then
         raise Input_Handler_Exception with
           "Input_Handler.Register_Key_Actions too many key actions";
      end if;

      Actions.Num_Actions := Actions.Num_Actions + 1;
      Actions.Action_Names (Actions.Num_Actions) :=
        To_Unbounded_String (Name);
      return Actions.Num_Actions;
   end Register_Key_Action;

   --  ------------------------------------------------------------------------

   function Right_Action return Integer is
   begin
      return Actions.Right_Action_ID;
   end Right_Action;

   --  ------------------------------------------------------------------------

   procedure Set_Joy_Axis_For_Action (Action_Name : String; Key_Code : Natural;
                                      Sign        : Character) is
   begin
      null;
   end Set_Joy_Axis_For_Action;

   --  ------------------------------------------------------------------------

   procedure Set_Joy_Button_For_Action (Action_Name : String; Key_Code : Natural) is
   begin
      null;
   end Set_Joy_Button_For_Action;

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

      for index in 1 .. Actions.Num_Actions loop
         if Actions.Action_Names (index) = Action_Name then
            Actions.Key_Bindings (index) := Key'Enum_Val (Key_Code);
            Result := index;
         end if;
      end loop;

      for index in 1 .. Actions.Num_Actions loop
         if Actions.Action_Names (index) /= Action_Name  and
           Actions.Key_Bindings (index) = Key'Enum_Val (Key_Code) then
            Game_Utils.Game_Log ("Input_Handler.Set_Key_For_Action,  WARNING:"
                                 &  " duplicate entry for Key_Code: " &
                                 Natural'Image (Key_Code));
            Result := -2;
         end if;
      end loop;

      return Result;

   end Set_Key_For_Action;

   --  ------------------------------------------------------------------------

   function Sword_Action return Integer is
   begin
      return Actions.Sel_Sword_Action_ID;
   end Sword_Action;

   --  -------------------------------------------------------------------------

   function Up_Action return Integer is
   begin
      return Actions.Up_Action_ID;
   end Up_Action;

   --  ------------------------------------------------------------------------

   function Was_Action_Pressed (Window : in out Input_Callback.Barbarian_Window;
                                Action_ID : Action_Range) return Boolean is
      Result : Boolean := False;
   begin
      --  Joystick not implemented
      Result := Input_Callback.Was_Key_Pressed
        (Window, Actions.Key_Bindings (Action_ID));
      return Result;
   end Was_Action_Pressed;

   --  ------------------------------------------------------------------------

   function Wipe_Screen_Action return Integer is
   begin
      return Actions.Wipe_Screen_Action_ID;
   end Wipe_Screen_Action;

   --  ------------------------------------------------------------------------
end Input_Handler;
