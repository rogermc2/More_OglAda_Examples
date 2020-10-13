
with Glfw.Input.Mouse;

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Game_Utils;

package body Input_Handler is

   Input_Actions : Input_Actions_Data;
   Input_State   : Input_State_Data;

   function Register_Key_Action (Name : String) return Integer;
   procedure Set_Joy_Axis_For_Action (Action_Name : String; Key_Code : Natural;
                                      Sign        : Character);
   procedure Set_Joy_Button_For_Action (Action_Name : String; Key_Code : Natural);
   procedure Set_Key_For_Action (Action_Name : String; Key_Code : Natural);

   --  ------------------------------------------------------------------------

   function Action_Name (Index: Integer) return Unbounded_String is
   begin
      return Input_Actions.Action_Names (Index);
   end Action_Name;

   --  ------------------------------------------------------------------------

   function Attack_Action return Integer is
   begin
      return Input_Actions.Attack_Action;
   end Attack_Action;

   --  ------------------------------------------------------------------------

   procedure Config_Joystick (aLine : String) is
      use Ada.Strings;
      Start   : constant Positive := aLine'First;
      Last    : constant Positive := aLine'Last;
      Pos1    : Positive := Fixed.Index (aLine, "_");
      Pos2    : Positive := Fixed.Index (aLine, " ");
      Action  : String := aLine (Start .. Pos2 - 1);
      Code    : Integer;
      Sign    : Character := '+';
      Axis    : Integer := 0;
      Index   : Integer := 0;
      J_Index : Integer := -1;
      Found   : Boolean := False;
   begin
      -- Find index of Action
      while not Found and  index <= Input_Actions.Num_Actions loop
         Index := Index + 1;
         Found := Input_Actions.Action_Names (index) = Action;
         if Found then
            J_Index := Index;
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
               Input_Actions.Joy_Axis_Bindings (J_Index) := -1;
               Input_Actions.Joy_Axis_Sign (J_Index) := ' ';
            else
               if Sign /= '-' and Sign /= '+' then
                  raise Input_Handler_Exception with
                    "Read_Key_Config joystick axis sign " & Sign & "invalid.";
               end if;
               Input_Actions.Joy_Axis_Bindings (J_Index) := Axis;
               Input_Actions.Joy_Axis_Sign (J_Index) := Sign;
            end if;
         end if;
         Code := Integer'Value (aLine (Pos2 + 1 .. Last));
         Set_Key_For_Action (Action, Code);
      end if;

   end Config_Joystick;

   --  ------------------------------------------------------------------------

   function Down_Action return Integer is
   begin
      return Input_Actions.Down_Action;
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

   procedure Init (Window  : in out Glfw.Windows.Window) is
      --          Hide_Cursor : Boolean := True;
   begin
      Game_Utils.Game_Log ("--- Init Input Handler ---");
      Window.Set_Cursor_Mode (Glfw.Input.Mouse.Hidden);
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

   function Joy_Axis_Bindings (Index: Integer) return Integer is
   begin
      return Input_Actions.Joy_Axis_Bindings (Index);
   end Joy_Axis_Bindings;

   --  ------------------------------------------------------------------------

   function Joy_Axis_Sign (Index: Integer) return Character is
   begin
      return Input_Actions.Joy_Axis_Sign (Index);
   end Joy_Axis_Sign;

   --  ------------------------------------------------------------------------

   function Joy_Button_Bindings (Index: Integer) return Integer is
   begin
      return Input_Actions.Joy_Button_Bindings (Index);
   end Joy_Button_Bindings;

   --  ------------------------------------------------------------------------

   function Key_Binding (Index: Integer) return Integer is
   begin
      return Key'Enum_Rep (Input_Actions.Key_Bindings (Index));
   end Key_Binding;

   --  ------------------------------------------------------------------------

   function Key_Name (Index: Integer) return Unbounded_String is
   begin
      return Input_State.Key_Names (Index);
   end Key_Name;

   --  ------------------------------------------------------------------------

   function Num_Actions return Integer is
   begin
      return Input_Actions.Num_Actions;
   end Num_Actions;

   --  ------------------------------------------------------------------------

   function Ok_Action return Integer is
   begin
      return Input_Actions.Ok_Action;
   end Ok_Action;

   --  ------------------------------------------------------------------------

   function Open_Menu_Action return Integer is
   begin
      --  Joystick processing
      return Input_Actions.Open_Menu_Action;
   end Open_Menu_Action;

   --  ------------------------------------------------------------------------

   procedure Read_Key_Config (aLine : String) is
      use Ada.Strings;
      Start   : constant Positive := aLine'First;
      Last    : constant Positive := aLine'Last;
      Pos1    : Positive := Fixed.Index (aLine, "_");
      Pos2    : Positive := Fixed.Index (aLine, " ");
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
            Action : String := aLine (Start .. Pos2 - 1);
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
      Input_Actions.Num_Actions := 0;
      Input_Actions.Left_Action := Register_Key_Action ("Move_Left");
      Input_Actions.Right_Action := Register_Key_Action ("Move_Right");
      Input_Actions.Up_Action := Register_Key_Action ("Move_Fwd");
      Input_Actions.Down_Action := Register_Key_Action ("Move_Bk");
      Input_Actions.Attack_Action := Register_Key_Action ("Attack");
      Input_Actions.Open_Door_Action := Register_Key_Action ("Open_Door/Buy");
      Input_Actions.Wipe_Screen_Action := Register_Key_Action ("Wipe_Screen");
      Input_Actions.Sel_Sword_Action := Register_Key_Action ("Select_Sword");
      Input_Actions.Sel_Javelin_Action := Register_Key_Action ("Select_Javelin");
      Input_Actions.Sel_Hammer_Action := Register_Key_Action ("Select_Hammer");
      Input_Actions.Cycle_Weapons_Action := Register_Key_Action ("Cycle_Weapons");
      Input_Actions.Open_Menu_Action := Register_Key_Action ("Open_Menu");
      Input_Actions.Menu_Back_Action := Register_Key_Action ("Menu_Back");
      Input_Actions.Ok_Action := Register_Key_Action ("Ok");
      Input_Actions.Clear_Binding_Action := Register_Key_Action("Clear_Binding");

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

   procedure Set_Key_For_Action (Action_Name : String; Key_Code : Natural) is
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
         end if;
      end loop;

      for index in 1 .. Input_Actions.Num_Actions loop
         if Input_Actions.Action_Names (index) /= Action_Name  and
           Input_Actions.Key_Bindings (index) = Key'Enum_Val (Key_Code) then
            Game_Utils.Game_Log (" Input_Handler.Set_Key_For_Action,  WARNING:" &
                                   " duplicate entry for Key_Code: " & Natural'Image (Key_Code));
         end if;
      end loop;

   end Set_Key_For_Action;

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

   function Up_Action return Integer is
   begin
      return Input_Actions.Up_Action;
   end Up_Action;

   --  ------------------------------------------------------------------------

   function Was_Action_Pressed (Action : Integer) return Boolean is
      Result : Boolean := False;
   begin
      if Action <= 0 or Action > Max_Actions then
         raise Input_Handler_Exception with
           "Input_Handler.Was_Action_Pressed invalid action code: " &
           Integer'Image (Action);
      end if;

      --  Joystick not implemented
      Put_Line ("Input_Handler.Was_Action_Pressed action code: " &
                  Integer'Image (Action));
      Result := Was_Key_Pressed (Input_Actions.Key_Bindings (Action));
      return Result;
   end Was_Action_Pressed;

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
      if Key_Val < 32 or Key_Val > Max_Keys then
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
