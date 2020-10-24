
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glfw.Input.Keys;

with GL.Types;

with Maths;

with Audio;
with Game_Utils;
with GUI;
with Input_Handler;
with Menu_Strings;
with Settings;
with Text;

package body Menu_Support is

   Num_Video_Modes : constant Integer := 10;
   type Size_Array is array (1 .. Num_Video_Modes) of Integer;

   Video_Mode_Widths  : constant Size_Array := (640, 800, 1024, 1280, 1600,
                                                1920, 1280, 1366, 1600, 1920);
   Video_Mode_Heights : constant Size_Array := (480, 600, 768, 960, 1200,
                                                1440, 720, 768, 900, 1080);
   Menu_Beep_Sound    : constant String := "metal_wood_clack_1.wav";

   Prev_Kb_Binding_Text    : Unbounded_String := To_Unbounded_String ("");
   Prev_Gp_Binding_Text    : Unbounded_String := To_Unbounded_String ("");
   Cal_Kb_Cursor_Curr_Item : Integer := 0;
   Menu_Cal_KB_Open        : Boolean := False;

   procedure Process_Menu_Gr_Cases (Cursor_Current_Item : Integer := -1);
   procedure Process_Video_Modes
     (Current_Mode       : in out Integer; Cursor_Current_Item : Integer;
      Graphic_Value_Text : GL_Maths.Integer_Array; Increment : Boolean);

   --  -------------------------------------------------------------------------

   function Cycle_Up_PC (IP : Integer) return Integer is
      IP_Inc : Integer := IP + 1;
   begin
      if IP > 10 then
         IP_Inc := 0;
      end if;
      return IP_Inc;
   end Cycle_Up_PC;

   --  -------------------------------------------------------------------------

   procedure Process_Menu_Cal_GP (Window  : in out Glfw.Windows.Window) is
   begin
      null;
   end Process_Menu_Cal_GP;

   --  -------------------------------------------------------------------------

   procedure Process_Menu_Cal_KB (Window                    : in out Glfw.Windows.Window;
                                  KB_Binding_Text          : GL_Maths.Integer_Array;
                                  Greatest_Axis_Text        : Integer;
                                  Already_Bound_Text         : Integer;
                                  Modify_Binding_Mode, Already_Bound : in out Boolean;
                                  Since_Last_Key            : in out Float) is
      use Glfw.Input.Keys;
      use Input_Handler;
      Pos_Index    : Integer := -1;
      Tex_Index    : Integer := -1;
      R            : Integer := -1;
      Prev_Binding : Integer := -1;
   begin
      GUI.Show_Controller_Button_Overlay (Pos_Index, Tex_Index);
      Pos_Index := 1;
      Tex_Index := 3;
      GUI.Show_Controller_Button_Overlay (Pos_Index, Tex_Index);
      Pos_Index := 2;
      Tex_Index := 1;
      GUI.Show_Controller_Button_Overlay (Pos_Index, Tex_Index);
      Text.Update_Text (Greatest_Axis_Text, "BACKSP or gamepad-Y to clear.");
      if Modify_Binding_Mode then
         Text.Update_Text (Greatest_Axis_Text, "ESC or gamepad-BACK to abort.");
         if Key_Pressed then
            if Last_Key_Down = Escape or
              Was_Action_Pressed (Window, Menu_Back_Action) then
               --  revert text to original
               Text.Update_Text (KB_Binding_Text (Cal_Kb_Cursor_Curr_Item),
                                 To_String (Prev_Kb_Binding_Text));
               Modify_Binding_Mode := False;
               Already_Bound := False;
               Lock_All_Keys;
            else
               R := Set_Key_For_Action (Action_Name (Cal_Kb_Cursor_Curr_Item),
                                        Key'Enum_Rep (Last_Key_Down));
               Already_Bound := False;
               if R = -2 then
                  Text.Update_Text (Already_Bound_Text, "that key is already bound!");
                  Already_Bound := True;
               end if;
               Modify_Binding_Mode := False;
               Text.Update_Text
                 (KB_Binding_Text (Cal_Kb_Cursor_Curr_Item),
                  To_String (Key_Name (Key'Enum_Rep (Last_Key_Down))));
               Lock_All_Keys;
            end if;
         end if;
      elsif Was_Key_Pressed (Window, Escape) or
        Was_Action_Pressed (Window, Open_Menu_Action) or
        Was_Action_Pressed (Window, Menu_Back_Action) then
         Menu_Cal_KB_Open := False;
      elsif Was_Key_Pressed (Window, Enter) or
        Was_Action_Pressed (Window, OK_Action) or
        Was_Action_Pressed (Window, Attack_Action) then
         Prev_Binding := Key_Binding (Cal_Kb_Cursor_Curr_Item);
         Prev_Kb_Binding_Text := Key_Name (Prev_Binding);
         Text.Update_Text (KB_Binding_Text (Cal_Kb_Cursor_Curr_Item), "???");
         Modify_Binding_Mode := True;
         Set_Key_Pressed (False);
      elsif Is_Key_Down (Up) or
        Is_Action_Down (Up_Action) then
         Cal_Kb_Cursor_Curr_Item := Cal_Kb_Cursor_Curr_Item - 1;
         if Cal_Kb_Cursor_Curr_Item < 0 then
            Cal_Kb_Cursor_Curr_Item := Num_Actions - 1;
         end if;
         Since_Last_Key := 0.0;
         Audio.Play_Sound (Menu_Beep_Sound, True);
      elsif Is_Key_Down (Down) or
        Is_Action_Down (Down_Action) then
         Cal_Kb_Cursor_Curr_Item := Cal_Kb_Cursor_Curr_Item + 1;
         if Cal_Kb_Cursor_Curr_Item > Num_Actions then
            Cal_Kb_Cursor_Curr_Item := 0;
         end if;
         Since_Last_Key := 0.0;
         Audio.Play_Sound (Menu_Beep_Sound, True);
      elsif Was_Key_Pressed (Window, Backspace) or
        Was_Action_Pressed (Window, Clear_Binding_Action) or
        Was_Joy_Y_Pressed then
         R := Set_Key_For_Action (Action_Name (Cal_Kb_Cursor_Curr_Item), 0);
         Text.Update_Text (KB_Binding_Text (Cal_Kb_Cursor_Curr_Item), "none");
      end if;
   end Process_Menu_Cal_KB;

   --  -------------------------------------------------------------------------

   procedure Process_Menu_Audio (Window                     : in out Glfw.Windows.Window;
                                 Audio_Value_Text           : GL_Maths.Integer_Array;
                                 Menu_Audio_Open            : in out Boolean;
                                Since_Last_Key             : in out Float;
                                Audio_Cursor_Current_Item  : in out Integer) is
      use Glfw.Input.Keys;
      use Input_Handler;
      use Menu_Support;
      use Menu_Strings;
      use Settings;
   begin
      if Was_Key_Pressed (Window, Escape) or
        Was_Action_Pressed (Window, Open_Menu_Action) or
        Was_Action_Pressed (Window, Menu_Back_Action) then
         Menu_Audio_Open := False;
         --  return
      elsif Was_Key_Pressed (Window, Enter) or
        Was_Action_Pressed (Window, OK_Action) or
        Was_Action_Pressed (Window, Attack_Action) then
         case Audio_Cursor_Current_Item is
            when 1 =>
               Set_Audio_Volume (Cycle_Up_PC (Audio_Volume));
               Audio.Set_Audio_Volume (Audio_Volume);
               Text.Update_Text (Audio_Value_Text (Audio_Cursor_Current_Item),
                                 Integer'Image (10 * Audio_Volume));
               Audio.Play_Sound (Menu_Beep_Sound, True);
            when 2 =>
               Set_Music_Volume (Cycle_Up_PC (Music_Volume));
               Audio.Set_Audio_Volume (Music_Volume);
               Text.Update_Text (Audio_Value_Text (Audio_Cursor_Current_Item),
                                 Integer'Image (10 * Music_Volume));
               Audio.Play_Sound (Menu_Beep_Sound, True);
            when others => null;
         end case;

         if Is_Key_Down (Up) or Is_Action_Down (Up_Action) then
            Audio_Cursor_Current_Item := Audio_Cursor_Current_Item - 1;
            if Audio_Cursor_Current_Item < 0 then
               Audio_Cursor_Current_Item := Num_Audio_Entries - 1;
               Since_Last_Key := 0.0;
               Audio.Play_Sound (Menu_Beep_Sound, True);
            end if;

         elsif Is_Key_Down (Down) or Is_Action_Down (Down_Action) then
            Audio_Cursor_Current_Item := Audio_Cursor_Current_Item + 1;
            if Audio_Cursor_Current_Item >= Num_Audio_Entries then
               Audio_Cursor_Current_Item := 0;
               Since_Last_Key := 0.0;
               Audio.Play_Sound (Menu_Beep_Sound, True);
            end if;

         elsif Is_Key_Down (Left) or Is_Action_Down (Left_Action) then
            if Audio_Cursor_Current_Item = 1 then
               Set_Audio_Volume (Maths.Max_Integer (0, Audio_Volume - 1));
               Text.Update_Text (Audio_Value_Text (1),
                                 Integer'Image (10 * Audio_Volume));
               Audio.Play_Sound (Menu_Beep_Sound, True);

            elsif Audio_Cursor_Current_Item = 2 then
               Set_Music_Volume (Maths.Max_Integer (0, Music_Volume - 1));
               Text.Update_Text (Audio_Value_Text (2),
                                 Integer'Image (10 * Music_Volume));
            end if;

         elsif Is_Key_Down (Right) or Is_Action_Down (Right_Action) then
            if Audio_Cursor_Current_Item = 1 then
               Set_Audio_Volume (Maths.Min_Integer (10, Audio_Volume + 1));
               Text.Update_Text (Audio_Value_Text (1),
                                 Integer'Image (10 * Audio_Volume));
               Audio.Play_Sound (Menu_Beep_Sound, True);

            elsif Audio_Cursor_Current_Item = 2 then
               Set_Music_Volume (Maths.Min_Integer (10, Music_Volume + 1));
               Text.Update_Text (Audio_Value_Text (2),
                                 Integer'Image (10 * Music_Volume));
            end if;
         end if;
      end if;
   end Process_Menu_Audio;

   --  -------------------------------------------------------------------------

   function Process_Menu_Gr (Window                     : in out Glfw.Windows.Window;
                             Graphic_Value_Text         : GL_Maths.Integer_Array;
                             Menu_Gr_Open, Restart_Flag : in out Boolean;
                             Since_Last_Key             : in out Float;
                             Cursor_Current_Item,
                             Current_Video_Mode         : in out Integer) return Boolean is
      use Glfw.Input.Keys;
      use Input_Handler;
      use Menu_Support;
      use Menu_Strings;
      use Settings;
      Result : Boolean;
   begin
      Result := Was_Key_Pressed (Window, Escape) or
        Was_Action_Pressed (Window, Open_Menu_Action) or
        Was_Action_Pressed (Window, Menu_Back_Action);
      if Result then
         Menu_Gr_Open := False;
         --  return
      elsif Was_Key_Pressed (Window, Enter) or
        Was_Action_Pressed (Window, OK_Action) or
        Was_Action_Pressed (Window, Attack_Action) then
         Process_Menu_Gr_Cases (Cursor_Current_Item);
         Audio.Play_Sound (Menu_Beep_Sound, True);
         Result := True;
         --  return
      elsif Is_Key_Down (Up) or Is_Action_Down (Up_Action) then
         Cursor_Current_Item := Cursor_Current_Item - 1;
         if Cursor_Current_Item < 0 then
            Cursor_Current_Item := Num_Graphic_Entries - 1;
            Since_Last_Key := 0.0;
            Audio.Play_Sound (Menu_Beep_Sound, True);
            Result := True;
         end if;
      elsif Is_Key_Down (Down) or Is_Action_Down (Down_Action) then
         Cursor_Current_Item := Cursor_Current_Item + 1;
         if Cursor_Current_Item >= Num_Graphic_Entries then
            Cursor_Current_Item := 0;
            Since_Last_Key := 0.0;
            Audio.Play_Sound (Menu_Beep_Sound, True);
            Result := True;
         end if;
      elsif Is_Key_Down (Left) or Is_Action_Down (Left_Action) then
         if Cursor_Current_Item = 0 then
            Set_Graphic_Preset
              (Gfx_Preset_Type'Enum_Val
                 (Gfx_Preset_Type'Enum_Rep (Graphic_Preset) - 1));
            if Graphic_Preset < Gfx_Dire then
               Set_Graphic_Preset (Gfx_Ultra);
               Text.Update_Text (Graphic_Value_Text (Cursor_Current_Item),
                                 Graphic_Preset_Strings
                                   (Gfx_Preset_Type'Enum_Rep (Graphic_Preset)));
               Result := Set_Menu_Graphic_Presets;
            end if;
         elsif Cursor_Current_Item = 2 then
            Process_Video_Modes (Current_Video_Mode, Cursor_Current_Item,
                                 Graphic_Value_Text, False);
            Restart_Flag := True;
         end if;
         Audio.Play_Sound (Menu_Beep_Sound, True);

      elsif Is_Key_Down (Right) or Is_Action_Down (Right_Action) then
         if Cursor_Current_Item = 0 then
            Set_Graphic_Preset
              (Gfx_Preset_Type'Enum_Val
                 (Gfx_Preset_Type'Enum_Rep (Graphic_Preset) + 1));
            if Graphic_Preset > Gfx_Ultra then
               Set_Graphic_Preset (Gfx_Dire);
               Text.Update_Text (Graphic_Value_Text (Cursor_Current_Item),
                                 Graphic_Preset_Strings
                                   (Gfx_Preset_Type'Enum_Rep (Graphic_Preset)));
               Result := Set_Menu_Graphic_Presets;
            end if;
         elsif Cursor_Current_Item = 2 then
            Process_Video_Modes (Current_Video_Mode, Cursor_Current_Item,
                                 Graphic_Value_Text, True);
            Restart_Flag := True;
         end if;
         Audio.Play_Sound (Menu_Beep_Sound, True);
      end if;  --  Line 1195
      return Result;
   end Process_Menu_Gr;

   --  -------------------------------------------------------------------------

   procedure Process_Menu_Gr_Cases (Cursor_Current_Item : Integer := -1) is
      use Glfw.Input.Keys;
      use Input_Handler;
   begin
      case Cursor_Current_Item is
         when 0 => null;
         when 1 => null;
         when 2 => null;
         when 3 => null;
         when 4 => null;
         when 5 => null;
         when 6 => null;
         when 7 => null;
         when 8 => null;
         when 9 => null;
         when 10 => null;
         when 11 => null;
         when 12 => null;
         when 13 => null;
         when 14 => null;
         when 15 => null;
         when 16 => null;
         when others => null;
      end case;

   end Process_Menu_Gr_Cases;

   --  -------------------------------------------------------------------------

   procedure Process_Video_Modes
     (Current_Mode       : in out Integer; Cursor_Current_Item : Integer;
      Graphic_Value_Text : GL_Maths.Integer_Array; Increment : Boolean) is
      use Settings;
      Width  : Integer;
      Height : Integer;
      Inc    : Integer;
   begin
      if Increment then
         Inc := 1;
      else
         Inc := -1;
      end if;
      for index in 1 .. Num_Video_Modes loop
         Current_Mode := Game_Utils.Loop_I
           (Current_Mode + Inc, 0, Num_Video_Modes - 1 );
         Width := Video_Mode_Widths (Current_Mode);
         Height := Video_Mode_Heights (Current_Mode);
         if Window_Width_To_Save /= Width or
           Window_Height_To_Save /= Height then
            Set_Window_Width_To_Save (Width);
            Set_Window_Height_To_Save (Height);
            declare
               New_Text : constant String :=
                            Integer'Image (Width) & "x" & Integer'Image (Height);
            begin
               Text.Update_Text (Graphic_Value_Text (Cursor_Current_Item), New_Text);
            end;
         end if;
      end loop;
   end Process_Video_Modes;

   --  -------------------------------------------------------------------------

   procedure Process_Menu_Input (Window  : in out Glfw.Windows.Window;
                                 Joy_Name : String;
                                 Menu_Input_Open,
                                 Menu_Cal_Gp_Butts_Open : in out Boolean;
                                 Joystick_Detected_Text  : in out Integer;
                                 Input_Cursor_Current_Item : in out Integer) is
      use Glfw.Input.Keys;
      use Input_Handler;
   begin
      if Was_Key_Pressed (Window, Escape) or
        Was_Action_Pressed (Window, Open_Menu_Action) or
        Was_Action_Pressed (Window, Menu_Back_Action) then
         Menu_Input_Open := False;
         Text.Update_Text (Joystick_Detected_Text,
                           "joystick detected: " & Joy_Name);
      elsif Was_Key_Pressed (Window, Enter) or
        Was_Action_Pressed (Window, OK_Action) or
        Was_Action_Pressed (Window, Attack_Action) then
         case Input_Cursor_Current_Item is
            when 0 => Settings.Disable_Joystick (not Settings.Joystick_Disabled);
            when 1 => Menu_Cal_KB_Open := True;
            when 2 =>
               if Joystick_Connected and not Settings.Joystick_Disabled then
                  Menu_Cal_Gp_Butts_Open := True;
                  Text.Update_Text (Joystick_Detected_Text,
                           "Joystick detected: " & Joy_Name);
               elsif not Joystick_Connected then
                  Text.Update_Text (Joystick_Detected_Text,
                           "Connect joystick first.");
               else
                  Text.Update_Text (Joystick_Detected_Text,
                           "Enable joystick first.");
               end if;
            when others => null;
         end case;
      end if;
   end Process_Menu_Input;

   --  -------------------------------------------------------------------------

end Menu_Support;

