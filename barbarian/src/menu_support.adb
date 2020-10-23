
with Glfw.Input.Keys;

with Audio;
with Game_Utils;
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

   procedure Process_Menu_Audio (Window  : in out Glfw.Windows.Window;
                                 Menu_Audio_Open : in out Boolean;
                                 Audio_Cursor_Current_Item  : Integer := -1) is
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
         Menu_Audio_Open := False;
         --  return
      elsif Was_Key_Pressed (Window, Enter) or
        Was_Action_Pressed (Window, OK_Action) or
        Was_Action_Pressed (Window, Attack_Action) then
         case Audio_Cursor_Current_Item is
            when 1 =>
               Set_Audio_Volume (Cycle_Up_PC (Audio_Volume));
               Audio.Set_Audio_Volume (Audio_Volume);
            when 2 => null;
            when others => null;
         end case;
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

end Menu_Support;

