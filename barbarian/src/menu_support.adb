
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glfw.Input.Keys;

with GL.Types;

with Maths;

with Audio;
with Camera;
with Cursor_Shader_Manager;
with Game_Utils;
with GL_Utils;
with GUI;
with Input_Handler;
with Menu_Strings;
with Settings;
with Text;
with Texture_Manager;

package body Menu_Support is

   Title_Impact_Sound : constant String := "DEMOLISH_Wood_Metal_Deep_stereo.wav";
   Num_Video_Modes    : constant Integer := 10;
   type Size_Array is array (1 .. Num_Video_Modes) of Integer;

   Credits_Music      : constant String := "Protagonist_through_Pain.ogg";
   Video_Mode_Widths  : constant Size_Array := (640, 800, 1024, 1280, 1600,
                                                1920, 1280, 1366, 1600, 1920);
   Video_Mode_Heights : constant Size_Array := (480, 600, 768, 960, 1200,
                                                1440, 720, 768, 900, 1080);
   Menu_Beep_Sound    : constant String := "metal_wood_clack_1.wav";

   Prev_Kb_Binding_Text    : Unbounded_String := To_Unbounded_String ("");
   Prev_Gp_Binding_Text    : Unbounded_String := To_Unbounded_String ("");
   Cal_Kb_Cursor_Curr_Item : Integer := 0;
   Menu_Cal_KB_Open        : Boolean := False;
   Cursor_Rot_Speed        : constant Float := 240.0; --  deg per sec
   Cursor_Degrees          : Maths.Degree := 0.0;

   procedure Process_Menu_Graphic_Cases (Cursor_Item : Integer := -1);
   procedure Process_Video_Modes
     (Current_Mode       : in out Integer; Cursor_Item : Integer;
      Graphic_Value_Text : GL_Maths.Integer_Array; Increment : Boolean);

   --  -------------------------------------------------------------------------

   function Confirm_Quit_Open (Window            : in out Glfw.Windows.Window;
                               Confirm_Quit_Open : in out Boolean)
                               return Boolean is
      use Glfw.Input.Keys;
      use Input_Handler;
      Result : Boolean := False;
   begin
      Result := Was_Key_Pressed (Window, Escape) or
        Was_Action_Pressed (Window, Open_Menu_Action) or
        Was_Action_Pressed (Window, Menu_Back_Action);
      if Result then
         Confirm_Quit_Open := False;
      else
         Result := not Was_Key_Pressed (Window, Y) or
           Was_Key_Pressed (Window, Enter) or
           Was_Action_Pressed (Window, OK_Action) or
           Was_Action_Pressed (Window, Attack_Action);
         Confirm_Quit_Open := False;
      end if;
      return Result;

   end Confirm_Quit_Open;

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

   procedure Do_Bounce (Title_Bounce_Timer : in out Float; Elapsed : Float;
                        Title_V : in out GL.Types.Singles.Matrix4) is
      use GL.Types;
      use GL.Types.Singles;
      use GL_Maths;
      End_Fall_At   : constant Single := 0.5;
      End_Bounce_At : constant Single := 1.0;
      Num_Bounces   : constant Integer := 3;
      Fall_Dist     : constant Single := 2.5; -- 1.0 is middle of 3d mesh at top of screen
      Bounce_Amp    : Float := 0.5;
      Bounce_Matrix : Matrix4 := Identity4;
      Y             : Single;
      Cam_Pos       : constant Vector3 := (0.0, -6.5, 3.0);
      Cam_Target    : constant Vector3 := Cam_Pos + (0.0, 1.0, -1.0);
   begin
      if End_Bounce_At >= Single (Title_Bounce_Timer) then
         Y := Accel_Exp (Single (Title_Bounce_Timer), (0.0, End_Fall_At),
                         (Fall_Dist, 0.0));
         if Float (End_Fall_At) < Title_Bounce_Timer + Elapsed then
            Audio.Play_Sound (Title_Impact_Sound, False);
         end if;
      else
         Y := Decel_Bounce (Single (Title_Bounce_Timer), (0.0, End_Fall_At),
                         (Fall_Dist, 0.0), Num_Bounces);
      end if;
      Bounce_Matrix := Maths.Translation_Matrix ((0.0, Y, 0.0));

      Maths.Init_Lookat_Transform
        (Cam_Pos, Cam_Target, (0.0, 1.0, 1.0), Title_V);
      Title_V := Bounce_Matrix * Title_V;
      Title_Bounce_Timer := Title_Bounce_Timer + Elapsed;
   end Do_Bounce;

   --  -------------------------------------------------------------------------

   procedure Draw_3D_Menu_Items
     (Menu_Cursor_Texture   : GL.Objects.Textures.Texture;
      Cursor_VAO            : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Cursor_Shader_Program : GL.Objects.Programs.Program;
      Cursor_M              : in out GL.Types.Singles.Matrix4;
      Cursor_V              : GL.Types.Singles.Matrix4;
      Cursor_Pos            : GL.Types.Singles.Vector2;
      Cursor_Scale          : GL.Types.Single;
      Cursor_Point_Count    : Integer; Elapsed : Float) is
      use GL.Objects.Vertex_Arrays;
      use GL.Types;
      use GL.Types.Singles;
      use Maths;
      Scale        : constant Single :=
                       Cursor_Scale / Single (Settings.Framebuffer_Height);
      Scale_Matrix : constant Matrix4 := Scaling_Matrix (Scale);
      Rot_Matrix   : Matrix4;
      T_Matrix     : Matrix4 := Identity4;
      P_Matrix     : Matrix4 := Identity4;
   begin
      Texture_Manager.Bind_Texture (0, Menu_Cursor_Texture);
      GL_Utils.Bind_VAO (Cursor_VAO);
      Game_Utils.Game_Log ("Menu_Support.Draw_3D_Menu_Items, Cursor_VAO bound");
      Cursor_Degrees := Cursor_Degrees + Degree (Cursor_Rot_Speed * Elapsed);
      Rot_Matrix := Rotate_Y_Degree (Scale_Matrix, Cursor_Degrees);
      Cursor_M := Rot_Matrix;
      T_Matrix := Translation_Matrix ((Cursor_Pos (GL.X), Cursor_Pos (GL.X), 0.0));
      P_Matrix := T_Matrix * Camera.GUI_Proj_Matrix;
      GL.Objects.Programs.Use_Program (Cursor_Shader_Program);
      Cursor_Shader_Manager.Set_Model_Matrix (Cursor_M);
      Cursor_Shader_Manager.Set_View_Matrix (Cursor_V);
      Cursor_Shader_Manager.Set_Perspective_Matrix (P_Matrix);
      Draw_Arrays (Triangles, 0, Int (Cursor_Point_Count));
   end Draw_3D_Menu_Items;

   --  -------------------------------------------------------------------------

   function General_Menu_Support (Window  : in out Glfw.Windows.Window;
                                 Joystick_Detected_Text    : Integer;
                                 Joy_Name : String;
                                  Menu_Was_Closed, Graphics_Open, Audio_Open,
                                  Input_Open, Confirm_Quit_Open,
                                  Credits_Open, New_Game, In_Custom_Map,
                                  Custom_Maps  : in out Boolean;
                                  Since_Last_Key  : in out Float;
                                  Menu_Cursor_Item : in out Menu_Choice_Type)
                                  return Boolean is
      use Glfw.Input.Keys;
      use Input_Handler;
      Result : Boolean := False;
   begin
      if Was_Key_Pressed (Window, Escape) or
        Was_Action_Pressed (Window, Open_Menu_Action) or
        Was_Action_Pressed (Window, Menu_Back_Action) then
         Result := Settings.Save_Settings;
         if Result then
            Menu_Was_Closed := True;
         else
            Game_Utils.Game_Log ("Menu_Support.General_Menu_Support, " &
                                 "ERROR: could not save settings from Mmenu");
         end if;
      elsif Is_Key_Down (Up) or
        Is_Action_Down (Up_Action) then
         Menu_Cursor_Item := Menu_Choice_Type'Pred (Menu_Cursor_Item);
         Since_Last_Key := 0.0;
         Audio.Play_Sound (Menu_Beep_Sound, True);
         Result := True;
      elsif Is_Key_Down (Down) or
        Is_Action_Down (Down_Action) then
         Menu_Cursor_Item := Menu_Choice_Type'Succ (Menu_Cursor_Item);
         Since_Last_Key := 0.0;
         Audio.Play_Sound (Menu_Beep_Sound, True);
         Result := True;
      elsif Was_Key_Pressed (Window, Enter) or
        Was_Action_Pressed (Window, OK_Action) or
        Was_Action_Pressed (Window, Attack_Action) then
         if Menu_Cursor_Item = Menu_Quit then
            Confirm_Quit_Open := True;
            Result := Settings.Save_Settings;
            if not Result then
                Game_Utils.Game_Log ("Menu_Support.General_Menu_Support, " &
                                 "ERROR: could not save settings from Mmenu");
            end if;
         elsif Menu_Cursor_Item = Menu_Graphics then
            Graphics_Open := True;
            Result := True;
         elsif Menu_Cursor_Item = Menu_Audio then
            Audio_Open := True;
            Result := True;
         elsif Menu_Cursor_Item = Menu_Input then
            Input_Open := True;
            Text.Update_Text (Joystick_Detected_Text,
                              "joystick detected: " & Joy_Name);
            Result := True;
         elsif Menu_Cursor_Item = Menu_Credits then
            Credits_Open := True;
            Audio.Pause_Music (True);
--              Audio.Play_Credits_Music (Credits_Music);
            Result := True;
         elsif Menu_Cursor_Item = Menu_New_Game then
            New_Game := True;
            In_Custom_Map := False;
            Result := Settings.Save_Settings;
            if not Result then
               Game_Utils.Game_Log ("Menu_Support.General_Menu_Support, " &
                                    "ERROR: could not save settings from Mmenu");
            end if;
         elsif Menu_Cursor_Item = Menu_Custom_Map then
            Custom_Maps := True;
            In_Custom_Map := True;
            Result := Settings.Save_Settings;
            if not Result then
               Game_Utils.Game_Log ("Menu_Support.General_Menu_Support, " &
                                    "ERROR: could not save settings from Mmenu");
            end if;
         end if;
      end if;

      return Result;
   end General_Menu_Support;

   --  -------------------------------------------------------------------------

   procedure Process_Menu_Cal_GP (Window  : in out Glfw.Windows.Window) is
   begin
      null;
   end Process_Menu_Cal_GP;

   --  -------------------------------------------------------------------------

   procedure Process_Menu_Cal_KB (Window                             : in out Glfw.Windows.Window;
                                  KB_Binding_Text                    : GL_Maths.Integer_Array;
                                  Greatest_Axis_Text                 : Integer;
                                  Already_Bound_Text                 : Integer;
                                  Modify_Binding_Mode, Already_Bound : in out Boolean;
                                  Since_Last_Key                     : in out Float) is
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
                                 Audio_Cursor_Item  : in out Integer) is
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
         case Audio_Cursor_Item is
            when 1 =>
               Set_Audio_Volume (Cycle_Up_PC (Audio_Volume));
               Audio.Set_Audio_Volume (Audio_Volume);
               Text.Update_Text (Audio_Value_Text (Audio_Cursor_Item),
                                 Integer'Image (10 * Audio_Volume));
               Audio.Play_Sound (Menu_Beep_Sound, True);
            when 2 =>
               Set_Music_Volume (Cycle_Up_PC (Music_Volume));
               Audio.Set_Audio_Volume (Music_Volume);
               Text.Update_Text (Audio_Value_Text (Audio_Cursor_Item),
                                 Integer'Image (10 * Music_Volume));
               Audio.Play_Sound (Menu_Beep_Sound, True);
            when others => null;
         end case;

         if Is_Key_Down (Up) or Is_Action_Down (Up_Action) then
            Audio_Cursor_Item := Audio_Cursor_Item - 1;
            if Audio_Cursor_Item < 0 then
               Audio_Cursor_Item := Num_Audio_Entries - 1;
               Since_Last_Key := 0.0;
               Audio.Play_Sound (Menu_Beep_Sound, True);
            end if;

         elsif Is_Key_Down (Down) or Is_Action_Down (Down_Action) then
            Audio_Cursor_Item := Audio_Cursor_Item + 1;
            if Audio_Cursor_Item >= Num_Audio_Entries then
               Audio_Cursor_Item := 0;
               Since_Last_Key := 0.0;
               Audio.Play_Sound (Menu_Beep_Sound, True);
            end if;

         elsif Is_Key_Down (Left) or Is_Action_Down (Left_Action) then
            if Audio_Cursor_Item = 1 then
               Set_Audio_Volume (Maths.Max_Integer (0, Audio_Volume - 1));
               Text.Update_Text (Audio_Value_Text (1),
                                 Integer'Image (10 * Audio_Volume));
               Audio.Play_Sound (Menu_Beep_Sound, True);

            elsif Audio_Cursor_Item = 2 then
               Set_Music_Volume (Maths.Max_Integer (0, Music_Volume - 1));
               Text.Update_Text (Audio_Value_Text (2),
                                 Integer'Image (10 * Music_Volume));
            end if;

         elsif Is_Key_Down (Right) or Is_Action_Down (Right_Action) then
            if Audio_Cursor_Item = 1 then
               Set_Audio_Volume (Maths.Min_Integer (10, Audio_Volume + 1));
               Text.Update_Text (Audio_Value_Text (1),
                                 Integer'Image (10 * Audio_Volume));
               Audio.Play_Sound (Menu_Beep_Sound, True);

            elsif Audio_Cursor_Item = 2 then
               Set_Music_Volume (Maths.Min_Integer (10, Music_Volume + 1));
               Text.Update_Text (Audio_Value_Text (2),
                                 Integer'Image (10 * Music_Volume));
            end if;
         end if;
      end if;
   end Process_Menu_Audio;

   --  -------------------------------------------------------------------------

   procedure Process_Menu_Credits (Window      : in out Glfw.Windows.Window;
                                   Credits_Open, End_Story_Open,
                                   Menu_Closed : in out Boolean;
                                  Text_Timer : in out Float) is
      use Glfw.Input.Keys;
      use Input_Handler;
   begin
      if Was_Key_Pressed (Window, Space) or Was_Key_Pressed (Window, Escape) or
        Was_Action_Pressed (Window, Open_Menu_Action) or
        Was_Action_Pressed (Window, OK_Action) then
         Credits_Open := False;
         if End_Story_Open then
            End_Story_Open := False;
            Menu_Closed := True;
         end if;
         Text_Timer := 0.0;
         Audio.Stop_Credits_Music;
         Audio.Pause_Music (False);
      end if;
   end Process_Menu_Credits;

   --  -------------------------------------------------------------------------

   function Process_Menu_Graphics (Window                     : in out Glfw.Windows.Window;
                                   Graphic_Value_Text         : GL_Maths.Integer_Array;
                                   Menu_Gr_Open, Restart_Flag : in out Boolean;
                                   Since_Last_Key             : in out Float;
                                   Cursor_Item, Video_Mode    : in out Integer) return Boolean is
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
         Process_Menu_Graphic_Cases (Cursor_Item);
         Audio.Play_Sound (Menu_Beep_Sound, True);
         Result := True;
         --  return
      elsif Is_Key_Down (Up) or Is_Action_Down (Up_Action) then
         Cursor_Item := Cursor_Item - 1;
         if Cursor_Item < 0 then
            Cursor_Item := Num_Graphic_Entries - 1;
            Since_Last_Key := 0.0;
            Audio.Play_Sound (Menu_Beep_Sound, True);
            Result := True;
         end if;
      elsif Is_Key_Down (Down) or Is_Action_Down (Down_Action) then
         Cursor_Item := Cursor_Item + 1;
         if Cursor_Item >= Num_Graphic_Entries then
            Cursor_Item := 0;
            Since_Last_Key := 0.0;
            Audio.Play_Sound (Menu_Beep_Sound, True);
            Result := True;
         end if;
      elsif Is_Key_Down (Left) or Is_Action_Down (Left_Action) then
         if Cursor_Item = 0 then
            Set_Graphic_Preset
              (Gfx_Preset_Type'Enum_Val
                 (Gfx_Preset_Type'Enum_Rep (Graphic_Preset) - 1));
            if Graphic_Preset < Gfx_Dire then
               Set_Graphic_Preset (Gfx_Ultra);
               Text.Update_Text (Graphic_Value_Text (Cursor_Item),
                                 Graphic_Preset_Strings
                                   (Gfx_Preset_Type'Enum_Rep (Graphic_Preset)));
               Result := Set_Menu_Graphic_Presets;
            end if;
         elsif Cursor_Item = 2 then
            Process_Video_Modes (Video_Mode, Cursor_Item,
                                 Graphic_Value_Text, False);
            Restart_Flag := True;
         end if;
         Audio.Play_Sound (Menu_Beep_Sound, True);

      elsif Is_Key_Down (Right) or Is_Action_Down (Right_Action) then
         if Cursor_Item = 0 then
            Set_Graphic_Preset
              (Gfx_Preset_Type'Enum_Val
                 (Gfx_Preset_Type'Enum_Rep (Graphic_Preset) + 1));
            if Graphic_Preset > Gfx_Ultra then
               Set_Graphic_Preset (Gfx_Dire);
               Text.Update_Text (Graphic_Value_Text (Cursor_Item),
                                 Graphic_Preset_Strings
                                   (Gfx_Preset_Type'Enum_Rep (Graphic_Preset)));
               Result := Set_Menu_Graphic_Presets;
            end if;
         elsif Cursor_Item = 2 then
            Process_Video_Modes (Video_Mode, Cursor_Item,
                                 Graphic_Value_Text, True);
            Restart_Flag := True;
         end if;
         Audio.Play_Sound (Menu_Beep_Sound, True);
      end if;  --  Line 1195
      return Result;
   end Process_Menu_Graphics;

   --  -------------------------------------------------------------------------

   procedure Process_Menu_Graphic_Cases (Cursor_Item : Integer := -1) is
      use Glfw.Input.Keys;
      use Input_Handler;
   begin
      case Cursor_Item is
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

   end Process_Menu_Graphic_Cases;

   --  -------------------------------------------------------------------------

   procedure Process_Video_Modes
     (Current_Mode       : in out Integer; Cursor_Item : Integer;
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
               Text.Update_Text (Graphic_Value_Text (Cursor_Item), New_Text);
            end;
         end if;
      end loop;
   end Process_Video_Modes;

   --  -------------------------------------------------------------------------

   procedure Process_Menu_Input (Window                    : in out Glfw.Windows.Window;
                                 Joy_Name                  : String;
                                 Since_Last_Key            : in out Float;
                                 Menu_Input_Open,
                                 Menu_Cal_Gp_Butts_Open    : in out Boolean;
                                 Joystick_Detected_Text    : Integer;
                                 Input_Cursor_Item : in out Integer) is
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
         case Input_Cursor_Item is
            when 0 => Settings.Disable_Joystick (not Settings.Joystick_Disabled);
            when 1 => Menu_Cal_KB_Open := True;
            when 2 | 3 =>
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
      elsif Is_Key_Down (Up) or Is_Action_Down (Up_Action) then
         Input_Cursor_Item := Input_Cursor_Item - 1;
         if Input_Cursor_Item < 0 then
            Input_Cursor_Item := Menu_Strings.Num_Input_Entries - 1;
         end if;
         Text.Update_Text (Joystick_Detected_Text,
                           "Joystick detected: " & Joy_Name);
         Since_Last_Key := 0.0;
         Audio.Play_Sound (Menu_Beep_Sound, True);
      elsif Is_Key_Down (Down) or Is_Action_Down (Down_Action) then
         Input_Cursor_Item := Input_Cursor_Item + 1;
         if Input_Cursor_Item >= Menu_Strings.Num_Input_Entries then
            Input_Cursor_Item := 0;
         end if;
         Text.Update_Text (Joystick_Detected_Text,
                           "Joystick detected: " & Joy_Name);
         Since_Last_Key := 0.0;
         Audio.Play_Sound (Menu_Beep_Sound, True);
      end if;
   end Process_Menu_Input;

   --  -------------------------------------------------------------------------

end Menu_Support;
