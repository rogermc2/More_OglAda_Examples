
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

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
    Cursor_Rot_Speed        : constant Float := 240.0; --  deg per sec
    Cursor_Degrees          : Maths.Degree := 0.0;

    Default_Axis_Values     : array (1 .. 16) of Float := (others => 0.0);
    Axis_Defaults_Mode      : Boolean := False;
    Graphics_Restart_Flag   : Boolean := False;
    Key_Processed           : Boolean := False;

    procedure Process_Menu_Graphic_Cases
      (Cursor_Item : Menu_Strings.Graphic_Choice_Type;
       Restart_Flag : in out Boolean;
       Graphic_Value_Text : Menu_Strings.Graphic_Value_Array);
    procedure Process_Video_Modes
      (Current_Mode       : in out Integer;
       Cursor_Item        : Menu_Strings.Graphic_Choice_Type;
       Graphic_Value_Text : Graphic_Value_Array; Increment : Boolean);
    procedure Set_Graphic_Presets
      (Graphic_Value_Text : Menu_Strings.Graphic_Value_Array);

    --  -------------------------------------------------------------------------

    procedure Check_Close_Menu_Credits (Window      : in out Barbarian_Window;
                                        Credits_Open, End_Story_Open,
                                        Menu_Closed : in out Boolean;
                                        Text_Timer  : in out Float) is
        use Glfw.Input.Keys;
        use Input_Handler;
    begin
        if Was_Key_Pressed (Window, Space) or Was_Key_Pressed (Window, Escape) or
          Was_Action_Pressed (Window, Menu_Open_Action) or
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
    end Check_Close_Menu_Credits;

    --  -------------------------------------------------------------------------

    function Confirm_Quit_Open (Window            : in out Barbarian_Window;
                                Confirm_Quit_Open : in out Boolean)
                                return Boolean is
        use Glfw.Input.Keys;
        use Input_Handler;
        Result : Boolean := False;
    begin
        Result := Was_Key_Pressed (Window, Escape) or
          Was_Action_Pressed (Window, Menu_Open_Action) or
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

    function Cycle_Up_Fac (IP : Integer) return Integer is
        Fac : Integer;
    begin
        case IP is
           when 0 => Fac := 1;
           when 16 => Fac := 1;
           when others => Fac := 2 * IP;
        end case;
        return Fac;
    end Cycle_Up_Fac;

    --  -------------------------------------------------------------------------

    procedure Do_Bounce (Title_Bounce_Timer : in out Float; Elapsed : Float;
                         Title_V            : in out GL.Types.Singles.Matrix4) is
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
            Y := Accel_Expand (Single (Title_Bounce_Timer), (Fall_Dist, 0.0),
                               ( 0.0, End_Fall_At));
            if Float (End_Fall_At) < Title_Bounce_Timer + Elapsed then
                Audio.Play_Sound (Title_Impact_Sound, False);
            end if;
        else
            Y := Decel_Bounce (Single (Title_Bounce_Timer), (Fall_Dist, 0.0),
                               ( 0.0, End_Fall_At), Num_Bounces);
        end if;
        Bounce_Matrix := Maths.Translation_Matrix ((0.0, Y, 0.0));

        Maths.Init_Lookat_Transform
          (Cam_Pos, Cam_Target, (0.0, 1.0, 1.0), Title_V);
        Title_V := Bounce_Matrix * Title_V;
        Title_Bounce_Timer := Title_Bounce_Timer + Elapsed;
    end Do_Bounce;

    --  -------------------------------------------------------------------------

    procedure Draw_Skull_Cursor
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
        Cursor_Degrees := Cursor_Degrees + Degree (Cursor_Rot_Speed * Elapsed);
        Rot_Matrix := Rotate_Y_Degree (Scale_Matrix, Cursor_Degrees);
        Cursor_M := Rot_Matrix;
        T_Matrix := Translation_Matrix ((Cursor_Pos (GL.X), Cursor_Pos (GL.Y), 0.0));
        P_Matrix := T_Matrix * Camera.GUI_Proj_Matrix;
        GL.Objects.Programs.Use_Program (Cursor_Shader_Program);
        Cursor_Shader_Manager.Set_Model_Matrix (Cursor_M);
        Cursor_Shader_Manager.Set_View_Matrix (Cursor_V);
        Cursor_Shader_Manager.Set_Perspective_Matrix (P_Matrix);
        Draw_Arrays (Triangles, 0, Int (Cursor_Point_Count));
    end Draw_Skull_Cursor;

    --  -------------------------------------------------------------------------

    procedure Enable_Strings
      (Condition : Boolean; Graphic_Value_Text : Menu_Strings.Graphic_Value_Array;
       Cursor_Item : Graphic_Choice_Type) is
    begin
        if Condition then
            Text.Update_Text (Graphic_Value_Text (Cursor_Item),
                              Enabled_Strings (Graphic_Strings_Enabled));
        else
            Text.Update_Text (Graphic_Value_Text (Cursor_Item),
                              Enabled_Strings (Graphic_Strings_Disabled));
        end if;
    end Enable_Strings;

    --  -------------------------------------------------------------------------

    procedure General_Menu_Support
      (Window                                       : in out Barbarian_Window;
       Joystick_Detected_Text                       : Integer;
       Joy_Name                                     : String;
       Menu_Was_Closed, Graphics_Open, Audio_Open,
       Input_Open, Confirm_Quit_Open,
       Credits_Open, New_Game, In_Custom_Map,
       Custom_Maps                                  : in out Boolean;
       Since_Last_Key                               : in out Float;
       Menu_Cursor_Item                             : in out Menu_Strings.Main_Choice_Type) is
        use Glfw.Input.Keys;
        use Input_Handler;
        use Menu_Strings;
        Result : Boolean := False;
    begin
        if Was_Key_Pressed (Window, Escape) or
          Was_Action_Pressed (Window, Menu_Open_Action) or
          Was_Action_Pressed (Window, Menu_Back_Action) then
            Result := Settings.Save_Settings;
            if Result then
                Menu_Was_Closed := True;
            else
                Game_Utils.Game_Log
                  ("Menu_Support.General_Menu_Support, could not save settings." );
            end if;

        elsif Is_Key_Down (Up) or
          Is_Action_Down (Up_Action) then
            if Menu_Cursor_Item = Main_New_Game then
                Menu_Cursor_Item := Main_Quit;
            else
                Menu_Cursor_Item := Main_Choice_Type'Pred (Menu_Cursor_Item);
            end if;
            Since_Last_Key := 0.0;
            Audio.Play_Sound (Menu_Beep_Sound, True);
            Result := True;
        elsif Is_Key_Down (Down) or
          Is_Action_Down (Down_Action) then
            if Menu_Cursor_Item = Main_Quit then
                Menu_Cursor_Item := Main_New_Game;
            else
                Menu_Cursor_Item := Main_Choice_Type'Succ (Menu_Cursor_Item);
            end if;
            Since_Last_Key := 0.0;
            Audio.Play_Sound (Menu_Beep_Sound, True);
            Result := True;

        elsif Was_Key_Pressed (Window, Enter) or
          Was_Action_Pressed (Window, OK_Action) or
          Was_Action_Pressed (Window, Attack_Action) then
            if Menu_Cursor_Item = Main_Quit then
                Confirm_Quit_Open := True;
                Result := Settings.Save_Settings;
                if not Result then
                    Game_Utils.Game_Log ("Menu_Support.General_Menu_Support, " &
                                           "ERROR: could not save settings from Mmenu");
                end if;
            elsif Menu_Cursor_Item = Main_Graphics then
                Graphics_Open := True;
                Result := True;
            elsif Menu_Cursor_Item = Main_Audio then
                Audio_Open := True;
                Result := True;
            elsif Menu_Cursor_Item = Main_Input then
                Input_Open := True;
                Text.Update_Text (Joystick_Detected_Text,
                                  "joystick detected: " & Joy_Name);
                Result := True;
            elsif Menu_Cursor_Item = Main_Credits then
                Credits_Open := True;
                Audio.Pause_Music (True);
                --              Audio.Play_Credits_Music (Credits_Music);
                Result := True;
            elsif Menu_Cursor_Item = Main_New_Game then
                New_Game := True;
                In_Custom_Map := False;
                Result := Settings.Save_Settings;
                if not Result then
                    Game_Utils.Game_Log ("Menu_Support.General_Menu_Support, " &
                                           "ERROR: could not save settings from Mmenu");
                end if;
            elsif Menu_Cursor_Item = Main_Custom_Map then
                Custom_Maps := True;
                In_Custom_Map := True;
                Result := Settings.Save_Settings;
                if not Result then
                    Game_Utils.Game_Log ("Menu_Support.General_Menu_Support, " &
                                           "ERROR: could not save settings from Mmenu");
                end if;
            end if;
        end if;
    end General_Menu_Support;

    --  -------------------------------------------------------------------------

    procedure Process_Menu_Cal_GP is
        Pos_Index : Integer := 1;
        Tex_Index : Integer := 1;
        --        Biggest_Diff   : Float := 0.0;
        --        Biggest_Diff_I : Integer := -1;
        --        Button_Pressed : Integer := -1;
        --        Sign           : Character := '-';
    begin
        GUI.Show_Controller_Button_Overlay (Pos_Index, Tex_Index);
        Pos_Index := 2;
        Tex_Index := 4;
        GUI.Show_Controller_Button_Overlay (Pos_Index, Tex_Index);
        Pos_Index := 3;
        Tex_Index := 2;
        GUI.Show_Controller_Button_Overlay (Pos_Index, Tex_Index);

        Axis_Defaults_Mode := True;
        --  Poll_Joystick;
        --  Process Joystick

    end Process_Menu_Cal_GP;

    --  -------------------------------------------------------------------------

    procedure Process_Menu_Cal_KB (Window                             : in out Barbarian_Window;
                                   KB_Binding_Text                    : GL_Maths.Integer_Array;
                                   Greatest_Axis_Text                 : Integer;
                                   Already_Bound_Text                 : Integer;
                                   Modify_Binding_Mode,
                                   Already_Bound, Menu_Cal_KB_Open    : in out Boolean;
                                   Since_Last_Key                     : in out Float) is
        use Glfw.Input.Keys;
        use Input_Handler;
        Pos_Index    : Integer := 1;
        Tex_Index    : Integer := 1;
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
          Was_Action_Pressed (Window, Menu_Open_Action) or
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

    procedure Process_Menu_Audio (Window             : in out Barbarian_Window;
                                  Audio_Value_Text   : Audio_Text_Array;
                                  Menu_Audio_Open    : in out Boolean;
                                  Since_Last_Key     : in out Float;
                                  Audio_Cursor_Item  : in out Audio_Choice_Type) is
        use Glfw.Input.Keys;
        use Input_Handler;
        use Menu_Support;
        use Menu_Strings;
        use Settings;
    begin
        if Was_Key_Pressed (Window, Escape) or
          Was_Action_Pressed (Window, Menu_Open_Action) or
          Was_Action_Pressed (Window, Menu_Back_Action) then
            Menu_Audio_Open := False;
            --  return
        elsif Was_Key_Pressed (Window, Enter) or
          Was_Action_Pressed (Window, OK_Action) or
          Was_Action_Pressed (Window, Attack_Action) then
            case Audio_Cursor_Item is
            when Audio_Strings_Master_Volume =>
                Set_Audio_Volume (Cycle_Up_PC (Audio_Volume));
                Audio.Set_Audio_Volume (Audio_Volume);
                Text.Update_Text (Audio_Value_Text (Audio_Cursor_Item),
                                  Integer'Image (10 * Audio_Volume));
                Audio.Play_Sound (Menu_Beep_Sound, True);
            when Audio_Strings_Music_Volume =>
                Set_Music_Volume (Cycle_Up_PC (Music_Volume));
                Audio.Set_Audio_Volume (Music_Volume);
                Text.Update_Text (Audio_Value_Text (Audio_Cursor_Item),
                                  Integer'Image (10 * Music_Volume));
                Audio.Play_Sound (Menu_Beep_Sound, True);
            when others => null;
            end case;

            if Is_Key_Down (Up) or Is_Action_Down (Up_Action) then
                if Audio_Cursor_Item = Audio_Choice_Type'First then
                    Audio_Cursor_Item := Audio_Choice_Type'Last;
                    Since_Last_Key := 0.0;
                    Audio.Play_Sound (Menu_Beep_Sound, True);
                else
                    Audio_Cursor_Item := Audio_Choice_Type'Pred (Audio_Cursor_Item);
                end if;

            elsif Is_Key_Down (Down) or Is_Action_Down (Down_Action) then
                if Audio_Cursor_Item = Audio_Choice_Type'Last then
                    Audio_Cursor_Item := Audio_Choice_Type'First;
                    Since_Last_Key := 0.0;
                    Audio.Play_Sound (Menu_Beep_Sound, True);
                else
                    Audio_Cursor_Item := Audio_Choice_Type'Succ (Audio_Cursor_Item);
                end if;

            elsif Is_Key_Down (Left) or Is_Action_Down (Left_Action) then
                if Audio_Cursor_Item = Audio_Strings_Master_Volume then
                    Set_Audio_Volume (Maths.Max_Integer (0, Audio_Volume - 1));
                    Text.Update_Text (Audio_Value_Text (Audio_Strings_Master_Volume),
                                      Integer'Image (10 * Audio_Volume));
                    Audio.Play_Sound (Menu_Beep_Sound, True);

                elsif Audio_Cursor_Item = Audio_Strings_Music_Volume then
                    Set_Music_Volume (Maths.Max_Integer (0, Music_Volume - 1));
                    Text.Update_Text (Audio_Value_Text (Audio_Strings_Music_Volume),
                                      Integer'Image (10 * Music_Volume));
                end if;

            elsif Is_Key_Down (Right) or Is_Action_Down (Right_Action) then
                if Audio_Cursor_Item = Audio_Strings_Master_Volume then
                    Set_Audio_Volume (Maths.Min_Integer (10, Audio_Volume + 1));
                    Text.Update_Text (Audio_Value_Text (Audio_Strings_Master_Volume),
                                      Integer'Image (10 * Audio_Volume));
                    Audio.Play_Sound (Menu_Beep_Sound, True);

                elsif Audio_Cursor_Item = Audio_Strings_Music_Volume then
                    Set_Music_Volume (Maths.Min_Integer (10, Music_Volume + 1));
                    Text.Update_Text (Audio_Value_Text (Audio_Strings_Music_Volume),
                                      Integer'Image (10 * Music_Volume));
                end if;
            end if;
        end if;
    end Process_Menu_Audio;

    --  -------------------------------------------------------------------------

    function Process_Menu_Graphics
      (Window                     : in out Barbarian_Window;
       Graphic_Value_Text         : Menu_Strings.Graphic_Value_Array;
       Menu_Gr_Open, Restart_Flag : in out Boolean;
       Since_Last_Key             : in out Float;
       Cursor_Item                : in out Menu_Strings.Graphic_Choice_Type;
       Video_Mode                 : in out Integer) return Boolean is
        use Glfw.Input.Keys;
        use Input_Handler;
        use Menu_Support;
        use Menu_Strings;
        Result : Boolean;
    begin
        Result := Was_Key_Pressed (Window, Escape) or
          Was_Action_Pressed (Window, Menu_Open_Action) or
          Was_Action_Pressed (Window, Menu_Back_Action);
        if Result then
            Menu_Gr_Open := False;
            --  return
        elsif Was_Key_Pressed (Window, Enter) or
          Was_Action_Pressed (Window, OK_Action) or
          Was_Action_Pressed (Window, Attack_Action) then
            Process_Menu_Graphic_Cases (Cursor_Item, Restart_Flag,
                                        Graphic_Value_Text);
            Audio.Play_Sound (Menu_Beep_Sound, True);
            Result := True;
            --  return
        elsif Is_Key_Down (Up) or Is_Action_Down (Up_Action) then
            if not Key_Processed then
                if Cursor_Item = Graphic_Choice_Type'First then
                    Cursor_Item := Graphic_Choice_Type'Last;
                    Since_Last_Key := 0.0;
                    Audio.Play_Sound (Menu_Beep_Sound, True);
                    Result := True;
                else
                    Cursor_Item := Graphic_Choice_Type'Pred (Cursor_Item);
                end if;
                Key_Processed := True;
            end if;
        elsif Is_Key_Down (Down) or Is_Action_Down (Down_Action) then
            if not Key_Processed then
                if Cursor_Item = Graphic_Choice_Type'Last then
                    Cursor_Item := Graphic_Choice_Type'First;
                    Since_Last_Key := 0.0;
                    Audio.Play_Sound (Menu_Beep_Sound, True);
                    Result := True;
                else
                    Cursor_Item := Graphic_Choice_Type'Succ (Cursor_Item);
                end if;
                Key_Processed := True;
            end if;
        elsif Is_Key_Down (Left) or Is_Action_Down (Left_Action) then
            if not Key_Processed then
                if Cursor_Item = Graphic_Presets then
                    if Settings.Graphic_Preset /= Graphic_Preset_Choice_Type'First then
                        Settings.Set_Graphic_Preset
                          (Graphic_Preset_Choice_Type'Pred (Settings.Graphic_Preset));
                    else
                        Settings.Set_Graphic_Preset (Graphic_Preset_Custom);
                    end if;
                    Text.Update_Text (Graphic_Value_Text (Cursor_Item),
                                      Graphic_Preset_Strings (Settings.Graphic_Preset));
                    Set_Graphic_Presets (Graphic_Value_Text);

                elsif Cursor_Item = Graphic_Windowed_Size then
                    Process_Video_Modes (Video_Mode, Cursor_Item, Graphic_Value_Text,
                                         False);
                    Restart_Flag := True;
                end if;
                Audio.Play_Sound (Menu_Beep_Sound, True);
                Key_Processed := True;
            end if;
        elsif Is_Key_Down (Right) or Is_Action_Down (Right_Action) then
            if not Key_Processed then
                if Cursor_Item = Graphic_Presets then
                    if Settings.Graphic_Preset /= Graphic_Preset_Choice_Type'Last then
                        Settings.Set_Graphic_Preset
                          (Graphic_Preset_Choice_Type'Succ (Settings.Graphic_Preset));
                    else
                        Settings.Set_Graphic_Preset (Graphic_Preset_Dire);
                    end if;
                    Text.Update_Text (Graphic_Value_Text (Cursor_Item),
                                      Graphic_Preset_Strings (Settings.Graphic_Preset));
                    Set_Graphic_Presets (Graphic_Value_Text);
                elsif Cursor_Item = Graphic_Windowed_Size then
                    Process_Video_Modes (Video_Mode, Cursor_Item,
                                         Graphic_Value_Text, True);
                    Restart_Flag := True;
                end if;
                Audio.Play_Sound (Menu_Beep_Sound, True);
                Key_Processed := True;
            end if;
        else
            Key_Processed := False;
        end if;  --  Line 1195
        return Result;
    end Process_Menu_Graphics;

    --  -------------------------------------------------------------------------

    procedure Process_Menu_Graphic_Cases (Cursor_Item : Graphic_Choice_Type;
                                          Restart_Flag : in out Boolean;
                                          Graphic_Value_Text : Menu_Strings.Graphic_Value_Array) is
        use Glfw.Input.Keys;
        use GL.Types;
        use Input_Handler;
        procedure Set_Custom is
        begin
            Settings.Set_Graphic_Preset (Graphic_Preset_Custom);
            Text.Update_Text (Graphic_Value_Text (Graphic_Choice_Type'First),
                              Graphic_Preset_Strings (Settings.Graphic_Preset));
        end Set_Custom;
    begin
        case Cursor_Item is
            when Graphic_Presets =>
                if Settings.Graphic_Preset = Graphic_Preset_Choice_Type'Last then
                    Settings.Set_Graphic_Preset (Graphic_Preset_Choice_Type'First);
                else
                    Settings.Set_Graphic_Preset
                      (Graphic_Preset_Choice_Type'Succ (Settings.Graphic_Preset));
                end if;
                Text.Update_Text (Graphic_Value_Text (Cursor_Item),
                                  Graphic_Preset_Strings (Settings.Graphic_Preset));
            when Graphic_Opengl_Version => null;
            when Graphic_Windowed_Size => null;
            when Graphic_Full_Screen =>
                Settings.Set_Full_Screen (not Settings.Full_Screen);
                Restart_Flag := True;
                Enable_Strings (Settings.Full_Screen, Graphic_Value_Text,
                                Cursor_Item);
--                  if Settings.Full_Screen then
--                      Text.Update_Text (Graphic_Value_Text (Cursor_Item),
--                                        Enabled_Strings (Graphic_Strings_Enabled));
--                  else
--                      Text.Update_Text (Graphic_Value_Text (Cursor_Item),
--                                        Enabled_Strings (Graphic_Strings_Disabled));
--                  end if;
            when Graphic_Vsync =>
                Settings.Set_V_Sync (not Settings.V_Sync);
                Restart_Flag := True;
                Enable_Strings (Settings.V_Sync, Graphic_Value_Text,
                                Cursor_Item);
            when Graphic_Shadows =>
                Settings.Set_Shadows_Enabled (not Settings.Shadows_Enabled);
                Restart_Flag := True;
                Enable_Strings (Settings.Shadows_Enabled, Graphic_Value_Text,
                                Cursor_Item);
                Set_Custom;
            when Graphic_Shadow_Size =>
                case Settings.Shadows_Size is
                    when 256 => Settings.Set_Shadow_Size (512);
                    when 512 => Settings.Set_Shadow_Size (1024);
                    when 1024 => Settings.Set_Shadow_Size (2048);
                    when 2048 => Settings.Set_Shadow_Size (256);
                    when others =>
                        raise Menu_Support_Error with
                          "Menu_Support.Process_Menu_Graphic_Cases invalid Shadows_Size: "
                          & Integer'Image (Settings.Shadows_Size);
                end case;
            when Graphic_Outlines =>
                Settings.Set_Render_OLS (not Settings.Render_OLS);
                Enable_Strings (Settings.Render_OLS, Graphic_Value_Text,
                                Cursor_Item);
                Set_Custom;
            when Graphic_Framebuffer_Fx =>
                Settings.Set_FB_Effects_Enabled (not Settings.Fb_Effects_Enabled);
                Enable_Strings (Settings.Fb_Effects_Enabled, Graphic_Value_Text,
                                Cursor_Item);
                Set_Custom;
            when Graphic_Texture_Filter =>
                case Settings.Texture_Filter is
                    when 0 => Settings.Set_TexF (1);
                    when 1 => Settings.Set_TexF (2);
                    when 2 => Settings.Set_TexF (0);
                    when others =>
                        raise Menu_Support_Error with
                          "Menu_Support.Process_Menu_Graphic_Cases invalid TexF: "
                          & Integer'Image (Settings.Texture_Filter);
                end case;
            when Graphic_Anisotropy => null;
            when Graphic_Msaa =>
                Settings.Set_MSAA
                  (Cycle_Up_Fac (Settings.Multi_Sample_Anti_Aliasing));
                Text.Update_Text
                  (Graphic_Value_Text (Cursor_Item),
                   Integer'Image (Settings.Multi_Sample_Anti_Aliasing));
            when Graphic_Ssaa =>
                if Settings.Super_Sample_Anti_Aliasing < 4.0 then
                    Settings.Set_SSAA (float (2.0 * Settings.Super_Sample_Anti_Aliasing));
                else
                    Settings.Set_SSAA (0.5);
                end if;
                Text.Update_Text
                  (Graphic_Value_Text (Cursor_Item),
                   Single'Image (Settings.Super_Sample_Anti_Aliasing));
                Restart_Flag := True;
                Set_Custom;
            when Graphic_Render_Dist =>
                if Settings.Render_Distance < 19 then
                    Settings.Set_Render_Distance (Settings.Render_Distance + 2);
                else
                    Settings.Set_Render_Distance (5);
                end if;
                Text.Update_Text (Graphic_Value_Text (Cursor_Item),
                                  Integer'Image (Settings.Render_Distance));
                Set_Custom;
            when Graphic_Far_Clip =>
                if Settings.Far_Clip < 46.0 then
                    Settings.Set_Far_Clip (Settings.Far_Clip + 5.0);
                else
                    Settings.Set_Far_Clip (5.0);
                end if;
                Text.Update_Text (Graphic_Value_Text (Cursor_Item),
                                  Single'Image (Settings.Far_Clip));
                Camera.Recalculate_Perspective
                  (Camera.Field_Of_View_Y, Single (Settings.Framebuffer_Width),
                   Single (Settings.Framebuffer_Height), Camera.Near, Camera.Far);
                Set_Custom;
            when Graphic_Auto_Blood_Wipe =>
                Settings.Set_Autoblood_Wipe (not Settings.Auto_Blood_Wipe);
                Enable_Strings (Settings.Auto_Blood_Wipe, Graphic_Value_Text,
                                Cursor_Item);
            when Graphic_Show_Fps =>
                Settings.Set_Show_FPS (not Settings.Show_FPS);
                Enable_Strings (Settings.Show_FPS, Graphic_Value_Text,
                                Cursor_Item);
        end case;
    end Process_Menu_Graphic_Cases;

    --  -------------------------------------------------------------------------

    procedure Process_Video_Modes
      (Current_Mode       : in out Integer;
       Cursor_Item        : Menu_Strings.Graphic_Choice_Type;
       Graphic_Value_Text : Graphic_Value_Array; Increment : Boolean) is
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

    procedure Process_Menu_Input (Window                    : in out Barbarian_Window;
                                  Joy_Name                  : String;
                                  Since_Last_Key            : in out Float;
                                  Menu_Input_Open, Menu_Cal_KB_Open,
                                  Menu_Cal_Gp_Axes_Open,
                                  Menu_Cal_Gp_Butts_Open    : in out Boolean;
                                  Joystick_Detected_Text    : Integer;
                                  Input_Cursor_Item         : in out Input_Choice_Type) is
        use Glfw.Input.Keys;
        use Input_Handler;
    begin
        if Was_Key_Pressed (Window, Escape) or
          Was_Action_Pressed (Window, Menu_Open_Action) or
          Was_Action_Pressed (Window, Menu_Back_Action) then
            Menu_Input_Open := False;
            Text.Update_Text (Joystick_Detected_Text,
                              "joystick detected: " & Joy_Name);
        elsif Was_Key_Pressed (Window, Enter) or
          Was_Action_Pressed (Window, OK_Action) or
          Was_Action_Pressed (Window, Attack_Action) then
            case Input_Cursor_Item is
            when Input_Gamepad_Joystick =>
                Settings.Set_Disable_Joystick (not Settings.Joystick_Disabled);
            when Input_Calibrate_Keyboard => Menu_Cal_KB_Open := True;
            when Input_Calibrate_Gamepad_Buttons =>
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
            when Input_Calibrate_Gamepad_Axes_Triggers =>
                if Joystick_Connected and not Settings.Joystick_Disabled then
                    Menu_Cal_Gp_Axes_Open := True;
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
            if Input_Cursor_Item = Input_Choice_Type'First then
                Input_Cursor_Item := Input_Choice_Type'Last;
            else
                Input_Cursor_Item := Input_Choice_Type'Pred (Input_Cursor_Item);
            end if;

            Text.Update_Text (Joystick_Detected_Text,
                              "Joystick detected: " & Joy_Name);
            Since_Last_Key := 0.0;
            Audio.Play_Sound (Menu_Beep_Sound, True);
        elsif Is_Key_Down (Down) or Is_Action_Down (Down_Action) then
            if Input_Cursor_Item = Input_Choice_Type'Last then
                Input_Cursor_Item := Input_Choice_Type'First;
            else
                Input_Cursor_Item := Input_Choice_Type'Succ (Input_Cursor_Item);
            end if;
            Text.Update_Text (Joystick_Detected_Text,
                              "Joystick detected: " & Joy_Name);
            Since_Last_Key := 0.0;
            Audio.Play_Sound (Menu_Beep_Sound, True);
        end if;
    end Process_Menu_Input;

    --  -------------------------------------------------------------------------

    procedure Set_Graphic_Presets
      (Graphic_Value_Text : Menu_Strings.Graphic_Value_Array) is
        use GL.Types;
        use Settings;
        Aspect : constant Float :=
                   Float (Framebuffer_Width) / Float (Framebuffer_Height);
    begin
        case Graphic_Preset is
            when Graphic_Preset_Dire =>
                Set_MSAA (1);
                Set_SSAA (0.5);
                Set_TexF (0);
                Set_Aniso (1);
                Set_Render_Distance (7);
                Set_Far_Clip (15.0);
                Set_Render_OLS (False);
                Set_Shadows_Enabled (False);
                Set_Shadow_Size (256);
                Set_FB_Effects_Enabled (True);
            when Graphic_Preset_Low =>
                Set_MSAA (1);
                Set_SSAA (1.0);
                Set_TexF (2);
                Set_Aniso (1);
                Set_Render_Distance (9);
                Set_Far_Clip (25.0);
                Set_Render_OLS (False);
                Set_Shadows_Enabled (True);
                Set_Shadow_Size (256);
                Set_FB_Effects_Enabled (True);
            when Graphic_Preset_Medium =>
                Set_MSAA (1);
                Set_SSAA (2.0);
                Set_TexF (2);
                Set_Aniso (4);
                Set_Render_Distance (15);
                Set_Far_Clip (40.0);
                Set_Render_OLS (True);
                Set_Shadows_Enabled (True);
                Set_Shadow_Size (512);
                Set_FB_Effects_Enabled (True);
            when Graphic_Preset_High =>
                Set_MSAA (4);
                Set_SSAA (2.0);
                Set_TexF (2);
                Set_Aniso (16);
                Set_Render_Distance (17);
                Set_Far_Clip (45.0);
                Set_Render_OLS (True);
                Set_Shadows_Enabled (True);
                Set_Shadow_Size (1024);
                Set_FB_Effects_Enabled (True);
            when Graphic_Preset_Ultra =>
                Set_MSAA (4);
                Set_SSAA (2.0);
                Set_TexF (2);
                Set_Aniso (16);
                Set_Render_Distance (17);
                Set_Far_Clip (45.0);
                Set_Render_OLS (True);
                Set_Shadows_Enabled (True);
                Set_Shadow_Size (1024);
                Set_FB_Effects_Enabled (True);
            when Graphic_Preset_Custom =>
                null;
        end case;

        Text.Update_Text (Graphic_Value_Text (Graphic_Shadows),
                          Enabled_Strings (Graphic_Strings_Enabled));
        Text.Update_Text (Graphic_Value_Text (Graphic_Shadow_Size),
                          Integer'Image (Shadows_Size));
        Enable_Strings (Render_OLS, Graphic_Value_Text, Graphic_Outlines);
--          if Render_OLS then
--              Text.Update_Text (Graphic_Value_Text (Graphic_Outlines),
--                                Enabled_Strings (Graphic_Strings_Enabled));
--          else
--              Text.Update_Text (Graphic_Value_Text (Graphic_Outlines),
--                                Enabled_Strings (Graphic_Strings_Disabled));
--          end if;
        Enable_Strings (Fb_Effects_Enabled, Graphic_Value_Text,
                        Graphic_Framebuffer_Fx);
--          if Fb_Effects_Enabled then
--              Text.Update_Text (Graphic_Value_Text (Graphic_Framebuffer_Fx),
--                                Enabled_Strings (Graphic_Strings_Enabled));
--          else
--              Text.Update_Text (Graphic_Value_Text (Graphic_Framebuffer_Fx),
--                                Enabled_Strings (Graphic_Strings_Disabled));
--          end if;

        Text.Update_Text (Graphic_Value_Text (Graphic_Texture_Filter),
                          Texture_Filter_Strings
                            (Texture_Filter_Choice_Type'Enum_Val (Texture_Filter)));
        Texture_Manager.Refilter_Textures;

        Text.Update_Text (Graphic_Value_Text (Graphic_Anisotropy),
                          Integer'Image (Anisotroic_Texturing_Factor));
        Text.Update_Text (Graphic_Value_Text (Graphic_Msaa),
                          Integer'Image (Multi_Sample_Anti_Aliasing));
        Text.Update_Text (Graphic_Value_Text (Graphic_Ssaa),
                          Single'Image (Super_Sample_Anti_Aliasing));
        Text.Update_Text (Graphic_Value_Text (Graphic_Render_Dist),
                          Integer'Image (Render_Distance));
        Text.Update_Text (Graphic_Value_Text (Graphic_Far_Clip),
                          Single'Image (Far_Clip));

        Camera.Recalculate_Perspective
          (Camera.Field_Of_View_Y, Single (Framebuffer_Width),
           Single (Framebuffer_Height), Camera.Near, Far_Clip);

        Enable_Strings (Auto_Blood_Wipe, Graphic_Value_Text,
                        Graphic_Auto_Blood_Wipe);
--          if Auto_Blood_Wipe then
--              Text.Update_Text (Graphic_Value_Text (Graphic_Auto_Blood_Wipe),
--                                Enabled_Strings (Graphic_Strings_Enabled));
--          else
--              Text.Update_Text (Graphic_Value_Text (Graphic_Auto_Blood_Wipe),
--                                Enabled_Strings (Graphic_Strings_Disabled));
--          end if;
        Graphics_Restart_Flag := True;

    end Set_Graphic_Presets;

    --  -------------------------------------------------------------------------

end Menu_Support;

