

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with GL.Attributes;
with GL.Culling;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types; use GL.Types;

with Input_Callback;
with Maths;
with Utilities;

with Custom_Levels_Manager;
with Game_Utils;
with GL_Utils;
with GUI;
with Input_Handler;
with Level_Menu_Manager;
with Maps_Manager;
with Menu_Credits_Shader_Manager;
with Main_Menu;
with Selected_Level_Manager;
with Settings;
with Text;
with Texture_Manager;

package body GUI_Level_Chooser is
    use Selected_Level_Manager;

    Quad_VAO                  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Quad_VBO                  : GL.Objects.Buffers.Buffer;
    Back_Texture              : GL.Objects.Textures.Texture;
    Back_Custom_Texture       : GL.Objects.Textures.Texture;
    Custom_Levels             : Custom_Levels_Manager.Custom_Levels_List;
    Num_Custom_Levels         : Natural := 0;
    Levels                    : Level_Menu_Manager.Levels_List;
    Selected_Level_ID         : Positive := 1;
    Selected_Level            : Selected_Level_Data;
    Selected_Level_Track      : Unbounded_String := To_Unbounded_String ("");
    Left_Margin_Cl            : Single := 0.0;
    Top_Margin_Cl             : Single := 0.0;
    Level_GUI_Width           : Single := 1024.0;
    Level_GUI_Height          : Single := 768.0;

    Choose_Level_Text_ID      : Integer := -1;
    Level_Title_Text_ID       : Integer := -1;
    Level_Story_Text_ID       : GL_Utils.Integer_List;
    Loading_Map_Text_ID       : Integer := -1;

    Cheated                   : Boolean := False;
    Level_Unmodified          : Boolean := True;
    Pillar_Crushes            : Integer := 0;
    Boulder_Crushes           : Integer := 0;
    Hammer_Kills              : Integer := 0;
    Fall_Kills                : Integer := 0;
    Since_Last_Key            : Float := 0.0;

    function Get_Map_Checksum (Map_Name : String) return Int;
    procedure Set_Background_Pane
      (Credits_Shader_Program : GL.Objects.Programs.Program;
       Use_Custom_Maps        : Boolean);
    procedure Update_GUI_Level_Chooser (Delta_Time : Float; Custom_Maps : Boolean);
    procedure Update_Selected_Entry_Level (First, Custom : Boolean);

    --  ------------------------------------------------------------------------

    function Cheated_On_Map return Boolean is
    begin
        return Cheated;
    end Cheated_On_Map;

    --  ------------------------------------------------------------------------

    function Checksums return Boolean is
        Map_Sums : constant Int_Array (1 .. 8) :=
                     (797678,  -- orig 800948,  --  intro
                      1740057, -- orig 1744957 threedoors
                      869974,  --  warlock
                      1014716, --  winder
                      1224821, --  under
                      917647,  --  skytemple
                      1534173, -- 1536666, --  hall
                      901523   --  orig 905383   --  attercoppe
                     );
        Map_Name : constant String := Get_Selected_Level_Name (False);
        Sum      : constant Int := Get_Map_Checksum (Map_Name);
        OK       : Boolean := False;
    begin
        OK := Sum = Map_Sums (Int (Selected_Level_ID));
        if not OK then
            Level_Unmodified := False;
            Game_Utils.Game_Log ("GUI_Level_Chooser.Checksums: map checksum " &
                                   Int'Image (Sum) & " for " & Map_Name &
                                   " did not match record - disabling.");
        end if;
        return OK;
    end Checksums;

    --  ------------------------------------------------------------------------

    procedure Draw_Custom_Level_Names is
    begin
        for index in Custom_Levels.First_Index .. Custom_Levels.Last_Index loop
            Text.Draw_Text (Custom_Levels.Element (index).Text_ID);
        end loop;
    end Draw_Custom_Level_Names;

    --  ------------------------------------------------------------------------

    procedure Draw_Level_Names is
        Last_Index : Positive := Levels.Last_Index;
    begin
        if Last_Index > 8 then
            Last_Index := 8;
         end if;
        for index in Levels.First_Index .. Last_Index loop
            Text.Draw_Text (Levels.Element (index).Name_Text_ID);
        end loop;
    end Draw_Level_Names;

    --  ------------------------------------------------------------------------

    function Get_Hammer_Kills return Integer is
    begin
        return Hammer_Kills;
    end Get_Hammer_Kills;

    --  ------------------------------------------------------------------------

    function Get_Map_Checksum (Map_Name : String) return Int is
        With_Path   : constant String := "src/maps/" & Map_Name;
        Map_File    : File_Type;
        Sum         : Integer := 0;
    begin
        Open (Map_File, In_File, With_Path);
        while not End_Of_File (Map_File) loop
            declare
                aLine    : constant String := Get_Line (Map_File);
                S_Length : constant Integer := aLine'Length;
            begin
                for index in 1 .. S_Length loop
                    Sum := Sum + Character'Pos (aLine (index));
                end loop;
            end;  --  declare block
        end loop;
        Close (Map_File);
        return Int (Sum);
    end Get_Map_Checksum;

    --  ------------------------------------------------------------------------

    function Get_Selected_Level_Music return String is
    begin
        return To_String (Selected_Level_Track);
    end Get_Selected_Level_Music;

    --  ------------------------------------------------------------------------

    function Get_Selected_Level_Name (Custom : Boolean) return String is
        Result  : Unbounded_String := To_Unbounded_String ("");
    begin
        --        Put_Line ("GUI_Level_Chooser.Get_Selected_Map_Name, Selected_Level_ID: " &
        --                 Integer'Image (Selected_Level_ID));
        if Custom then
            Result := To_Unbounded_String (Custom_Levels_Manager.Get_Custom_Map_Name
                                           (Custom_Levels, Selected_Level_ID));
        else
            Result := To_Unbounded_String (Level_Menu_Manager.Get_Level_Name
                                           (Levels, Selected_Level_ID));
        end if;

        return To_String (Result);
    end Get_Selected_Level_Name;

    --  ------------------------------------------------------------------------

    procedure Increment_Hammer_Kills is
    begin
        Hammer_Kills := Hammer_Kills + 1;
    end Increment_Hammer_Kills;

    --  ------------------------------------------------------------------------

    procedure Init is
        use GL.Types;
        use Settings;
        Text_Height     : constant Single :=
                            50.0 / Single (Settings.Framebuffer_Height);
        Quad_Points     : constant Singles.Vector2_Array (1 .. 4) :=
                            ((-1.0, -1.0), (-1.0, 1.0), (1.0, -1.0), (1.0, 1.0));
    begin
        Game_Utils.Game_Log ("GUI_Level_Chooser.Init ...");
        if Framebuffer_Width < 800 or Framebuffer_Height < 600 then
            Level_GUI_Width := 512.0;
            Level_GUI_Height := 512.0;
            Game_Utils.Game_Log
              ("Level gui menu size reduced to medium (width < 800px).");
        elsif Framebuffer_Width < 1024 or Framebuffer_Height < 768 then
            Level_GUI_Width := 800.0;
            Level_GUI_Height := 600.0;
            Game_Utils.Game_Log
              ("Level gui menu size reduced to medium (width < 1024px).");
        end if;

        Left_Margin_Cl := -(Level_GUI_Width - 40.0) / Single (Framebuffer_Width);
        Top_Margin_Cl := (Level_GUI_Height - 40.0) / Single (Framebuffer_Height);

        --      if not Game_Utils.Is_Little_Endian then
        --              Put_Line ("GUI_Level_Chooser.Init!");
        --              Put ("WARNING: unsupported big-endian system detected "
        --              Put_Line ("game may cease to function at this point.");
        --              Put_Line ("please notify game designer and provide your system specs.");
        --      end if;
        --        Game_Utils.Game_Log ("GUI_Level_Chooser loading maps from " &
        --                               "src/save/maps.dat");
        Level_Menu_Manager.Load_Story_Names ("src/save/maps.dat", Levels);
        Level_Menu_Manager.Init_Levels (Levels, Selected_Level_ID,
                                        Left_Margin_Cl, Top_Margin_Cl);
        Update_Selected_Entry_Level (First => True, Custom => False);

        Choose_Level_Text_ID :=
          Text.Add_Text ("choose thy battle!", 0.0, Single (Top_Margin_Cl),
                         30.0, 1.0, 1.0, 0.0, 0.8);
        Text.Centre_Text (Choose_Level_Text_ID, 0.0, Single (Top_Margin_Cl));
        Text.Set_Text_Visible (Choose_Level_Text_ID, False);

        Custom_Levels_Manager.Load_Custom_Map
          ("src/editor/maps.txt", Custom_Levels, Top_Margin_Cl,
           Left_Margin_Cl, Text_Height, Num_Custom_Levels);
        Loading_Map_Text_ID := Text.Create_Text_Box
          ("loading map...", -0.1, 0.0, 25.0,
           (0.0, 0.0, 0.0, 1.0),  (0.800, 0.795, 0.696, 1.0));
        Text.Set_Text_Visible (Loading_Map_Text_ID, False);

        Quad_VAO.Initialize_Id;
        Quad_VAO.Bind;
        Quad_VBO := GL_Utils.Create_2D_VBO (Quad_Points);
        GL.Objects.Buffers.Array_Buffer.Bind (Quad_VBO);
        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 2, Single_Type, False, 0, 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (0);

        Texture_Manager.Load_Image_To_Texture
          ("src/textures/gui_level_chooser_back.png", Back_Texture, False, True);
        Texture_Manager.Load_Image_To_Texture
          ("src/textures/map_2048.png", Back_Custom_Texture, False, True);

        Game_Utils.Game_Log ("GUI_Level_Chooser initialized");
    exception
        when others =>
            Put_Line ("An exception occurred in GUI_Level_Chooser.Init.");
            raise;
    end Init;

    --  ------------------------------------------------------------------------

    function Is_Level_Introduction return Boolean is
    begin
        return not Main_Menu.Are_We_In_Custom_Maps and
          Selected_Level.Level_Type = Level_Introduction;
    end Is_Level_Introduction;

    --  ------------------------------------------------------------------------

    function Is_Level_Warlock return Boolean is
    begin
        return not Main_Menu.Are_We_In_Custom_Maps and
          Selected_Level.Level_Type = Level_Warlock;
    end Is_Level_Warlock;

    --  ------------------------------------------------------------------------

    function Level_Is_Unmodified return Boolean is
    begin
        return Level_Unmodified;
    end Level_Is_Unmodified;

    --  ------------------------------------------------------------------------

    procedure Process_Input (Window       : in out Input_Callback.Barbarian_Window;
                             Menu_Open, Started_Loading_Map,
                             Cheat_Unlock : in out Boolean) is
        use Glfw.Input.Keys;
        use Input_Callback;
        use Input_Handler;
        Selected_Level : constant Level_Menu_Manager.Level_Data :=
                           Levels.Element (Selected_Level_ID);
        Enter_Pressed  : constant Boolean := Was_Key_Pressed (Window, Enter);
        OK_Pressed     : constant Boolean := Was_Action_Pressed (Window, OK_Action);
        Attack_Pressed : constant Boolean := Was_Action_Pressed (Window, Attack_Action);
        State_1        : Boolean;
    begin
        Put_Line ("Process_Input Enter: " & Boolean'Image (Enter_Pressed));
        Put_Line ("Process_Input OK_Action: " & Boolean'Image (OK_Pressed));
        Put_Line ("Process_Input Attack_Action: " & Boolean'Image (Attack_Pressed));
        State_1 := Enter_Pressed or OK_Pressed or Attack_Pressed;
        Put_Line ("Process_Input State_1: " &  Boolean'Image (State_1));
        New_Line;
        if State_1 then
            if not Selected_Level.Locked or Cheat_Unlock then
              Put_Line ("Process_Input Key or Action pressed Enter: " &
              Boolean'Image (Was_Key_Pressed (Window, Enter)) & " OK_Action: " &
              Boolean'Image (Was_Action_Pressed (Window, OK_Action)) &
              " Attack_Action: " & Boolean'Image (Was_Action_Pressed (Window, Attack_Action)));
                Started_Loading_Map := True;
                --              else
                --                  Game_Utils.Game_Log ("Process_Input Enter Selected_Level, Locked: "
                --                                       & Integer'Image (Selected_Level_ID) & "  "
                --                                       & To_String (Selected_Level.Level_Name) & "  "
                --                                        & Boolean'Image (Selected_Level.Locked));
            end if;
        elsif Was_Key_Pressed (Window, Escape) or
          Was_Action_Pressed (Window, Menu_Open_Action) then
            Menu_Open := True;
        elsif Is_Key_Down (A) and Is_Key_Down (N)  and Is_Key_Down (D) then
            if not Cheat_Unlock then
                --              Play_Sound (GONG_SOUND_FILE, true);
                Cheat_Unlock := True;
                Game_Utils.Game_Log ("cheater!");
            end if;
        end if;
        Input_Callback.Clear_All_Keys;
        Input_Callback.Set_Key_Pressed (False);
    end Process_Input;

    --  ------------------------------------------------------------------------

    procedure Render_Level_Menu (Window  : in out Input_Callback.Barbarian_Window;
                                 Credits_Shader_Program  : GL.Objects.Programs.Program;
                                 Delta_Time  : Float;
                                 Use_Custom_Levels, Started_Loading_Map,
                                 Level_Menu_Open : Boolean) is
        use Level_Menu_Manager.Levels_Package;
        use Custom_Levels_Manager.Custom_Levels_Package;
        LeveL_Menu    : Level_Menu_Manager.Level_Data;
        Custom_Level  : Custom_Levels_Manager.Custom_Data;
        Last_Index    : Positive := Levels.Last_Index;
    begin
        Utilities.Clear_Colour;
        --  Set_Background_Pane does the rendering
        Set_Background_Pane (Credits_Shader_Program, Use_Custom_Levels);

        if Use_Custom_Levels then
            Draw_Custom_Level_Names;
        else
            Draw_Level_Names;
        end if;

        Text.Draw_Text (Choose_Level_Text_ID);
        Text.Draw_Text (Level_Title_Text_ID);
        for index in Level_Story_Text_ID.First_Index .. Level_Story_Text_ID.Last_Index loop
            Text.Draw_Text (Level_Story_Text_ID.Element (index));
        end loop;

        Text.Draw_Text (Choose_Level_Text_ID);
        if Started_Loading_Map then
            Text.Draw_Text (Loading_Map_Text_ID);
        end if;

        if Level_Menu_Open then
            Main_Menu.Draw_Menu (Delta_Time);
        end if;

        GUI.Draw_Controller_Button_Overlays (Delta_Time);
        Glfw.Windows.Context.Swap_Buffers (Window'Access);

    end Render_Level_Menu;

    --  ------------------------------------------------------------------------

    procedure Reset_GUI_Level_Selection (Custom : Boolean) is
        use Level_Menu_Manager.Levels_Package;
        use Custom_Levels_Manager.Custom_Levels_Package;
        Map_Cursor        : Level_Menu_Manager.Levels_Package.Cursor :=
                              Levels.First;
        Custom_Map_Cursor : Custom_Levels_Manager.Custom_Levels_Package.Cursor :=
                              Custom_Levels.First;
        aLevel            : Level_Menu_Manager.Level_Data;
        Custom_Map        : Custom_Levels_Manager.Custom_Data;
    begin
        Selected_Level_ID := 1;
        --  Set map colour to white
        if Custom then
            while Has_Element (Custom_Map_Cursor) loop
                Custom_Map := Element (Custom_Map_Cursor);
                Text.Change_Text_Colour (Custom_Map.Text_ID, 1.0, 1.0, 1.0, 1.0);
                Custom_Levels.Replace_Element (Custom_Map_Cursor, Custom_Map);
                Next (Custom_Map_Cursor);
            end loop;
        else
            while Has_Element (Map_Cursor) and then To_Index (Map_Cursor) < 9 loop
                aLevel := Element (Map_Cursor);
                Text.Change_Text_Colour (aLevel.Name_Text_ID, 1.0, 1.0, 1.0, 1.0);
                Levels.Replace_Element (Map_Cursor, aLevel);
                Next (Map_Cursor);
            end loop;
        end if;
        Update_Selected_Entry_Level (False, Custom);

    end Reset_GUI_Level_Selection;

    --  ------------------------------------------------------------------------

    procedure Set_Background_Pane
      (Credits_Shader_Program : GL.Objects.Programs.Program;
       Use_Custom_Maps        : Boolean) is
        use GL.Culling;
        use GL.Toggles;
        use GL.Types;
        use Maths.Single_Math_Functions;
        Aspect     : constant Single := Single (Settings.Framebuffer_Width) /
                       Single (Settings.Framebuffer_Height);
        Now        : constant Single := Single (Glfw.Time);
        Sx         : constant Single := 2.0;
        Sy         : constant Single := 2.0 * Aspect;
        Px         : constant Single := Sin (0.15 * Now);
        Py         : constant  Single := -1.5 * Cos (0.09 * Now);
    begin
        GL.Objects.Programs.Use_Program (Credits_Shader_Program);
        Menu_Credits_Shader_Manager.Set_Scale ((Sx, Sy));
        Menu_Credits_Shader_Manager.Set_Position ((Px, Py));
        GL_Utils.Bind_VAO (Quad_VAO);
        if Use_Custom_Maps then
            Texture_Manager.Bind_Texture (0, Back_Texture);
        else
            Texture_Manager.Bind_Texture (0, Back_Custom_Texture);
        end if;
        Disable (Cull_Face);
        Disable (Depth_Test);

        GL.Objects.Vertex_Arrays.Draw_Arrays (Triangle_Strip, 0, 4);
        Enable (Cull_Face);
    end Set_Background_Pane;

    --  ------------------------------------------------------------------------

    procedure Set_Boulder_Crushes (Value : Integer) is
    begin
        Boulder_Crushes := Value;
    end Set_Boulder_Crushes;

    --  ------------------------------------------------------------------------

    procedure Set_Cheated_On_Map (State : Boolean) is
    begin
        Cheated := State;
    end Set_Cheated_On_Map;

    --  ------------------------------------------------------------------------

    procedure Set_Fall_Kills (Value : Integer) is
    begin
        Fall_Kills := Value;
    end Set_Fall_Kills;

    --  ------------------------------------------------------------------------

    procedure Set_Hammer_Kills (Value : Integer) is
    begin
        Hammer_Kills := Value;
    end Set_Hammer_Kills;

    --  ------------------------------------------------------------------------

    procedure Set_Pillar_Crushes (Value : Integer) is
    begin
        Pillar_Crushes := Value;
    end Set_Pillar_Crushes;

    --  ------------------------------------------------------------------------

    function Start_Level_Chooser_Loop
      (Window                 : in out Input_Callback.Barbarian_Window;
       Credits_Shader_Program : GL.Objects.Programs.Program;
       Custom_Levels          : Boolean) return Boolean is
        use GL.Toggles;
        Level_Menu_Open       : Boolean := Main_Menu.End_Story_Open;
        Level_Menu_Quit       : Boolean := False;
        Cheat_Unlock          : Boolean := False;
        Started_Loading_Map   : Boolean := False;
        Current_Time          : Float;
        Delta_Time            : Float;
        Last_Time             : Float := Float (Glfw.Time);
        Break                 : Boolean := False;
        Continue              : Boolean := True;
        Result                : Boolean := True;
        Selected_Level        : constant Level_Menu_Manager.Level_Data :=
                                  Levels.Element (Selected_Level_ID);
    begin
        Game_Utils.Game_Log ("Start_Level_Chooser_Loop Selected_Level:" &
                               Integer'Image (Selected_Level_ID) & "  " &
                               To_String (Selected_Level.Level_Name) &
                               ", Level_Menu_Open  " &
                               Boolean'Image (Level_Menu_Open));
        Put_Line ("Start_Level_Chooser_Loop Selected_Level:" &
                               Integer'Image (Selected_Level_ID) & "  " &
                               To_String (Selected_Level.Level_Name) &
                               ", Level_Menu_Open  " &
                               Boolean'Image (Level_Menu_Open));
        Glfw.Input.Poll_Events;
--          Input_Callback.Clear_All_Keys;
--          Input_Callback.Set_Key_Pressed (False);
        Reset_GUI_Level_Selection (Custom_Levels);
        while not Window.Should_Close and Continue and not Break loop
            Current_Time := Float (Glfw.Time);
            Delta_Time := Current_Time - Last_Time;
            Last_Time := Current_Time;
            if Level_Menu_Open then
               Put_Line ("Start_Level_Chooser_Loop Level_Menu_Open");
                Level_Menu_Quit :=
                  not Main_Menu.Update_Main_Menu (Window, Delta_Time);
                Level_Menu_Open := not Main_Menu.Menu_Was_Closed;

                if Main_Menu.Did_User_Choose_New_Game or
                  Main_Menu.Did_User_Choose_Custom_Maps then
                    Put_Line ("Start_Level_Chooser_Loop User_Choose_New_Game" );
                    Level_Menu_Open := False;
--                      Input_Callback.Clear_All_Keys;
--                      Input_Callback.Set_Key_Pressed (False);
                    Result := Start_Level_Chooser_Loop
                      (Window, Credits_Shader_Program, False);
                    Continue := False;
                elsif Level_Menu_Quit then
                    Continue := False;
                end if;  --  Level_Menu_Open
            else --  Level_Menu not Open
               Put_Line ("GUI_Level_Chooser.Start_Level_Chooser_Loop Update_GUI_Level_Chooser");
                Update_GUI_Level_Chooser (Delta_Time, Custom_Levels);
                New_Line;
            end if;  --  Level_Menu_Open

            if Continue then
                Started_Loading_Map := False;
                if not Level_Menu_Open then
                    --   Process input due to some other menu selection?
                    --                 Game_Utils.Game_Log ("GUI_Level_Chooser.Start_Level_Chooser_Loop Menu not Open");
                    Put_Line ("GUI_Level_Chooser.Start_Level_Chooser_Loop Process_Input Started_Loading_Map: "
                                     & Boolean'Image (Started_Loading_Map));
                    Process_Input (Window, Level_Menu_Open,
                                   Started_Loading_Map, Cheat_Unlock);
                end if;

                Put_Line ("GUI_Level_Chooser.Start_Level_Chooser_Loop Render_Level_Menu Started_Loading_Map: "
                                     & Boolean'Image (Started_Loading_Map));
                Render_Level_Menu
                  (Window, Credits_Shader_Program, Delta_Time, Custom_Levels,
                   Started_Loading_Map, Level_Menu_Open);

                Put_Line ("GUI_Level_Chooser.Start_Level_Chooser_Loop after Render_Level_Menu Started_Loading_Map: "
                                     & Boolean'Image (Started_Loading_Map));
                if Started_Loading_Map then
                    Put_Line
                      ("GUI_Level_Chooser.Start_Level_Chooser_Loop Started_Loading_Map");
                    Break := True;
                else
                    --                 Poll_Joystick;
                    Glfw.Input.Poll_Events;
                    Continue := not Window.Should_Close;
                end if;
            end if;
        end loop;  --  not Window.Should_Close and Continue

        if Continue then
            Enable (Depth_Test);

            if not Custom_Levels then
                Level_Unmodified := True;
                Game_Utils.Game_Log ("GUI_Level_Chooser.Start_Level_Chooser_Loop"
                                     & "map name found is " &
                                       Get_Selected_Level_Name (False));
                Result := Checksums;
            end if;
        end if;
        Put_Line ("Start_Level_Chooser_Loop finished, Break, Continue, Result: " &
                 Boolean'Image (Break) & "  "  & Boolean'Image (Continue) &
                 "  " & Boolean'Image (Result));
        return Result;

    exception
        when others =>
            Put_Line
              ("An exception occurred in GUI_Level_Chooser.Start_Level_Chooser_Loop.");
            return False;

    end Start_Level_Chooser_Loop;

    --  ------------------------------------------------------------------------

    procedure Update_GUI_Level_Chooser (Delta_Time  : Float;
                                        Custom_Maps : Boolean) is
        use Glfw.Input.Keys;
        use Input_Callback;
        use Input_Handler;
        Old_Sel : constant Integer := Selected_Level_ID;
        Old_Map : Level_Menu_Manager.Level_Data;
    begin
        Since_Last_Key := Since_Last_Key + Delta_Time;
        --        Game_Utils.Game_Log  ("Update_GUI_Level_Chooser Selected_Level_ID:" & Integer'Image (Selected_Map_ID));
        if Levels.Is_Empty then
            raise GUI_Level_Chooser_Exception with "Levels Is_Empty ";
        end if;

        if Selected_Level_ID < Levels.First_Index or
          Selected_Level_ID > Levels.Last_Index then
            raise GUI_Level_Chooser_Exception with
              "Invalid Selected_Level_ID: " & Integer'Image (Selected_Level_ID);
        end if;

        --         Game_Utils.Game_Log  ("Update_GUI_Level_Chooser get old map Old_Sel: " &
        --                       Integer'Image (Old_Sel));
        Old_Map := Levels.Element (Old_Sel);

        if Since_Last_Key > 0.15 then
            if Is_Key_Down (Down) or Is_Action_Down (Down_Action) then
                if Selected_Level_ID >= 8 then   --  Last wanted map
                    Selected_Level_ID := Levels.First_Index;
                else
                    Selected_Level_ID := Selected_Level_ID + 1;
                end if;
                --              Play_Sound (LEVEL_BEEP_SOUND, true);
                Since_Last_Key := 0.0;
            elsif Is_Key_Down (Up) or Is_Action_Down (Up_Action) then
                if Selected_Level_ID = Levels.First_Index then
                    Selected_Level_ID := 8;   --  Last wanted map
                else
                    Selected_Level_ID := Selected_Level_ID - 1;
                end if;
                --              Play_Sound (LEVEL_BEEP_SOUND, true);
                Since_Last_Key := 0.0;
            end if;

            --           Game_Utils.Game_Log  ("Update_GUI_Level_Chooser check Custom_Maps ");
            if Custom_Maps then
                if Selected_Level_ID >= Num_Custom_Levels then
                    Selected_Level_ID := Num_Custom_Levels - 1;
                end if;

                if Selected_Level_ID /= Old_Sel then
                    Text.Change_Text_Colour (Selected_Level_ID, 1.0, 0.0, 1.0, 1.0);
                    Text.Change_Text_Colour (Old_Sel, 1.0, 1.0, 1.0, 1.0);
                    Update_Selected_Entry_Level (False, Custom_Maps);
                end if;
            else
                if not Old_Map.Locked then
                    Text.Change_Text_Colour (Old_Sel, 1.0, 1.0, 1.0, 1.0);
                else
                    Text.Change_Text_Colour (Old_Sel, 0.25, 0.25, 0.25, 1.0);
                end if;
                Update_Selected_Entry_Level (False, Custom_Maps);
            end if;
        end if;

    end Update_GUI_Level_Chooser;

    --  ------------------------------------------------------------------------

    procedure Update_Selected_Entry_Level (First, Custom : Boolean) is
        use Custom_Levels_Manager;
        Level_Path       : Unbounded_String;
        Left_Margin_Px   : constant Single := 550.0;  --  650.0
        Lt_Margin_Cl     : constant Single :=
                             Left_Margin_Px / Single (Settings.Framebuffer_Width);
        Has_Hammer_Track : Boolean := False;
        Title_Length     : Integer := 0;
        Story_Line       : Unbounded_String;
    begin
        --          Game_Utils.Game_Log
        --            ("GUI_Level_Chooser.Update_Selected_Entry_Level: '" &
        --               To_String (Selected_Level.Title) & "'");
        if Selected_Level.Locked and not Custom then
            Selected_Level.Title := To_Unbounded_String ("locked");
            Selected_Level.Intro_Text.Clear;
            Selected_Level.Intro_Text.Append
              (To_Unbounded_String ("clear previous temples to unlock" &
                 ASCII.CR & ASCII.LF & "the portal to this map"));
        elsif Custom then Level_Path := To_Unbounded_String
              ("src/maps/" & Get_Custom_Map_Name (Custom_Levels, Selected_Level_ID));
            --           Game_Utils.Game_Log ("level chooser is peeking in map " &
            --                                  To_String (Map_Path));
        else
            Level_Path := To_Unbounded_String
              ("src/maps/" & Level_Menu_Manager.Get_Level_Name
                 (Levels, Selected_Level_ID));
            --              Game_Utils.Game_Log ("level chooser is peeking in map " &
            --                                              To_String (Level_Path));
            Selected_Level_Manager.Load_Map (To_String (Level_Path), Selected_Level,
                                             Has_Hammer_Track);
            Selected_Level_Track := Selected_Level.Music_Track;
        end if;

        if First then
            Game_Utils.Game_Log
              ("GUI_Level_Chooser.Update_Selected_Entry_Dot_Map first Title: '" &
                 To_String (Selected_Level.Title) & "'");
            Level_Title_Text_ID :=
              Text.Add_Text (To_String (Selected_Level.Title),
                             Left_Margin_Cl + Lt_Margin_Cl,
                             Top_Margin_Cl - 100.0 / Single (Settings.Framebuffer_Height),  --  180
                             30.0, 0.9, 0.9, 0.0, 0.8);
            Text.Set_Text_Visible (Level_Title_Text_ID, False);

            --           Game_Utils.Game_Log
            --             ("GUI_Level_Chooser.Update_Selected_Entry_Level first Last_Index: " &
            --                Integer'Image (Selected_Level.Map_Intro_Text.Last_Index));
            Level_Story_Text_ID.Clear;
            for index in Selected_Level.Intro_Text.First_Index ..
              Selected_Level.Intro_Text.Last_Index loop
                --              Game_Utils.Game_Log
                --                ("GUI_Level_Chooser.Update_Selected_Entry_Level first index: "
                --                  & Integer'Image (index) & "  '" &
                --                To_String (Selected_Level.Intro_Text.Element (index)) & "'");
                Story_Line := Selected_Level.Intro_Text.Element (index);
                if Length (Story_Line) < 1 then
                    Story_Line := To_Unbounded_String (" ");
                end if;
                Level_Story_Text_ID.Append
                  (Text.Add_Text (To_String (Story_Line),
                   Left_Margin_Cl + Lt_Margin_Cl,
                   Top_Margin_Cl - (1.0 + 0.2 * Single (index - 1)) * 240.0 / Single
                   (Settings.Framebuffer_Height),
                   14.0, 0.75, 0.75, 0.75, 1.0));
                Text.Set_Text_Visible (Level_Story_Text_ID.Element (index), False);
            end loop;
        else
            --             Game_Utils.Game_Log
            --                ("GUI_Level_Chooser.Update_Selected_Entry_Level not first Title: '" &
            --                  To_String (Selected_Level.Title) & "'");

            Text.Update_Text (Level_Title_Text_ID, To_String (Selected_Level.Title));
            Text.Centre_Text (Level_Title_Text_ID, -0.4, 0.75);

            Level_Story_Text_ID.Clear;
            for index in Selected_Level.Intro_Text.First_Index ..
              Selected_Level.Intro_Text.Last_Index loop
                Story_Line := Selected_Level.Intro_Text.Element (index);
                if Length (Story_Line) < 1 then
                    Story_Line := To_Unbounded_String (" ");
                end if;
                if index <= Level_Story_Text_ID.Last_Index then
                    Text.Update_Text (Level_Story_Text_ID.Element (index),
                                      To_String (Story_Line));
                else
                    Level_Story_Text_ID.Append
                      (Text.Add_Text (To_String (Story_Line),
                       Left_Margin_Cl + Lt_Margin_Cl,
                       Top_Margin_Cl -
                         (1.0 + 0.2 * Single (index - 1)) * 240.0 / Single
                       (Settings.Framebuffer_Height),
                       14.0, 0.75, 0.75, 0.75, 1.0));
                    Text.Set_Text_Visible
                      (Level_Story_Text_ID.Element (index), False);
                end if;
            end loop;
        end if;

    exception
        when others => Put_Line
              ("An exception occurred in GUI_Level_Chooser.Update_Selected_Entry_Level.");
    end Update_Selected_Entry_Level;

    --  ------------------------------------------------------------------------

end GUI_Level_Chooser;
