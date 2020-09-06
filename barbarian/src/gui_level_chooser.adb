
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with Custom_Maps_Manager;
with Game_Utils;
--  with GL_Utils;
with Levels_Maps_Manager;
--  with MMenu;
with Selected_Map_Manager;
with Settings;
with Text;

package body GUI_Level_Chooser is

    Custom_Maps               : Custom_Maps_Manager.Custom_Maps_List;
    Num_Custom_Maps           : Natural := 0;
    Maps                      : Levels_Maps_Manager.Maps_List;
    Selected_Map_ID           : Positive := 1;
    Selected_Map              : Selected_Map_Manager.Selected_Map_Data;
    Map_Title_Text            : Integer := -1;
    Map_Story_Text            : Integer := -1;
    Left_Margin_Cl            : Float := 0.0;
    Top_Margin_Cl             : Float := 0.0;
    Level_GUI_Width           : Float := 1024.0;
    Level_GUI_Height          : Float := 768.0;

    procedure Update_Selected_Entry_Dot_Map (First, Custom : Boolean);

    --  ------------------------------------------------------------------------

    function Get_Selected_Map_Name (Custom : Boolean) return String is
        Result      : String := "";
    begin
        if Custom then
            Result := Custom_Maps_Manager.Get_Custom_Map_Name
              (Custom_Maps, Selected_Map_ID);
        else
            Result := Levels_Maps_Manager.Get_Map_Name
              (Maps, Selected_Map_ID);
        end if;

        return Result;
    end Get_Selected_Map_Name;

    --  ------------------------------------------------------------------------

    procedure Init is
    use GL.Types;
        use Settings;
        Name_Maps       : Levels_Maps_Manager.Maps_List;
        Text_Height     : constant Float :=
                            50.0 / Float (Settings.Framebuffer_Height);
        Choose_Map_Text : Integer;
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
    Left_Margin_Cl := -Level_GUI_Width / float (Framebuffer_Width);
    Top_Margin_Cl := Level_GUI_Height / float (Framebuffer_Height);

--      if not Game_Utils.Is_Little_Endian then
--              Put_Line ("GUI_Level_Chooser.Init!");
--              Put ("WARNING: unsupported big-endian system detected "
--              Put_Line ("game may cease to function at this point.");
--              Put_Line ("please notify game designer and provide your system specs.");
--      end if;
        Levels_Maps_Manager.Load_Names ("../save/maps.dat", Name_Maps);
        Levels_Maps_Manager.Init_Maps (Name_Maps, Maps, Left_Margin_Cl, Top_Margin_Cl);
        Update_Selected_Entry_Dot_Map (True, False);
        Choose_Map_Text :=
          Text.Add_Text ("choose thy battle!", 0.0, Top_Margin_Cl,
                         30.0, 1.0, 1.0, 0.0, 0.8);
        Text.Centre_Text (Choose_Map_Text, 0.0, Top_Margin_Cl);
        Text.Set_Text_Visible (Choose_Map_Text, False);

        Custom_Maps_Manager.Load_Custom_Map
          ("editor/maps.txt", Custom_Maps, Top_Margin_Cl, Left_Margin_Cl,
           Text_Height, Num_Custom_Maps);
    exception
        when others =>
            Put_Line ("An exception occurred in GUI_Level_Chooser.Init.");
            raise;
    end Init;

    --  ------------------------------------------------------------------------

    procedure Reset_GUI_Level_Selection (Custom : Boolean) is
        use Levels_Maps_Manager.Maps_Package;
        use Custom_Maps_Manager.Custom_Maps_Package;
        Map_Cursor        : Levels_Maps_Manager.Maps_Package.Cursor :=
                              Maps.First;
        Custom_Map_Cursor : Custom_Maps_Manager.Custom_Maps_Package.Cursor :=
                              Custom_Maps.First;
        aMap              : Levels_Maps_Manager.Level_Map_Data;
        Custom_Map        : Custom_Maps_Manager.Custom_Data;
    begin
        Selected_Map_ID := 1;
        if Custom then
            while Has_Element (Custom_Map_Cursor) loop
                Custom_Map := Element (Custom_Map_Cursor);
                Text.Change_Text_Colour (aMap.Map_Name_Text_ID, 1.0, 1.0, 1.0, 1.0);
                Custom_Maps.Replace_Element (Custom_Map_Cursor, Custom_Map);
                Next (Custom_Map_Cursor);
            end loop;
        else
            while Has_Element (Map_Cursor) loop
                aMap := Element (Map_Cursor);
                Text.Change_Text_Colour (aMap.Map_Name_Text_ID, 1.0, 1.0, 1.0, 1.0);
                Maps.Replace_Element (Map_Cursor, aMap);
                Next (Map_Cursor);
            end loop;
        end if;
    end Reset_GUI_Level_Selection;

    --  ------------------------------------------------------------------------

    function Start_Level_Chooser_Loop (Custom : Boolean) return Boolean is
--          Menu_Open    : Boolean := MMenu.End_Story_Open;
--          Menu_Quit    : Boolean := False;
--          Cheat_Unlock : Boolean := False;
--          Last_Time    : Float := GL_Utils.Get_Elapsed_Seconds;
    begin
        Reset_GUI_Level_Selection (Custom);

        return False;
    end Start_Level_Chooser_Loop;

    --  ------------------------------------------------------------------------

    procedure Update_Selected_Entry_Dot_Map (First, Custom : Boolean) is
        use Custom_Maps_Manager;
        Map_Path     : Unbounded_String;
        Lt_Margin_Px : constant Float := 650.0;
	Lt_Margin_Cl : constant Float :=
                         Lt_Margin_Px / Float (Settings.Framebuffer_Width);
    begin
        if Selected_Map.Locked and not Custom then
            Selected_Map.Map_Title := To_Unbounded_String ("locked");
            Selected_Map.Map_Intro_Text := To_Unbounded_String
              ("clear previous temples to unlock" &
              ASCII.CR & ASCII.LF & "the portal to this map");
        else
            if Custom then Map_Path := To_Unbounded_String ("maps/" &
                  Get_Custom_Map_Name (Custom_Maps, Selected_Map_ID) & ".map");
                Game_Utils.Game_Log ("level chooser is peeking in map " &
                                     To_String (Map_Path));
            else
                Map_Path := To_Unbounded_String
                  ("maps/" & Levels_Maps_Manager.Get_Map_Name
                     (Maps, Selected_Map_ID) & ".map");
                Game_Utils.Game_Log ("level chooser is peeking in map " &
                                     To_String (Map_Path));
            end if;
        end if;

        if First then
            Map_Title_Text :=
              Text.Add_Text (To_String (Selected_Map.Map_Title),
                             Left_Margin_Cl + Lt_Margin_Cl,
                             Top_Margin_Cl - 180.0 / Float (Settings.Framebuffer_Height),
                             30.0, 0.9, 0.9, 0.0, 0.8);
            Text.Set_Text_Visible (Map_Title_Text, False);

            Map_Story_Text :=
              Text.Add_Text (To_String (Selected_Map.Map_Intro_Text),
                             Left_Margin_Cl + Lt_Margin_Cl,
                             Top_Margin_Cl - 300.0 / Float (Settings.Framebuffer_Height),
                             20.0, 0.75, 0.75, 0.75, 1.0);
            Text.Set_Text_Visible (Map_Story_Text, False);
        else
            Text.Update_Text (Map_Title_Text, To_String (Selected_Map.Map_Title));
            Text.Update_Text (Map_Story_Text, To_String (Selected_Map.Map_Intro_Text));
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in GUI_Level_Chooser.Update_Selected_Entry_Dot_Map.");
    end Update_Selected_Entry_Dot_Map;

    --  ------------------------------------------------------------------------

end GUI_Level_Chooser;
