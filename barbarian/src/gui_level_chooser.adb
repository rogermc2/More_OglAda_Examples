
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with Game_Utils;
with GL_Utils;
with Levels_Maps_Manager;
with MMenu;
with Settings;
with Text;

package body GUI_Level_Chooser is

    Custom_Maps               : Levels_Maps_Manager.Custom_Maps_List;
    Maps                      : Levels_Maps_Manager.Maps_List;
    Selected_Map              : Integer := 1;
    Map_Title_Text            : Integer := -1;
    Map_Story_Text            : Integer := -1;
    Choose_Map_Text           : Integer := -1;
    Selected_Map_Title        : Unbounded_String := To_Unbounded_String ("");
    Selected_Map_Intro_Text   : Unbounded_String := To_Unbounded_String ("");
    Selected_Map_Track        : Unbounded_String := To_Unbounded_String ("");
    Selected_Map_Hammer_Track : Unbounded_String := To_Unbounded_String ("");
    Has_Hammer_Track          : Boolean := False;
    Left_Margin_Cl            : Float := 0.0;
    Top_Margin_Cl             : Float := 0.0;
    Level_GUI_Width           : Float := 1024.0;
    Level_GUI_Height          : Float := 768.0;

    procedure Update_Selected_Entry_Dot_Map (First, Custom : Boolean);

    --  ------------------------------------------------------------------------

    function Get_Selected_Map_Name (Custom : Boolean) return String is
        use Levels_Maps_Manager;
        use Maps_Package;
        aMap : Level_Map_Data;
        OK   : Boolean := False;
        Result : String := "";
    begin
        if Custom then
            OK := Has_Element (Custom_Maps.To_Cursor (Selected_Map));
            if OK then
                aMap := Custom_Maps.Element (Selected_Map);
            end if;
        else
            OK := Has_Element (Custom_Maps.To_Cursor (Selected_Map));
            if OK then
                aMap := Maps.Element (Selected_Map);
            end if;
        end if;

        if OK then
            Result := To_String (aMap.Map_Name);
        else
            Game_Utils.Game_Log
              ("GUI_Level_Chooser.Get_Selected_Map_Name " &
              "encountered an invalid Map ID: " & Integer'Image (Selected_Map));
        end if;
        return Result;
    end Get_Selected_Map_Name;

    --  ------------------------------------------------------------------------

    procedure Init is
    use GL.Types;
        use Settings;
        Name_Maps  : Levels_Maps_Manager.Maps_List;
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

    exception
        when others =>
            Put_Line ("An exception occurred in GUI_Level_Chooser.Init.");
            raise;
    end Init;

    --  ------------------------------------------------------------------------

    procedure Reset_GUI_Level_Selection (Custom : Boolean) is
        use Levels_Maps_Manager.Maps_Package;
        Map_Cursor        : Cursor := Maps.First;
        Custom_Map_Cursor : Cursor := Custom_Maps.First;
        aMap              : Levels_Maps_Manager.Level_Map_Data;
    begin
        Selected_Map := 1;
        if Custom then
            while Has_Element (Custom_Map_Cursor) loop
                aMap := Element (Custom_Map_Cursor);
                Text.Change_Text_Colour (aMap.Map_Name_Text_ID, 1.0, 1.0, 1.0, 1.0);
                Custom_Maps.Replace_Element (Custom_Map_Cursor, aMap);
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
        Menu_Open    : Boolean := MMenu.End_Story_Open;
        Menu_Quit    : Boolean := False;
        Cheat_Unlock : Boolean := False;
        Last_Time    : Float := GL_Utils.Get_Elapsed_Seconds;
    begin
        Reset_GUI_Level_Selection (Custom);

        return False;
    end Start_Level_Chooser_Loop;

    --  ------------------------------------------------------------------------

    procedure Update_Selected_Entry_Dot_Map (First, Custom : Boolean) is
        use Levels_Maps_Manager;
        aMap         : Level_Map_Data := First_Element (Maps);
        Custom_Map   : Level_Map_Data := First_Element (Custom_Maps);
        Map_Path     : Unbounded_String;
        Lt_Margin_Px : constant Float := 650.0;
	Lt_Margin_Cl : constant Float :=
                         Lt_Margin_Px / Float (Settings.Framebuffer_Width);
    begin
        if aMap.Locked and not Custom then
            Selected_Map_Title := To_Unbounded_String ("locked");
            Selected_Map_Intro_Text := To_Unbounded_String
              ("clear previous temples to unlock" &
              ASCII.CR & ASCII.LF & "the portal to this map");
        else
            if Custom then
                Map_Path := "maps/" & Custom_Map.Map_Name & ".map";
                Levels_Maps_Manager.Load_Map (To_String (Map_Path), Custom_Map,
                                              Has_Hammer_Track);
               Custom_Maps.Replace_Element (1, Custom_Map);
            else
                Map_Path := "maps/" & Custom_Map.Map_Name & ".map";
                Levels_Maps_Manager.Load_Map (To_String (Map_Path), aMap,
                                              Has_Hammer_Track);
                Maps.Replace_Element (1, aMap);
            end if;
        end if;

        if First then
            Map_Title_Text :=
              Text.Add_Text (To_String (Selected_Map_Title),
                             Left_Margin_Cl + Lt_Margin_Cl,
                             Top_Margin_Cl - 180.0 / Float (Settings.Framebuffer_Height),
                             30.0, 0.9, 0.9, 0.0, 0.8);
            Text.Set_Text_Visible (Map_Title_Text, False);

            Map_Story_Text :=
              Text.Add_Text (To_String (Selected_Map_Intro_Text),
                             Left_Margin_Cl + Lt_Margin_Cl,
                             Top_Margin_Cl - 300.0 / Float (Settings.Framebuffer_Height),
                             20.0, 0.75, 0.75, 0.75, 1.0);
            Text.Set_Text_Visible (Map_Story_Text, False);
        else
            Text.Update_Text (Map_Title_Text, To_String (Selected_Map_Title));
            Text.Update_Text (Map_Story_Text, To_String (Selected_Map_Intro_Text));
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in GUI_Level_Chooser.Update_Selected_Entry_Dot_Map.");
    end Update_Selected_Entry_Dot_Map;

    --  ------------------------------------------------------------------------

end GUI_Level_Chooser;
