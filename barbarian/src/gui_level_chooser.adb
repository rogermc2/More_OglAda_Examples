
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Input.Keys;

with GL.Types; use GL.Types;
with GL.Objects.Vertex_Arrays;

with Maths;
with Utilities;

with Custom_Maps_Manager;
with Game_Utils;
with GL_Utils;
with Input_Handler;
with Levels_Maps_Manager;
with Menu_Credits_Shader_Manager;
with MMenu;
with Selected_Map_Manager;
with Settings;
with Text;

package body GUI_Level_Chooser is

   Quad_VAO                  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Custom_Maps               : Custom_Maps_Manager.Custom_Maps_List;
   Num_Custom_Maps           : Natural := 0;
   Maps                      : Levels_Maps_Manager.Maps_List;
   Selected_Map_ID           : Positive := 1;
   Selected_Map              : Selected_Map_Manager.Selected_Map_Data;
   Map_Title_Text            : Integer := -1;
   Map_Story_Text            : Integer := -1;
   Left_Margin_Cl            : Single := 0.0;
   Top_Margin_Cl             : Single := 0.0;
   Level_GUI_Width           : Single := 1024.0;
   Level_GUI_Height          : Single := 768.0;

   Choose_Map_Txt            : Integer := -1;
   Map_Title_Txt             : Integer := -1;
   Map_Story_Txt             : Integer := -1;
   Loading_Map_Txt           : Integer := -1;

   Cheated                   : Boolean := False;
   Map_Is_Unmodified         : Boolean := True;
   Pillar_Crushes            : Integer := 0;
   Boulder_Crushes           : Integer := 0;
   Hammer_Kills              : Integer := 0;
   Fall_Kills                : Integer := 0;
   Since_Last_Key            : Float := 0.0;

   procedure Update_GUI_Level_Chooser (Delta_Time : Float; Custom_Maps : Boolean);
   procedure Update_Selected_Entry_Dot_Map (First, Custom : Boolean);

   --  ------------------------------------------------------------------------

   function Cheated_On_Map return Boolean is
   begin
      return Cheated;
   end Cheated_On_Map;

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
      Text_Height     : constant Single :=
                          50.0 / Single (Settings.Framebuffer_Height);
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

      Left_Margin_Cl := -Level_GUI_Width / Single (Framebuffer_Width);
      Top_Margin_Cl := Level_GUI_Height / Single (Framebuffer_Height);

      --      if not Game_Utils.Is_Little_Endian then
      --              Put_Line ("GUI_Level_Chooser.Init!");
      --              Put ("WARNING: unsupported big-endian system detected "
      --              Put_Line ("game may cease to function at this point.");
      --              Put_Line ("please notify game designer and provide your system specs.");
      --      end if;
      Game_Utils.Game_Log ("GUI_Level_Chooser loading maps from " &
                             "src/save/maps.dat");
      Levels_Maps_Manager.Load_Names ("src/save/maps.dat", Maps);
      Levels_Maps_Manager.Init_Maps (Maps, Selected_Map_ID,
                                     Left_Margin_Cl, Top_Margin_Cl);
      Update_Selected_Entry_Dot_Map (True, False);
      Choose_Map_Text :=
        Text.Add_Text ("choose thy battle!", 0.0, Single (Top_Margin_Cl),
                       30.0, 1.0, 1.0, 0.0, 0.8);
      Text.Centre_Text (Choose_Map_Text, 0.0, Single (Top_Margin_Cl));
      Text.Set_Text_Visible (Choose_Map_Text, False);

      Game_Utils.Game_Log ("GUI_Level_Chooser Maps Choose_Map_Text initialized. ");
      Custom_Maps_Manager.Load_Custom_Map
        ("src/editor/maps.txt", Custom_Maps, Top_Margin_Cl,
         Left_Margin_Cl, Text_Height, Num_Custom_Maps);
   exception
      when others =>
         Put_Line ("An exception occurred in GUI_Level_Chooser.Init.");
         raise;
   end Init;

   --  ------------------------------------------------------------------------

   procedure Process_Input (Menu_Open, Started_Loading_Map, Cheat_Unlock : in out Boolean) is
      use  Glfw.Input.Keys;
      use Input_Handler;
      use Levels_Maps_Manager.Maps_Package;
      Selected_Map : Levels_Maps_Manager.Level_Map_Data :=
                       Maps.Element (Selected_Map_ID);
   begin
      if Was_Key_Pressed (Enter) or Was_Action_Pressed (OK_Action)
        or Was_Action_Pressed (Attack_Action) then
         if not Selected_Map.Locked or Cheat_Unlock then
            Started_Loading_Map := True;
         end if;
      elsif Was_Key_Pressed (Escape) or Was_Action_Pressed (Open_Menu_Action) then
         Menu_Open := True;
      elsif Is_Key_Down (A) and Is_Key_Down (N)  and Is_Key_Down (D) then
         if not Cheat_Unlock then
            --              Play_Sound (GONG_SOUND_FILE, true);
            Cheat_Unlock := True;
            Game_Utils.Game_Log ("cheater!");
         end if;
      end if;
   end Process_Input;

   --  ------------------------------------------------------------------------

   procedure Render is
      use GL.Types;
      use Maths.Single_Math_Functions;
      Aspect : constant Single := Single (Settings.Framebuffer_Width) /
                 Single (Settings.Framebuffer_Height);
      Now    : constant Single := Single (Glfw.Time);
      Sx     : constant Single := 2.0;
      Sy     : constant Single := 2.0 * Aspect;
      Px     : constant Single := Sin (0.15 * Now);
      Py     :constant  Single := -1.5 * Cos (0.09 * Now);
   begin
      Utilities.Clear_Colour;
      Menu_Credits_Shader_Manager.Set_Scale ((Sx, Sy));
      Menu_Credits_Shader_Manager.Set_Position ((Px, Py));

   end Render;

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

   procedure Set_Cheated_On_Map (State : Boolean) is
   begin
      Cheated := State;
   end Set_Cheated_On_Map;

   --  ------------------------------------------------------------------------

   function Start_Level_Chooser_Loop (Window     : in out Glfw.Windows.Window;
                                      Custom_Maps: Boolean) return Boolean is
      Menu_Open           : Boolean := MMenu.End_Story_Open;
      Menu_Quit           : Boolean := False;
      Cheat_Unlock        : Boolean := False;
      Started_Loading_Map : Boolean := False;
      Current_Time        : Float;
      Delta_Time          : Float;
      Last_Time           : Float := GL_Utils.Get_Elapsed_Seconds;
      Continue            : Boolean := True;
      Restart             : Boolean := False;
      Result              : Boolean := False;
   begin
      Reset_GUI_Level_Selection (Custom_Maps);
      while not Window.Should_Close and Continue loop
         Game_Utils.Game_Log ("GUI_Level_Chooser.Start_Level_Chooser_Loop loop. ");
         Current_Time := GL_Utils.Get_Elapsed_Seconds;
         Delta_Time := Current_Time - Last_Time;
         Last_Time := Current_Time;
         if Menu_Open then
            Menu_Quit := not MMenu.Update_MMenu (Delta_Time);
            if MMenu.Menu_Was_Closed then
               Menu_Open := False;
            end if;
            if MMenu.Did_User_Choose_New_Game or
              MMenu.Did_User_Choose_Custom_Maps then
               Menu_Open := False;
               Continue := False;
               Restart := True;
            elsif Menu_Quit then
               Continue := False;
            end if;
         else
            Game_Utils.Game_Log ("Start_Level_Chooser_Loop Update_GUI_Level_Chooser");
            Update_GUI_Level_Chooser (Delta_Time, Custom_Maps);
         end if;
         Game_Utils.Game_Log ("Start_Level_Chooser_Loop continue: " &
                             Boolean'Image (Continue));

         if Continue then
            if not Menu_Open then
               Game_Utils.Game_Log ("Start_Level_Chooser_Loop Menu not Open");
               Process_Input (Menu_Open, Started_Loading_Map, Cheat_Unlock);
            end if;
            Render;
         end if;
      end loop;

      if Restart then
         Result := Start_Level_Chooser_Loop (Window, False);
      end if;
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in GUI_Level_Chooser.Start_Level_Chooser_Loop.");
         return False;

   end Start_Level_Chooser_Loop;

   --  ------------------------------------------------------------------------

   procedure Update_GUI_Level_Chooser (Delta_Time : Float; Custom_Maps : Boolean) is
      use Glfw.Input.Keys;
      use Input_Handler;
      Old_Sel : Integer := Selected_Map_ID;
      Old_Map : Levels_Maps_Manager.Level_Map_Data;
   begin
      New_Line;
      Since_Last_Key := Since_Last_Key + Delta_Time;
      Game_Utils.Game_Log  ("Update_GUI_Level_Chooser Selected_Map_ID:" & Integer'Image (Selected_Map_ID));
      if Maps.Is_Empty then
         raise GUI_Level_Chooser_Exception with "Maps.Is_Empty ";
      end if;
      if Selected_Map_ID < Maps.First_Index or
        Selected_Map_ID > Maps.Last_Index then
         raise GUI_Level_Chooser_Exception with
         "Invalid Selected_Map_ID: " & Integer'Image (Selected_Map_ID);
      end if;

      Game_Utils.Game_Log  ("Update_GUI_Level_Chooser get old map Old_Sel: " &
               Integer'Image (Old_Sel));
      Old_Map := Maps.Element (Old_Sel);

      if Since_Last_Key > 0.15 then
         Game_Utils.Game_Log  ("Update_GUI_Level_Chooser Since_Last_Key > 0.15");
         if Is_Key_Down (Down) or Is_Action_Down (Down_Action) then
            Selected_Map_ID := Selected_Map_ID + 1;
            --              Play_Sound (LEVEL_BEEP_SOUND, true);
            Since_Last_Key := 0.0;
         elsif Is_Key_Down (Up) or Is_Action_Down (Up_Action) then
            Selected_Map_ID := Selected_Map_ID - 1;
            --              Play_Sound (LEVEL_BEEP_SOUND, true);
            Since_Last_Key := 0.0;
         end if;

         Game_Utils.Game_Log  ("Update_GUI_Level_Chooser check Custom_Maps ");
         if Custom_Maps then
            if Selected_Map_ID >= Num_Custom_Maps then
               Selected_Map_ID := Selected_Map_ID - Num_Custom_Maps + 1;
            end if;
            if Selected_Map_ID /= Old_Sel then
               Text.Change_Text_Colour (Selected_Map_ID, 1.0, 0.0, 1.0, 1.0);
               Text.Change_Text_Colour (Old_Sel, 1.0, 1.0, 1.0, 1.0);
               Update_Selected_Entry_Dot_Map (False, Custom_Maps);
            end if;
         else
            if Selected_Map_ID >= Maps.Last_Index then
               Selected_Map_ID := Selected_Map_ID - Maps.Last_Index + 1;
            end if;
            if not Old_Map.Locked then
               Text.Change_Text_Colour (Old_Sel, 1.0, 1.0, 1.0, 1.0);
            else
               Text.Change_Text_Colour (Old_Sel, 0.25, 0.25, 0.25, 1.0);
            end if;
               Update_Selected_Entry_Dot_Map (False, Custom_Maps);
         end if;
      end if;
      Game_Utils.Game_Log  ("Update_GUI_Level_Chooser finished");

   end Update_GUI_Level_Chooser;

   --  ------------------------------------------------------------------------

   procedure Update_Selected_Entry_Dot_Map (First, Custom : Boolean) is
      use Custom_Maps_Manager;
      Map_Path     : Unbounded_String;
      Peek_Map     : Selected_Map_Manager.Selected_Map_Data;
      Lt_Margin_Px : constant Single := 650.0;
      Lt_Margin_Cl : constant Single :=
                       Lt_Margin_Px / Single (Settings.Framebuffer_Width);
      Has_Hammer_Track : Boolean := False;
      Title_Length : Integer := 0;
   begin
      if Selected_Map.Locked and not Custom then
         Selected_Map.Map_Title := To_Unbounded_String ("locked");
         Selected_Map.Map_Intro_Text := To_Unbounded_String
           ("clear previous temples to unlock" &
              ASCII.CR & ASCII.LF & "the portal to this map");
      else
         if Custom then Map_Path := To_Unbounded_String
              ("src/maps/" & Get_Custom_Map_Name (Custom_Maps, Selected_Map_ID));
            Game_Utils.Game_Log ("level chooser is peeking in map " &
                                   To_String (Map_Path));
         else
            Map_Path := To_Unbounded_String
              ("src/maps/" & Levels_Maps_Manager.Get_Map_Name
                 (Maps, Selected_Map_ID));
            Game_Utils.Game_Log ("level chooser is peeking in map " &
                                   To_String (Map_Path));
            Selected_Map_Manager.Load_Map (To_String (Map_Path), Selected_Map,
                                           Has_Hammer_Track);
         end if;
      end if;

      if First then
         Game_Utils.Game_Log ("GUI_Level_Chooser.Update_Selected_Entry_Dot_Map first: '" &
                     To_String (Selected_Map.Map_Title) &
                   "'");
         Map_Title_Text :=
           Text.Add_Text (To_String (Selected_Map.Map_Title),
                          Left_Margin_Cl + Lt_Margin_Cl,
                          Top_Margin_Cl - 180.0 / Single (Settings.Framebuffer_Height),
                          30.0, 0.9, 0.9, 0.0, 0.8);
         Text.Set_Text_Visible (Map_Title_Text, False);

         Map_Story_Text :=
           Text.Add_Text (To_String (Selected_Map.Map_Intro_Text),
                          Left_Margin_Cl + Lt_Margin_Cl,
                          Top_Margin_Cl - 300.0 / Single (Settings.Framebuffer_Height),
                          20.0, 0.75, 0.75, 0.75, 1.0);
         Text.Set_Text_Visible (Map_Story_Text, False);
      else

       Game_Utils.Game_Log ("GUI_Level_Chooser.Update_Selected_Entry_Dot_Map not first.");
         Text.Update_Text (Map_Title_Text, To_String (Selected_Map.Map_Title));
         Text.Update_Text (Map_Story_Text, To_String (Selected_Map.Map_Intro_Text));
      end if;
      Game_Utils.Game_Log ("GUI_Level_Chooser.Update_Selected_Entry_Dot_Map finished.");

   exception
      when others =>
         Put_Line ("An exception occurred in GUI_Level_Chooser.Update_Selected_Entry_Dot_Map.");
   end Update_Selected_Entry_Dot_Map;

   --  ------------------------------------------------------------------------

end GUI_Level_Chooser;
