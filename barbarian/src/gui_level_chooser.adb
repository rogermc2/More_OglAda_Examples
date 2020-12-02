

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

with Custom_Maps_Manager;
with Game_Utils;
with GL_Utils;
with GUI;
with Input_Handler;
with Level_Menu_Manager;
with Maps_Manager;
with Menu_Credits_Shader_Manager;
with Main_Menu;
with Selected_Map_Manager;
with Settings;
with Text;
with Texture_Manager;

package body GUI_Level_Chooser is
   use Selected_Map_Manager;

   Quad_VAO                  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Quad_VBO                  : GL.Objects.Buffers.Buffer;
   Back_Texture              : GL.Objects.Textures.Texture;
   Back_Custom_Texture       : GL.Objects.Textures.Texture;
   Custom_Maps               : Custom_Maps_Manager.Custom_Maps_List;
   Num_Custom_Maps           : Natural := 0;
   Maps                      : Level_Menu_Manager.Levels_List;
   Selected_Map_ID           : Positive := 1;
   Selected_Map              : Selected_Map_Data;
   Selected_Map_Track        : Unbounded_String := To_Unbounded_String ("");
   Left_Margin_Cl            : Single := 0.0;
   Top_Margin_Cl             : Single := 0.0;
   Level_GUI_Width           : Single := 1024.0;
   Level_GUI_Height          : Single := 768.0;

   Choose_Level_Text_ID      : Integer := -1;
   Level_Title_Text_ID       : Integer := -1;
   Level_Story_Text_ID       : GL_Utils.Integer_List;
   Loading_Map_Text_ID       : Integer := -1;

   Cheated                   : Boolean := False;
   Map_Unmodified            : Boolean := True;
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
   procedure Update_Selected_Entry_Dot_Map (First, Custom : Boolean);

   --  ------------------------------------------------------------------------

   function Cheated_On_Map return Boolean is
   begin
      return Cheated;
   end Cheated_On_Map;

   --  ------------------------------------------------------------------------

   function Checksums return Boolean is
      Map_Sums : constant Int_Array (1 .. 8) :=
                   (797678,  -- orig 800948,  --  intro
                    1744957, --  threedoors
                    869974,  --  warlock
                    1014716, --  winder
                    1224821, --  under
                    917647,  --  skytemple
                    1534173, -- 1536666, --  hall
                    905383   --  attercoppe
                   );
      Map_Name : constant String := Get_Selected_Map_Name (False);
      Sum      : constant Int := Get_Map_Checksum (Map_Name);
      OK       : Boolean := False;
   begin
      OK := Sum = Map_Sums (Int (Selected_Map_ID));
      if not OK then
         Map_Unmodified := False;
         Game_Utils.Game_Log ("GUI_Level_Chooser.Checksums: map checksum " &
                                Int'Image (Sum) & " for " & Map_Name &
                                " did not match record - disabling.");
      end if;
      return OK;
   end Checksums;

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

   function Get_Selected_Map_Music return String is
   begin
      return To_String (Selected_Map_Track);
   end Get_Selected_Map_Music;

   --  ------------------------------------------------------------------------

   function Get_Selected_Map_Name (Custom : Boolean) return String is
      Result      : Unbounded_String := To_Unbounded_String ("");
   begin
      --        Put_Line ("GUI_Level_Chooser.Get_Selected_Map_Name, Selected_Map_ID: " &
      --                 Integer'Image (Selected_Map_ID));
      if Custom then
         Result := To_Unbounded_String (Custom_Maps_Manager.Get_Custom_Map_Name
                                        (Custom_Maps, Selected_Map_ID));
      else
         Result := To_Unbounded_String (Level_Menu_Manager.Get_Map_Name
                                        (Maps, Selected_Map_ID));
      end if;

      return To_String (Result);
   end Get_Selected_Map_Name;

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
      Level_Menu_Manager.Load_Story_Names ("src/save/maps.dat", Maps);
      Level_Menu_Manager.Init_Level_Maps (Maps, Selected_Map_ID,
                                          Left_Margin_Cl, Top_Margin_Cl);
      Update_Selected_Entry_Dot_Map (True, False);

      Choose_Level_Text_ID :=
        Text.Add_Text ("choose thy battle!", 0.0, Single (Top_Margin_Cl),
                       30.0, 1.0, 1.0, 0.0, 0.8);
      Text.Centre_Text (Choose_Level_Text_ID, 0.0, Single (Top_Margin_Cl));
      Text.Set_Text_Visible (Choose_Level_Text_ID, False);

      Custom_Maps_Manager.Load_Custom_Map
        ("src/editor/maps.txt", Custom_Maps, Top_Margin_Cl,
         Left_Margin_Cl, Text_Height, Num_Custom_Maps);
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

   function Is_Map_Introduction return Boolean is
   begin
      return not Main_Menu.Are_We_In_Custom_Maps and
        Selected_Map.Map_Type = Map_Introduction;
   end Is_Map_Introduction;

   --  ------------------------------------------------------------------------

   function Is_Map_Warlock return Boolean is
   begin
      return not Main_Menu.Are_We_In_Custom_Maps and
        Selected_Map.Map_Type = Map_Warlock;
   end Is_Map_Warlock;

   --  ------------------------------------------------------------------------

   function Map_Is_Unmodified return Boolean is
   begin
      return Map_Unmodified;
   end Map_Is_Unmodified;

   --  ------------------------------------------------------------------------

   procedure Process_Input (Window                                       : in out Input_Callback.Barbarian_Window;
                            Menu_Open, Started_Loading_Map, Cheat_Unlock : in out Boolean) is
      use Glfw.Input.Keys;
      use Input_Callback;
      use Input_Handler;
      use Level_Menu_Manager.Levels_Package;
      Selected_Map : constant Level_Menu_Manager.Level_Map_Data :=
                       Maps.Element (Selected_Map_ID);
   begin
      --        Game_Utils.Game_Log ("Process_Input OK_Action: " & Natural'Image (OK_Action));
      --        Game_Utils.Game_Log ("Process_Input Attack_Action: " & Natural'Image (Attack_Action));
      if Was_Key_Pressed (Window, Enter) or Was_Action_Pressed (Window, OK_Action)
        or Was_Action_Pressed (Window, Attack_Action) then
         if not Selected_Map.Locked or Cheat_Unlock then
            Started_Loading_Map := True;
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
   end Process_Input;

   --  ------------------------------------------------------------------------

   procedure Render (Window                                          : in out Input_Callback.Barbarian_Window;
                     Credits_Shader_Program                          : GL.Objects.Programs.Program;
                     Delta_Time                                      : Float;
                     Use_Custom_Maps, Started_Loading_Map, Menu_Open : Boolean) is
      use Level_Menu_Manager.Levels_Package;
      use Custom_Maps_Manager.Custom_Maps_Package;
      LeveL_Menu  : Level_Menu_Manager.Level_Map_Data;
      Custom_Map  : Custom_Maps_Manager.Custom_Data;
      Last_Index  : Positive := Maps.Last_Index;
   begin
      Utilities.Clear_Colour;
      Set_Background_Pane (Credits_Shader_Program, Use_Custom_Maps);

      if Use_Custom_Maps then
         for index in Custom_Maps.First_Index .. Custom_Maps.Last_Index loop
            Custom_Map := Custom_Maps.Element (index);
            Text.Draw_Text (Custom_Map.Text_ID);
         end loop;
      else
         if Last_Index > 8 then
            Last_Index := 8;
         end if;
         for index in Maps.First_Index .. Last_Index loop
            LeveL_Menu := Maps.Element (index);
            Text.Draw_Text (LeveL_Menu.Map_Name_Text_ID);
         end loop;
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

      if Menu_Open then
         Main_Menu.Draw_Menu (Delta_Time);
      end if;

      GUI.Draw_Controller_Button_Overlays (Delta_Time);
      Glfw.Windows.Context.Swap_Buffers (Window'Access);

   end Render;

   --  ------------------------------------------------------------------------

   procedure Reset_GUI_Level_Selection (Custom : Boolean) is
      use Level_Menu_Manager.Levels_Package;
      use Custom_Maps_Manager.Custom_Maps_Package;
      Map_Cursor        : Level_Menu_Manager.Levels_Package.Cursor :=
                            Maps.First;
      Custom_Map_Cursor : Custom_Maps_Manager.Custom_Maps_Package.Cursor :=
                            Custom_Maps.First;
      aMap              : Level_Menu_Manager.Level_Map_Data;
      Custom_Map        : Custom_Maps_Manager.Custom_Data;
   begin
      Selected_Map_ID := 1;
      --  Set map colour to white
      if Custom then
         while Has_Element (Custom_Map_Cursor) loop
            Custom_Map := Element (Custom_Map_Cursor);
            Text.Change_Text_Colour (Custom_Map.Text_ID, 1.0, 1.0, 1.0, 1.0);
            Custom_Maps.Replace_Element (Custom_Map_Cursor, Custom_Map);
            Next (Custom_Map_Cursor);
         end loop;
      else
         while Has_Element (Map_Cursor) and then To_Index (Map_Cursor) < 9 loop
            aMap := Element (Map_Cursor);
            Text.Change_Text_Colour (aMap.Map_Name_Text_ID, 1.0, 1.0, 1.0, 1.0);
            Maps.Replace_Element (Map_Cursor, aMap);
            Next (Map_Cursor);
         end loop;
      end if;
      Update_Selected_Entry_Dot_Map (False, Custom);

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
      Py         :constant  Single := -1.5 * Cos (0.09 * Now);
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
      Custom_Maps            : Boolean) return Boolean is
      use GL.Toggles;
      Menu_Open           : Boolean := Main_Menu.End_Story_Open;
      Menu_Quit           : Boolean := False;
      Cheat_Unlock        : Boolean := False;
      Started_Loading_Map : Boolean := False;
      Current_Time        : Float;
      Delta_Time          : Float;
      Last_Time           : Float := Float (Glfw.Time);
      Continue            : Boolean := True;
      Restart             : Boolean := False;
      Result              : Boolean := False;
   begin
      Reset_GUI_Level_Selection (Custom_Maps);
      while not Window.Should_Close and Continue loop
         Current_Time := Float (Glfw.Time);
         Delta_Time := Current_Time - Last_Time;
         Last_Time := Current_Time;
         if Menu_Open then
            --              Game_Utils.Game_Log ("Start_Level_Chooser_Loop Delta_Time" &
            --                                    Float'Image (Delta_Time));
            Menu_Quit := not Main_Menu.Update_Menu (Window, Delta_Time);
            if Main_Menu.Menu_Was_Closed then
               Menu_Open := False;
            end if;

            if Main_Menu.Did_User_Choose_New_Game or
              Main_Menu.Did_User_Choose_Custom_Maps then
               Menu_Open := False;
               Continue := False;
               Restart := True;
            elsif Menu_Quit then
               Continue := False;
            end if;
         else
            --              Game_Utils.Game_Log ("GUI_Level_Chooser.Start_Level_Chooser_Loop Update_GUI_Level_Chooser");
            Update_GUI_Level_Chooser (Delta_Time, Custom_Maps);
         end if;

         if Continue then
            Started_Loading_Map := False;
            if not Menu_Open then
               --                 Game_Utils.Game_Log ("GUI_Level_Chooser.Start_Level_Chooser_Loop Menu not Open");
               Process_Input (Window, Menu_Open, Started_Loading_Map, Cheat_Unlock);
            end if;

            Render (Window, Credits_Shader_Program, Delta_Time, Custom_Maps,
                    Started_Loading_Map, Menu_Open);
            Continue := not Started_Loading_Map;
            if Continue then
               --                 Poll_Joystick;
               Glfw.Input.Poll_Events;
               Continue := not Window.Should_Close;
            end if;

         end if;
      end loop;

      Enable (Depth_Test);

      if not Custom_Maps then
         Map_Unmodified := True;
         Result := Checksums;
      end if;

      if Result and Restart then
         Result := Start_Level_Chooser_Loop (Window, Credits_Shader_Program, False);
      end if;
      return Result;

   exception
      when others =>
         Put_Line ("An exception occurred in GUI_Level_Chooser.Start_Level_Chooser_Loop.");
         return False;

   end Start_Level_Chooser_Loop;

   --  ------------------------------------------------------------------------

   procedure Update_GUI_Level_Chooser (Delta_Time  : Float;
                                       Custom_Maps : Boolean) is
      use Glfw.Input.Keys;
      use Input_Callback;
      use Input_Handler;
      Old_Sel : constant Integer := Selected_Map_ID;
      Old_Map : Level_Menu_Manager.Level_Map_Data;
   begin
      Since_Last_Key := Since_Last_Key + Delta_Time;
      --        Game_Utils.Game_Log  ("Update_GUI_Level_Chooser Selected_Map_ID:" & Integer'Image (Selected_Map_ID));
      if Maps.Is_Empty then
         raise GUI_Level_Chooser_Exception with "Maps.Is_Empty ";
      end if;

      if Selected_Map_ID < Maps.First_Index or
        Selected_Map_ID > Maps.Last_Index then
         raise GUI_Level_Chooser_Exception with
           "Invalid Selected_Map_ID: " & Integer'Image (Selected_Map_ID);
      end if;

      --         Game_Utils.Game_Log  ("Update_GUI_Level_Chooser get old map Old_Sel: " &
      --                       Integer'Image (Old_Sel));
      Old_Map := Maps.Element (Old_Sel);

      if Since_Last_Key > 0.15 then
         if Is_Key_Down (Down) or Is_Action_Down (Down_Action) then
            if Selected_Map_ID >= 8 then   --  Last wanted map
               Selected_Map_ID := Maps.First_Index;
            else
               Selected_Map_ID := Selected_Map_ID + 1;
            end if;
            --              Play_Sound (LEVEL_BEEP_SOUND, true);
            Since_Last_Key := 0.0;
         elsif Is_Key_Down (Up) or Is_Action_Down (Up_Action) then
            if Selected_Map_ID = Maps.First_Index then
               Selected_Map_ID := 8;   --  Last wanted map
            else
               Selected_Map_ID := Selected_Map_ID - 1;
            end if;
            --              Play_Sound (LEVEL_BEEP_SOUND, true);
            Since_Last_Key := 0.0;
         end if;

         --           Game_Utils.Game_Log  ("Update_GUI_Level_Chooser check Custom_Maps ");
         if Custom_Maps then
            if Selected_Map_ID >= Num_Custom_Maps then
               Selected_Map_ID := Num_Custom_Maps - 1;
            end if;

            if Selected_Map_ID /= Old_Sel then
               Text.Change_Text_Colour (Selected_Map_ID, 1.0, 0.0, 1.0, 1.0);
               Text.Change_Text_Colour (Old_Sel, 1.0, 1.0, 1.0, 1.0);
               Update_Selected_Entry_Dot_Map (False, Custom_Maps);
            end if;
         else
            if not Old_Map.Locked then
               Text.Change_Text_Colour (Old_Sel, 1.0, 1.0, 1.0, 1.0);
            else
               Text.Change_Text_Colour (Old_Sel, 0.25, 0.25, 0.25, 1.0);
            end if;
            Update_Selected_Entry_Dot_Map (False, Custom_Maps);
         end if;
      end if;

   end Update_GUI_Level_Chooser;

   --  ------------------------------------------------------------------------

   procedure Update_Selected_Entry_Dot_Map (First, Custom : Boolean) is
      use Custom_Maps_Manager;
      Map_Path         : Unbounded_String;
      Left_Margin_Px   : constant Single := 550.0;  --  650.0
      Lt_Margin_Cl     : constant Single :=
                           Left_Margin_Px / Single (Settings.Framebuffer_Width);
      Has_Hammer_Track : Boolean := False;
      Title_Length     : Integer := 0;
      Story_Line       : Unbounded_String;
   begin
      --        Game_Utils.Game_Log
      --             ("GUI_Level_Chooser.Update_Selected_Entry_Dot_Map: '" &
      --          To_String (Selected_Map.Map_Title) & "'");
      if Selected_Map.Locked and not Custom then
         Selected_Map.Map_Title := To_Unbounded_String ("locked");
         Selected_Map.Map_Intro_Text.Clear;
         Selected_Map.Map_Intro_Text.Append (To_Unbounded_String
                                             ("clear previous temples to unlock" &
                                                  ASCII.CR & ASCII.LF & "the portal to this map"));
      elsif Custom then Map_Path := To_Unbounded_String
           ("src/maps/" & Get_Custom_Map_Name (Custom_Maps, Selected_Map_ID));
         --           Game_Utils.Game_Log ("level chooser is peeking in map " &
         --                                  To_String (Map_Path));
      else
         Map_Path := To_Unbounded_String
           ("src/maps/" & Level_Menu_Manager.Get_Map_Name
              (Maps, Selected_Map_ID));
         --           Game_Utils.Game_Log ("level chooser is peeking in map " &
         --                                  To_String (Map_Path));
         Selected_Map_Manager.Load_Map (To_String (Map_Path), Selected_Map,
                                        Has_Hammer_Track);
         Selected_Map_Track := Selected_Map.Music_Track;
      end if;

      if First then
         Game_Utils.Game_Log
           ("GUI_Level_Chooser.Update_Selected_Entry_Dot_Map first Map_Title: '" &
              To_String (Selected_Map.Map_Title) & "'");
         Level_Title_Text_ID :=
           Text.Add_Text (To_String (Selected_Map.Map_Title),
                          Left_Margin_Cl + Lt_Margin_Cl,
                          Top_Margin_Cl - 100.0 / Single (Settings.Framebuffer_Height),  --  180
                          30.0, 0.9, 0.9, 0.0, 0.8);
         Text.Set_Text_Visible (Level_Title_Text_ID, False);

         Game_Utils.Game_Log
           ("GUI_Level_Chooser.Update_Selected_Entry_Dot_Map first Last_Index: " &
              Integer'Image (Selected_Map.Map_Intro_Text.Last_Index));
         Level_Story_Text_ID.Clear;
         for index in Selected_Map.Map_Intro_Text.First_Index ..
           Selected_Map.Map_Intro_Text.Last_Index loop
            --              Game_Utils.Game_Log
            --                ("GUI_Level_Chooser.Update_Selected_Entry_Dot_Map first index: "
            --                  & Integer'Image (index) & "  '" &
            --                To_String (Selected_Map.Map_Intro_Text.Element (index)) & "'");
            Story_Line := Selected_Map.Map_Intro_Text.Element (index);
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
         Game_Utils.Game_Log
           ("GUI_Level_Chooser.Update_Selected_Entry_Dot_Map not first Map_Title: '" &
              To_String (Selected_Map.Map_Title) & "'");

         Text.Update_Text (Level_Title_Text_ID, To_String (Selected_Map.Map_Title));
         Level_Story_Text_ID.Clear;
         for index in Selected_Map.Map_Intro_Text.First_Index ..
           Selected_Map.Map_Intro_Text.Last_Index loop
            --              Game_Utils.Game_Log ("GUI_Level_Chooser.Update_Selected_Entry_Dot_Map index: "
            --                                  & integer'Image (index));
            Story_Line := Selected_Map.Map_Intro_Text.Element (index);
            --              Game_Utils.Game_Log ("GUI_Level_Chooser.Update_Selected_Entry_Dot_Map line: "
            --                                  & To_String (Story_Line));
            if Length (Story_Line) < 1 then
               Story_Line := To_Unbounded_String (" ");
            end if;
--              if index <= Level_Story_Text_ID.Last_Index then
--                 Text.Update_Text (Level_Story_Text_ID.Element (index),
--                                   To_String (Story_Line));
--              else
               Level_Story_Text_ID.Append
                 (Text.Add_Text (To_String (Story_Line),
                  Left_Margin_Cl + Lt_Margin_Cl,
                  Top_Margin_Cl - (1.0 + 0.2 * Single (index - 1)) * 240.0 / Single
                  (Settings.Framebuffer_Height),
                  14.0, 0.75, 0.75, 0.75, 1.0));
               Text.Set_Text_Visible (Level_Story_Text_ID.Element (index), False);
--              end if;
         end loop;
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in GUI_Level_Chooser.Update_Selected_Entry_Dot_Map.");
   end Update_Selected_Entry_Dot_Map;

   --  ------------------------------------------------------------------------

end GUI_Level_Chooser;
