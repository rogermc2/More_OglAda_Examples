
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Culling;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Toggles;
with GL.Types; use GL.Types;
with GL.Types.Colors;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Utilities;

with Audio;
with Audio_Manager;
with Blood_Splats;
with Camera;
with Character_Controller;
with Controller_Textures_Manager;
with FB_Effects;
with Game_Utils;
with GL_Utils;
with GUI;
with GUI_Level_Chooser;
with Input_Handler;
with Level_Menu_Manager;
with Main_Loop.Game_Support;
with Manifold;
with Maps_Manager;
with Mesh_Loader;
with Main_Menu;
with Particle_System;
with Projectile_Manager;
with Prop_Renderer;
with Settings;
with Shader_Manager;
with Shadows;
with Splats_Shader_Manager;
with Sprite_Shader_Manager;
with Sprite_Renderer;
with Sprite_World_Map;
with Text;
with Texture_Manager;

package body Main_Loop is

    procedure Main_Loop (Main_Window : in out Input_Callback.Barbarian_Window) is

        Black                   : constant Colors.Color := (0.0, 0.0, 0.0, 1.0);
        --     Red            : constant GL.Types.Singles.Vector4 := (1.0, 0.0, 0.0, 1.0);
        --     Green          : constant GL.Types.Singles.Vector4  := (0.0, 0.5, 0.0, 1.0);
        Blue           : constant Colors.Color := (0.0, 0.0, 0.5, 1.0);
        --     Magenta        : constant GL.Types.Singles.Vector4 := (1.0, 0.0, 1.0, 1.0);
        --     Yellow         : constant GL.Types.Singles.Vector4 := (1.0, 1.0, 0.0, 0.5);
        White                   : constant Colors.Color := (1.0, 1.0, 1.0, 0.0);
        Title_Track             : constant String := "Warlock_Symphony.ogg";
        Is_Playing_Hammer_Track : Boolean := False;
        Key_Pressed             : boolean := False;
        Last_Time               : float := Float (Glfw.Time);

        Logic_Step_Seconds   : constant Float := 0.01;
        --      Char_Map_Tell        : Integer;
        --     Fps_Text             : Integer;
        Max_Steps_Per_Frame             : Integer;
        Game_Map                        : Maps_Manager.Map;
        Game_Camera                     : Camera.Camera_Data := Camera.Default_Camera;
        Level_Name                      : Unbounded_String :=
                                            To_Unbounded_String ("anton2");
        Level_Time                      : Float := 0.0;
        Cheated_On_Map                  : Boolean := False;
        Quit_Game                       : Boolean := False;
        --          Skip_Intro_Screen_And_Main_Menu : Boolean := True;

        Avg_Frame_Time_Accum_Ms         : Float := 0.0;
        Curr_Frame_Time_Accum_Ms        : Float := 0.0;
        Avg_Frames_Count                : Integer := 0;
        Curr_Frames_Count               : Integer := 0;
        --     Batching_Mode            : Boolean := True;
        --     Reserve_video_Memory     : Boolean := True;
        Dump_Video                      : Boolean := False;
        --     Draw_Debug_Quads         : Boolean;
        --      Param                   : Integer := 0;
        Camera_Height                   : constant GL.Types.Single := 13.0;
        Changed_Camera_Height           : constant Boolean := False;
        Fps_Text_Index                  : Integer;

        Initialize_Exception            : Exception;
        Update_Exception                : Exception;

        procedure Update_Logic_Steps (Window : in out Input_Callback.Barbarian_Window;
                                      Seconds : Float);

        --  ------------------------------------------------------------------------

        procedure Game_Loop (Window : in out Input_Callback.Barbarian_Window;
                             Tile_Tex, Tile_Spec_Tex, Ramp_Diff_Tex,
                             Ramp_Spec_Tex : GL.Objects.Textures.Texture) is
            use Glfw.Input.Keys;
            use Game_Support;
            use GUI_Level_Chooser;
            Is_Running       : Boolean := True;
            Main_Menu_Quit   : Boolean := False;
            Last_Time        : Float := Float (Glfw.Time);
            Delta_Time       : Float := 0.0;
            Video_Timer      : Float := 0.0;
            Video_Dump_Timer : Float := 0.0;
            Frame_Time       : Float := 0.04;
            Save_Screenshot  : Boolean := False;
            Dump_Video       : Boolean := False;
            Cheating         : Boolean := False;
        begin
            Game_Utils.Game_Log ("Main_Loop.Game_Loop");
            Put_Line ("Main_Loop.Game_Loop");
            Game_Camera.Is_Dirty := True;
            while Is_Running loop
                Update_Timers (Last_Time, Delta_Time, Avg_Frame_Time_Accum_Ms,
                               Curr_Frame_Time_Accum_Ms, Avg_Frames_Count,
                               Curr_Frames_Count);
                if Main_Menu.Menu_Open then
                    --                      Game_Utils.Game_Log
                    --                        ("Main_Loop.Game_Loop Main Menu open");
                    Main_Menu_Quit := not Main_Menu.Update_Main_Menu
                      (Window, Delta_Time);
                    if Main_Menu.Menu_Was_Closed then
                        Main_Menu.Set_Menu_Open (False);
                        FB_Effects.Set_Feedback_Effect (FB_Effects.FB_Default);
                    end if;
                    Game_Utils.Game_Log
                      ("Main_Loop.Game_Loop check if User_Choose_New_Game");
                    if Main_Menu.Did_User_Choose_New_Game or
                      Main_Menu.Did_User_Choose_Custom_Maps then
                        Main_Menu.Set_Menu_Open (False);
                        Quit_Game := False;
                        Unload_Level;
                        Is_Running := False;
                    end if;
                else  --  Main_Menu not Open
                    --                      Game_Utils.Game_Log
                    --                        ("Main_Loop.Game_Loop Main Menu not open");
                    Level_Time := Level_Time + Delta_Time;
                end if;  --  Main_Menu_Open

                if Is_Running then
                    if Settings.Video_Record_Mode and Dump_Video then
                        Video_Timer := Video_Timer + Delta_Time;
                        Video_Dump_Timer := Video_Dump_Timer + Delta_Time;
                        Is_Running := Video_Timer < Float (GL_Utils.Video_Seconds_Total);
                    end if;  --  Video processing

                    if Is_Running then
                        Audio.Update_Ambient_Sounds;
                        Audio.Update_Boulder_Sounds;
                        --   Check_Keys;  -- DEBUG MODE!
                        Save_Screenshot :=
                          Input_Callback.Was_Key_Pressed (Window, F11);
                        if Settings.Video_Record_Mode and
                          Input_Callback.Was_Key_Pressed (Window, Backspace) then
                            Dump_Video := not Dump_Video;
                            Video_Timer := 0.0;
                            Put_Line ("==RECORDING VIDEO==");
                        end if;
                        --  Do cheating checks
                        Cheating := Cheat_Check_1;
                        if not Main_Menu.Menu_Open then
                            Game_Utils.Game_Log
                              ("Main_Loop.Game_Loop Is_Running Main Menu not open");
                            GUI.Update_GUIs (Delta_Time);
                            Text.Update_Comic_Texts (Delta_Time);
                            Text.Update_Particle_Texts (Delta_Time);
                            --  Check_Victory_Defeat checks that if the
                            --  "defeated" gui is up then controls aren't
                            --  updated except space to restart.
                            --  Note that this reloads and continues execution
                            --  of game as normal.
                            --  Major states stacks don't change or anything
                            Is_Running := Check_Victory_Defeat;

                            if Is_Running then
                                Camera.Update_Camera_Effects (Delta_Time);
                                Update_Logic_Steps (Window, Delta_Time);
                                if Main_Menu.End_Story_Open then
                                    --  Just won the game
                                    Game_Utils.Game_Log
                                      ("Main_Loop.Game_Loop Just won the game");
                                    Main_Menu.Set_Menu_Open (True);
                                    Unload_Level;
                                    Is_Running := False;
                                elsif Input_Handler.Was_Action_Pressed
                                  (Window, Input_Handler.Wipe_Screen_Action) then
                                    Game_Utils.Game_Log
                                      ("Main_Loop.Main_Game_Loop Action_Pressed");
                                    GUI.Start_Fist;
                                end if;
                            end if;
                        end if;  --  Main menu not open;

                        if Is_Running then
                            Player_1_View (Window, Tile_Tex, Tile_Spec_Tex,
                                           Ramp_Diff_Tex, Ramp_Spec_Tex,
                                           Delta_Time, Dump_Video,
                                           Save_Screenshot);
                        end if;
                        --                          Game_Utils.Game_Log
                        --                            ("Main_Loop.Game_Loop Player_1_View returned");
                        Is_Running := Is_Running and then not Main_Menu_Quit;
                        Is_Running := Is_Running and then not Main_Window.Should_Close;
                        Delay (0.1);
                    end if;  --  inner Is_Running
                end if;  --  Is_Running

                --                  Game_Utils.Game_Log ("Main_Loop.Main_Game_Loop end loop Window.Should_Close: "
                --                                       & Boolean'Image (Window.Should_Close));
                Is_Running := Is_Running and
                  not Window.Should_Close and not Main_Menu_Quit;
                --                  Game_Utils.Game_Log ("Main_Loop.Main_Game_Loop end loop Is_Running: "
                --                                       & Boolean'Image (Is_Running));
            end loop;
            Quit_Game := True;

        exception
            when others =>
                Put_Line ("Main_Loop.Game_Loop exception");
                raise;

        end Game_Loop;

        --  --------------------------------------------------------------------

        procedure Init_Modules (Window : in out Input_Callback.Barbarian_Window) is
            Window_Width   : Glfw.Size;
            Window_Height  : Glfw.Size;
            Width          :  Single;
            Height         :  Single;
        begin
            Window.Get_Framebuffer_Size (Window_Width, Window_Height);
            Width := Single (Window_Width);
            Height := Single (Window_Height);
            Input_Handler.Register_Input_Actions;
            Settings.Load_Settings;
            Shader_Manager.Init;
            Audio_Manager.Init;
            Texture_Manager.Init;
            Controller_Textures_Manager.Load_Controller_Textures;
            Mesh_Loader.Init;
            Camera.Init;
            if Changed_Camera_Height then
                Camera.Set_Camera_Height (Camera_Height);
            end if;
            Text.Init_Text_Rendering
              ("src/textures/comicscript.png", "src/fonts/comicscript.meta",
               Settings.Framebuffer_Width, Settings.Framebuffer_Height);
            Text.Init_Particle_Texts;
            Fps_Text_Index := Text.Add_Text ("fps: batches: vertices: ",
                                             -1.0, 1.0, 15.0, (1.0, 1.0, 0.0, 0.9));
            Text.Set_Text_Visible (Fps_Text_Index, False);
            Text.Init_Comic_Texts;

            GL_Utils.Set_Render_Defaults;
            GUI.Load_Gui_Shaders;

            Particle_System.Init;
            Prop_Renderer.Init;
            Sprite_Renderer.Init;
            GUI.Init_GUIs;
            Blood_Splats.Init;
            FB_Effects.Init (Integer (Window_Width), Integer (Window_Height));
            Shadows.Init;
            Manifold.Init;
            Main_Menu.Init;
            Input_Handler.Init (Window);
            Game_Utils.Game_Log ("----MODULES INITIALIZED----");

        end Init_Modules;

        --  ----------------------------------------------------------------------

        procedure Introduction
          (Window                 : in out Input_Callback.Barbarian_Window;
           Last_Time, Flash_Timer : in out Float; Is_Running : in out Boolean) is
            use Glfw.Input.Keys;
            use Maths.Single_Math_Functions;
            use Input_Callback;
            use Input_Handler;
            Current_Time  : Float;
            Elapsed_Time  : Float;
            b             : GL.Types.Single := 0.0;
            Back_Colour   : Colors.Color;
            Window_Closed : Boolean := False;
        begin
            Game_Utils.Game_Log ("---Main_Loop.Introduction---");
            Is_Running := True;
            Game_Camera.Is_Dirty := True;
            while Is_Running and not Window_Closed loop
                Current_Time := Float (Glfw.Time);
                Elapsed_Time := Current_Time - Last_Time;
                Last_Time := Current_Time;
                if Flash_Timer < 0.25 then
                    Flash_Timer := Flash_Timer + Elapsed_Time;
                    b := Abs (Sin (Single ((30.0)) * Single (Current_Time)));
                    b := 0.4;
                    Back_Colour := (b, b, b, 1.0);
                    Utilities.Clear_Background_Colour_And_Depth (Back_Colour);
                else
                    b := 0.0;
                    b := 0.7;
                    Back_Colour := (b, b, b, 1.0);
                    Utilities.Clear_Background_Colour_And_Depth (Back_Colour);
                    Main_Menu.Draw_Title_Only;
                end if;
                GUI.Draw_Controller_Button_Overlays (Elapsed_Time);
                Glfw.Input.Poll_Events;
                Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);

                Game_Camera.Is_Dirty := False;
                if Was_Key_Pressed (Window, Escape) or
                  Was_Key_Pressed (Window, Space) or
                  Was_Key_Pressed (Window, Enter) or
                  Was_Action_Pressed (Window, OK_Action) or
                  Was_Action_Pressed (Window, Attack_Action) then
                    Is_Running := False;
                end if;

                Window_Closed := Window.Should_Close;
                if Window_Closed then
                    Game_Utils.Game_Log ("Window closed by user or system ...exiting");
                end if;
                Game_Camera.Is_Dirty := False;
            end loop;  --  Is_Running

        end Introduction;

        --  --------------------------------------------------------------------

        procedure Main_Setup (Window     : in out Input_Callback.Barbarian_Window;
                              Is_Running : in out Boolean) is
            use Glfw.Input;
            Current_Time  : Float := Float (Glfw.Time);
            Delta_Time    : Float := 0.0;
            Flash_Timer   : Float := 0.0;
        begin
            Game_Utils.Game_Log ("Main_Loop.Main_Setup started");
            Window.Set_Input_Toggle (Glfw.Input.Sticky_Keys, True);
            --        Utilities.Enable_Mouse_Callbacks (Window, True);
            --        Window.Enable_Callback (Glfw.Windows.Callbacks.Char);
            --        Window.Enable_Callback (Glfw.Windows.Callbacks.Position);

            Window.Enable_Callback (Glfw.Windows.Callbacks.Framebuffer_Size);
            Window.Enable_Callback (Glfw.Windows.Callbacks.Size);
            --          Param := Game_Utils.Check_Param ("-map");

            Init_Modules (Window);
            --          Play_Music (Title_Track);
            --          Is_Playing_Hammer_Track := False;

            --              if not Skip_Intro_Screen_And_Main_Menu then
            Introduction (Window, Last_Time, Flash_Timer, Is_Running);
            --              end if;
            Is_Running := True;

        exception
            when others =>
                Put_Line ("An exception occurred in Main_Loop.Main_Setup.");
                raise;
        end Main_Setup;

        --  ------------------------------------------------------------------------

        procedure Run_Game (Window : in out Input_Callback.Barbarian_Window) is
            Window_Width   : Glfw.Size;
            Window_Height  : Glfw.Size;
            Width           : GL.Types.Single;
            Height          : GL.Types.Single;
            Map_Path        : Unbounded_String;
            Ambient_Light   : constant Singles.Vector3 := (0.025, 0.025, 0.025);
            Tile_Tex        : GL.Objects.Textures.Texture;
            Tile_Spec_Tex   : GL.Objects.Textures.Texture;
            Ramp_Diff_Tex   : GL.Objects.Textures.Texture;
            Ramp_Spec_Tex   : GL.Objects.Textures.Texture;
            Continue        : Boolean := True;
        begin
            Game_Utils.Game_Log ("Main_Loop.Run_Game started");
            Put_Line ("Main_Loop.Run_Game started");
            while Continue and not Quit_Game loop
                Window.Get_Framebuffer_Size (Window_Width, Window_Height);
                Width := Single (Window_Width);
                Height := Single (Window_Height);
                GL.Window.Set_Viewport (0, 0, Int (Width), Int (Height));

                Game_Utils.Game_Log ("Main_Loop.Run_Game Opening level map file " &
                                       To_String (Level_Name));
                --                  if not Skip_Intro_Screen_And_Main_Menu then
                --                      Put_Line ("Main_Loop.Run_Game starting Level_Chooser_Loop");
                Continue := GUI_Level_Chooser.Start_Level_Chooser_Loop
                  (Window, Main_Menu.Credits_Program,
                   Main_Menu.Are_We_In_Custom_Maps);
                --                  end if;

                if Continue then
                    --  Even if flagged to skip initial intro this means that
                    --  the level chooser can be accessed if the player selects
                    --  "new game" in the main menu???
                    --  Level Chooser should start on next iteration of this loop.
                    --                      Skip_Intro_Screen_And_Main_Menu := False;
                    Level_Name := To_Unbounded_String
                      (GUI_Level_Chooser.Get_Selected_Level_Name
                         (Main_Menu.Are_We_In_Custom_Maps));
                    Game_Utils.Game_Log
                      ("Main_Loop.Run_Game Start_Level_Chooser_Loop Level_Name "
                       & To_String (Level_Name));
                    Put_Line
                      ("Main_Loop.Run_Game Start_Level_Chooser_Loop Level_Name "
                       & To_String (Level_Name));

                    --  Level has been selected, start creating level
                    Map_Path := To_Unbounded_String ("src/maps/") & Level_Name;
                    --  Name line
                    Game_Utils.Game_Log ("Main_Loop.Run_Game Opening map file " &
                                           To_String (Map_Path));
                    Maps_Manager.Load_Maps (To_String (Map_Path), Game_Map,
                                            Tile_Tex, Tile_Spec_Tex,
                                            Ramp_Diff_Tex, Ramp_Spec_Tex);
                    --  Properties and characters are loaded by Load_Maps
                    Projectile_Manager.Init;

                    Utilities.Clear_Background_Colour_And_Depth (White);
                    GL.Toggles.Enable (GL.Toggles.Depth_Test);
                    GL.Toggles.Enable (GL.Toggles.Cull_Face);
                    GL.Culling.Set_Cull_Face (GL.Culling.Back);

                    Game_Utils.Game_Log ("---LEVEL START---");
                    Put_Line ("---LEVEL START---");
                    Sprite_Shader_Manager.Use_Sprite_Shader;
                    Sprite_Renderer.Set_Ambient_Light_Level (Ambient_Light);
                    Manifold.Set_Manifold_Ambient_Light (Ambient_Light);
                    Prop_Renderer.Set_Ambient_Light_Level (Ambient_Light);
                    Blood_Splats.Use_Splats_Shader;
                    Splats_Shader_Manager.Set_Ambient_Light (Ambient_Light);

                    FB_Effects.Fade_In;
                    Manifold.Update_Static_Lights_Uniforms;
                    Prop_Renderer.Update_Static_Lights_Uniforms;
                    Sprite_Renderer.Update_Static_Lights_Uniforms;

                    Camera.Camera_Wind_In;  --  Camera screw-in effect
                    Audio.Play_Sound ("enter_portal.wav", False);

                    Level_Time := 0.0;
                    Game_Loop (Window, Tile_Tex, Tile_Spec_Tex,
                               Ramp_Diff_Tex, Ramp_Spec_Tex);

                    if Main_Menu.End_Story_Open then
                        Main_Menu.Play_End_Story_Music;
                    else
                        Audio.Play_Music (Title_Track);
                    end if;
                    Is_Playing_Hammer_Track := False;
                    Quit_Game := Window.Should_Close;
                end if;  --  Continue
            end loop;

        exception
            when others =>
                Put_Line ("An exception occurred in Main_Loop.Run_Game.");
                raise;
        end Run_Game;

        --  --------------------------------------------------------------------

        procedure Run_Main_Menu (Window     : in out Input_Callback.Barbarian_Window;
                                 Is_Running : in out Boolean) is
            use Glfw.Input;
            Current_Time  : Float := Float (Glfw.Time);
            Delta_Time    : Float := 0.0;
            Flash_Timer   : Float := 0.0;
        begin
            --  initiate main menu loop
            Main_Menu.Start_Menu_Title_Bounce;
            Utilities.Clear_Background_Colour_And_Depth (Black);
            Utilities.Clear_Background_Colour_And_Depth (White);

            --              if not Skip_Intro_Screen_And_Main_Menu then
            Main_Menu.Set_Menu_Open (True);
            --              end if;

            Is_Running := True;
            Last_Time := Float (Glfw.Time);

            while Main_Menu.Menu_Open and Is_Running loop
                GL_Utils.Window_Resize (Window);
                GL_Utils.Frame_Buffer_Resize (Window);

                Current_Time := Float (Glfw.Time);
                Delta_Time := Current_Time - Last_Time;
                Last_Time := Current_Time;
                Utilities.Clear_Background_Colour_And_Depth (Black);
                Utilities.Clear_Background_Colour_And_Depth (Blue);

                Main_Menu.Draw_Menu (Delta_Time);

                GUI.Draw_Controller_Button_Overlays (Delta_Time);
                Glfw.Input.Poll_Events;
                --           --  Poll_Joystick
                Glfw.Windows.Context.Swap_Buffers (Window'Access);

                if not Main_Menu.Update_Main_Menu (Window, Delta_Time) then
                    Main_Menu.Set_Menu_Open (False);
                    Quit_Game := True;
                end if;

                if Main_Menu.Did_User_Choose_New_Game or
                  Main_Menu.Did_User_Choose_Custom_Maps then
                    Main_Menu.Set_Menu_Open (False);
                end if;
                Is_Running := not Window.Should_Close;
            end loop;

        exception
            when others =>
                Put_Line ("An exception occurred in Main_Loop.Run_Main_Menu.");
                raise;
        end Run_Main_Menu;

        --  ------------------------------------------------------------------------

        procedure Update_Logic_Steps (Window  : in out Input_Callback.Barbarian_Window;
                                      Seconds : Float) is
            Accum_Time : Float := Seconds;
            Time_Step  : Integer := 0;        begin
            while Accum_Time >= Logic_Step_Seconds loop
                Character_Controller.Update_Characters (Window, Logic_Step_Seconds);
                Prop_Renderer.Update_Properties (Logic_Step_Seconds);
                Projectile_Manager.Update_Projectiles (Logic_Step_Seconds);
                Time_Step := Time_Step + 1;
                Accum_Time := Accum_Time - Logic_Step_Seconds;
            end loop;
            Max_Steps_Per_Frame := Game_Utils.Max (Max_Steps_Per_Frame, Time_Step);

        exception
            when others =>
                Put_Line ("An exception occurred in Main_Loop.Update_Logic_Steps.");
                raise;

        end Update_Logic_Steps;

        --  ----------------------------------------------------------------------

        use Glfw.Input;
        Running : Boolean := True;
    begin
        Main_Window.Set_Input_Toggle (Sticky_Keys, True);
        Game_Utils.Restart_Game_Log;
        Main_Setup (Main_Window, Running);
        Run_Main_Menu (Main_Window, Running);

        if Running then
            GUI_Level_Chooser.Init;
            Run_Game (Main_Window);
        end if;
        Game_Utils.Close_Game_Log;

    end Main_Loop;

end Main_Loop;
