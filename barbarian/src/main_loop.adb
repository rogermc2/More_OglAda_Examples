
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Culling;
--  with GL.Text;
with GL.Toggles;
with GL.Types; use GL.Types;
with GL.Types.Colors;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

--  with Maths;
with Utilities;

--  with GL_Util;

with Audio;
with Blood_Splats;
with Camera;
--  with Character_Controller;
with FB_Effects;
with Game_Utils;
with GL_Utils;
with GUI;
with GUI_Level_Chooser;
with Input_Handler;
with Manifold;
with Maps_Manager;
with Mesh_Loader;
with MMenu;
with Particle_System;
--  with Projectile_Manager;
--  with Prop_Renderer;
with Settings;
with Shader_Manager;
with Shadows;
with Sprite_Renderer;
with Text;
with Texture_Manager;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    Black          : constant Colors.Color := (0.0, 0.0, 0.0, 1.0);
    --     Red            : constant GL.Types.Singles.Vector4 := (1.0, 0.0, 0.0, 1.0);
    --     Green          : constant GL.Types.Singles.Vector4  := (0.0, 0.5, 0.0, 1.0);
    --     Blue           : constant GL.Types.Singles.Vector4  := (0.0, 0.0, 0.5, 1.0);
    --     Magenta        : constant GL.Types.Singles.Vector4 := (1.0, 0.0, 1.0, 1.0);
    --     Yellow         : constant GL.Types.Singles.Vector4 := (1.0, 1.0, 0.0, 0.5);
    White          : constant Colors.Color := (1.0, 1.0, 1.0, 0.0);
    Key_Pressed    : boolean := False;
--      Flash_Timer    : float := 0.0;
--      Lt             : float := float (Glfw.Time);
    Last_Time      : float := 0.0;
    Mmenu_Open     : Boolean := True;

--      Logic_Step_Seconds   : constant Float := 0.01;
    Char_Map_Tell        : Integer;
    --     Fps_Text             : Integer;
--      Max_Steps_Per_Frame  : Integer;
    Game_Map             : Maps_Manager.Map;
    Level_Name            : Unbounded_String :=
                                 To_Unbounded_String ("anton2");
    Quit_Game            : Boolean := False;
    Skip_Intro           : Boolean := True;
    --     Batching_Mode        : Boolean := True;
    --
    --     Reserve_video_Memory : Boolean := True;
    --     Dump_Video           : Boolean;
    --     Draw_Debug_Quads     : Boolean;

    Window_Width        : Glfw.Size;
    Window_Height       : Glfw.Size;
--      Param                  : Integer := 0;
    Camera_Height          : constant GL.Types.Single := 13.0;
    Changed_Camera_Height  : constant Boolean := False;
    Fps_Text               : Integer;

    Initialize_Exception   : Exception;
--      Update_Exception       : Exception;

--      function Update_Logic_Steps (Seconds : Float) return Boolean is
--          Accum_Time : Float := Seconds;
--          Time_Step  : Integer := 0;
--          OK         : Boolean := True;
--      begin
--          while OK and Accum_Time >= Logic_Step_Seconds loop
--              OK := Character_Controller.Update_Characters (Logic_Step_Seconds);
--              if not OK then
--                  raise Update_Exception with
--                    "Update_Logic_Steps, error updating characters";
--              end if;
--              OK := Prop_Renderer.Update_Props (Logic_Step_Seconds);
--              if not OK then
--                  raise Update_Exception with
--                    "Update_Logic_Steps, error updating props";
--              end if;
--              OK := Projectile_Manager.Update_Projectiles (Logic_Step_Seconds);
--              if not OK then
--                  raise Update_Exception with
--                    "Update_Logic_Steps, error updating projectiles";
--              end if;
--              Time_Step := Time_Step + 1;
--              Accum_Time := Accum_Time + Logic_Step_Seconds;
--          end loop;
--          Max_Steps_Per_Frame := Game_Utils.Max (Max_Steps_Per_Frame, Time_Step);
--
--          return OK;
--      end Update_Logic_Steps;

    --  ----------------------------------------------------------------------------

    procedure Run_Game (Window : in out Glfw.Windows.Window) is
    --          use GL.Objects.Buffers;
    --          use GL.Types.Colors;
    --          use GL.Types.Singles;     --  for matrix multiplication
        Width               : GL.Types.Single;
        Height              : GL.Types.Single;
        Map_Path            : Unbounded_String;
        --        Model_Matrix        : constant Matrix4 := Identity4;
        --          Translation_Matrix  : Matrix4 := Identity4;
        --          Projection_Matrix   : Matrix4 := Identity4;
        --          View_Matrix         : Matrix4 := Identity4;
        --          View_Angle          : constant Maths.Degree := 50.0;
        --          View_Matrix         : Matrix4 := Identity4;
        --          Camera_Position     : constant Vector3 := (0.0, 0.0, 5.0);
        --          Half_Pi             : constant Single := 0.5 * Ada.Numerics.Pi;
        --          Horizontal_Angle    : constant Single := Ada.Numerics.Pi;
        --          Vertical_Angle      : constant Single := 0.0;
        --          Direction           : Vector3;
        --          Right               : Vector3;
        --          Up                  : Vector3;

    begin
        if not Skip_Intro then
            if GUI_Level_Chooser.Start_Level_Chooser_Loop
              (MMenu.Are_We_In_Custom_Maps) then
                Level_Name := To_Unbounded_String
              (GUI_Level_Chooser.Get_Selected_Map_Name (MMenu.Are_We_In_Custom_Maps));
            end if;
        end if;
	--   Even if flagged to skip initial intro this means that the level
	--  chooser can be accessed if the player selects "new game" in the main menu.
        Skip_Intro := False;
        --  Level has been selected, start creating level
        Map_Path := To_Unbounded_String ("maps/") & Level_Name &
          To_Unbounded_String (".map");
        --  Name line
        Game_Utils.Game_Log ("Opening map file " & To_String (Map_Path));
        Maps_Manager.Load_Map (To_String (Map_Path), Game_Map, Char_Map_Tell);
        --  Properties are loaded by Load_Map
        Game_Utils.Game_Log ("Game map loaded, Char_Map_Tell: " &
                              Integer'Image (Char_Map_Tell));

        Window.Get_Framebuffer_Size (Window_Width, Window_Height);
        Width := Single (Window_Width);
        Height := Single (Window_Height);
        GL.Window.Set_Viewport (0, 0, Int (Width), Int (Height));

        --          Maths.Init_Perspective_Transform
        --            (View_Angle, Width, Height, 0.1, -100.0, Projection_Matrix);
        --          Shader_Manager.Set_Projection_Matrix (Projection_Matrix);

        --          Translation_Matrix := Maths.Translation_Matrix ((0.0, 0.0, -14.0));
        --
        --          View_Matrix := Translation_Matrix * View_Matrix;
        --          Shader_Manager.Set_View_Matrix (View_Matrix);
        --        View_Matrix := Translation_Matrix * View_Matrix;
        --        Shader_Manager.Set_View_Matrix (View_Matrix);
        --  View and model matrices are initilized to identity by
        --  shader initialization.
        Utilities.Clear_Background_Colour_And_Depth (White);
        GL.Toggles.Enable (GL.Toggles.Depth_Test);
        GL.Toggles.Enable (GL.Toggles.Cull_Face);
        GL.Culling.Set_Cull_Face (GL.Culling.Back);

    exception
        when others =>
            Put_Line ("An exception occurred in Main_Loop.Run_Game.");
            raise;
    end Run_Game;

    --  ------------------------------------------------------------------------

    procedure Init_Modules is
    begin
        if not Shader_Manager.Init_Shaders then
            raise Initialize_Exception with "Init_Shaders failed.";
        end if;
        if not Audio.Init_Audio then
            raise Initialize_Exception with "Init_Audio failed.";
        end if;
        Texture_Manager.Init_Texture_Manager;
        if not GUI.Load_Controller_Textures then
            raise Initialize_Exception with "Init_Texture_Manager failed.";
        end if;
        Mesh_Loader.Init;
        Camera.Init_Camera;
        if not Changed_Camera_Height then
            raise Initialize_Exception with "Changed_Camera_Height failed.";
        end if;
        Camera.Set_Camera_Height (Camera_Height);
        if not Text.Init_Text_Rendering
          ("textures/comicscript.png", "fonts/comicscript.meta",
           Settings.Framebuffer_Width, Settings.Framebuffer_Height) then
            raise Initialize_Exception with "Init_Text_Rendering failed.";
        end if;
        if not Particle_System.Init_Particle_Systems then
            raise Initialize_Exception with "Init_Particle_Systems failed.";
        end if;
        Sprite_Renderer.Init_Sprite_Renderer;
        if not GUI.Init_GUIs then
            raise Initialize_Exception with "Init_Sprite_Renderer failed.";
        end if;
        if not Blood_Splats.Init_Blood_Splats then
            raise Initialize_Exception with "Init_Blood_Splats failed.";
        end if;
        if not FB_Effects.Init_FB
          (Integer (Window_Width),
           Integer (Window_Height)) then
            raise Initialize_Exception with "Init_FB failed.";
        end if;
        if not Shadows.Init_Shadows then
            raise Initialize_Exception with "Init_Shadows failed.";
        end if;
        if not Manifold.Init_Manifold then
            raise Initialize_Exception with "Init_Manifold failed.";
        end if;
        if not MMenu.Init_MMenu then
            raise Initialize_Exception with "Init_MMenu failed.";
        end if;
        if not Input_Handler.Init_Input_Handler then
            raise Initialize_Exception with "Init_Input_Handler failed.";
        end if;
    end Init_Modules;

    --  ------------------------------------------------------------------------

    procedure Setup is
    --          use GL.Objects.Buffers;
    --        Font_File : string := "../fonts/Helvetica.ttc";
        Current_Time : Float := 0.0;
        Delta_Time   : Float := 0.0;
    begin
--          Param := Game_Utils.Check_Param ("-map");
        Text.Init_Particle_Texts;
        Fps_Text := Text.Add_Text ("fps: batches: vertices: ",
                                   -1.0, 1.0, 15.0, 1.0, 1.0, 0.0, 0.9);
        Text.Set_Text_Visible (Fps_Text, False);
        Text.Init_Comic_Texts;
        GL_Utils.Set_Render_Defaults;
        GUI.Load_Gui_Shaders;
        Init_Modules;

        MMenu.Start_Mmenu_Title_Bounce;
        Utilities.Clear_Background_Colour_And_Depth (Black);
        Last_Time := GL_Utils.Get_Elapsed_Seconds;
        GL.Window.Set_Viewport (0, 0, Settings.Framebuffer_Width,
                                Settings.Framebuffer_Height);
        while Mmenu_Open loop
            Current_Time := GL_Utils.Get_Elapsed_Seconds;
            Delta_Time := Current_Time - Last_Time;
            Last_Time := Current_Time;
            Utilities.Clear_Background_Colour_And_Depth (Black);
            MMenu.Draw_Menu (Delta_Time);
            GUI.Draw_Controller_Button_Overlays (Delta_Time);
            Glfw.Input.Poll_Events;
            Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);

            Quit_Game := not MMenu.Update_MMenu (Delta_Time);
            Mmenu_Open := not Quit_Game;
            Mmenu_Open := Mmenu_Open and not (MMenu.Did_User_Choose_New_Game or
              MMenu.Did_User_Choose_Custom_Maps);
        end loop;

        if not Main_Window.Should_Close then
            GUI_Level_Chooser.Init_GUI_Level_Chooser;
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in Main_Loop.Setup.");
            raise;
    end Setup;

    --  ----------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean := True;
    Key_Now : Button_State;
begin
    Utilities.Clear_Background_Colour_And_Depth (White);
    Main_Window.Set_Input_Toggle (Sticky_Keys, True);
    Glfw.Input.Poll_Events;
    Setup;
    Running := not Main_Window.Should_Close;
    while Running loop
        --  Swap_Buffers first to display background colour on start up.
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Run_Game (Main_Window);
        --        Delay (0.2);
        Glfw.Input.Poll_Events;

        Key_Now := Main_Window.Key_State (Glfw.Input.Keys.Space);
        if not Key_Pressed and Key_Now = Glfw.Input.Pressed then
            Key_Pressed := True;
        else
            Key_Pressed := Key_Now = Glfw.Input.Pressed;
        end if;
        --     Delay (3.0);
        Running := Running and then not Quit_Game and then
          not (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and then not Main_Window.Should_Close;
    end loop;
end Main_Loop;
