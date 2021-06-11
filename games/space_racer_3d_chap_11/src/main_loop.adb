
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows;
with Glfw.Windows.Context;

with GL.Buffers;
--  with GL.Toggles;
with GL.Types.Colors;
with GL.Window;

with Maths;
with Utilities;

with Input_Manager;
with Levels_Manager;
with Model;
with Shader_Manager_UI;
with Sprite_Manager;
with Text_Manager;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Input_Callback.Callback_Window) is

    Back             : constant GL.Types.Colors.Color :=
                         (0.6, 0.6, 0.6, 0.0);
    Ship_Colour      : constant GL.Types.Colors.Basic_Color := (0.0, 0.0, 1.0);
    --      Splash_Threshold : constant Float := 5.0;
    --      UI_Threshold     : constant float := 0.1;
    --      UI_Timer         : Float := 0.0;
    --      Splash_Timer     : Float := 0.0;
    Splash_Screen    : Sprite_Manager.Sprite;
    Menu_Screen      : Sprite_Manager.Sprite;
    Credits_Screen   : Sprite_Manager.Sprite;
    Game_Over_Screen : Sprite_Manager.Sprite;
    Ship             : Model.Model_Data;
    Asteriods        : array (1 .. 3) of Model.Model_Data;
    Last_Time        : Float := Float (Glfw.Time);
    Command_Done     : Boolean := False;
    Speed            : GL.Types.Single := 1.0;
    Max_Speed        : GL.Types.Single := 0.0;
    Mission_Time     : Float := 0.0;
    Score            : Integer := 0;
    Asteriods_Hit    : Integer := 0;

    procedure Draw_UI (Screen : in out Input_Callback.Callback_Window);
    procedure Initialize_2D;
    procedure Process_UI_Command (Screen : in out Input_Callback.Callback_Window);
    procedure Resize_GL_Scene (Screen : in out Input_Callback.Callback_Window);
    procedure Start_Game  (Screen : in out Input_Callback.Callback_Window);

    --  -------------------------------------------------------------------------

    procedure Check_Collisions is
        Item      : Model.Model_Data;
        Collision : Boolean := False;
    begin
        for index in Asteriods'Range loop
            Item := Asteriods (index);
            Collision := Model.Collided_With (Ship, Item);
            if Collision then
                Model.Set_Is_Collidable (Item, False);
                Score := Score + 1;
                Asteriods_Hit := Asteriods_Hit + 1;
            end if;
        end loop;
    end Check_Collisions;

    --  -------------------------------------------------------------------------

    procedure Disable_2D is
    begin
        null;
    end Disable_2D;

    --  -------------------------------------------------------------------------

    procedure Draw_Credits (Window : in out Input_Callback.Callback_Window) is
        use GL.Types;
        Screen_Width       : Glfw.Size;
        Screen_Height      : Glfw.Size;
        Height             : Single;
        Start_X            : Single;
        Start_Y            : Single;
        Space_Y            : constant Single := 30.0;
    begin
        Window.Get_Framebuffer_Size (Screen_Width, Screen_Height);
        Start_X := 0.4 * Single (Screen_Width);
        Height := Single (Screen_Height);
        Start_Y := Height - 300.0;
        Text_Manager.Draw_Text (Window, "Robert Marsden", Start_X, Start_Y,
                                0.0, 1.0, 0.0);
        Text_Manager.Draw_Text (Window, "Roger Mc Murtrie",
                                Start_X, Start_Y + Space_Y, 0.0, 1.0, 0.0);

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Main_Loop.Draw_Credits.");
            Put_Line (Exception_Information (anError));
            raise;
    end Draw_Credits;

    --  -------------------------------------------------------------------------------------------------

    procedure Draw_Stats (Window : in out Input_Callback.Callback_Window) is
        use GL.Types;
        Screen_Width       : Glfw.Size;
        Screen_Height      : Glfw.Size;
        Height             : Single;
        Start_X            : Single;
        Start_Y            : Single;
        Space_Y            : constant Single := 30.0;
        Asteroids_Hit_Text : constant String :=
                               "Asteroids Hit: " & Integer'Image (Score);
        Score_Text         : constant String :=
                               "Score: " & Integer'Image (Score);
        Max_Speed_Text     : constant String :=
                               "Speed: " & Single'Image (Speed);
        Mission_Time_Text  : constant String :=
                               "Mission_Time: " & Float'Image (Mission_Time);
    begin
        Window.Get_Framebuffer_Size (Screen_Width, Screen_Height);
        Start_X := 0.4 * Single (Screen_Width);
        Height := Single (Screen_Height);
        Start_Y := Height - 275.0;
        Text_Manager.Draw_Text (Window, Asteroids_Hit_Text, Start_X, Start_Y,
                                0.0, 1.0, 0.0);
        Text_Manager.Draw_Text (Window, Max_Speed_Text,
                                Start_X, Start_Y + Space_Y, 0.0, 1.0, 0.0);
        Text_Manager.Draw_Text (Window, Mission_Time_Text,
                                Start_X, Start_Y + 2.0 * Space_Y, 0.0, 1.0, 0.0);
        Text_Manager.Draw_Text (Window, Score_Text,
                                Start_X, Start_Y + 3.0 * Space_Y, 0.0, 1.0, 0.0);

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Main_Loop.Draw_Stats.");
            Put_Line (Exception_Information (anError));
            raise;
    end Draw_Stats;

    --  -------------------------------------------------------------------------------------------------

    procedure Draw_UI (Screen : in out Input_Callback.Callback_Window) is
        use GL.Types;
        use Text_Manager;
        Screen_Width      : Glfw.Size;
        Screen_Height     : Glfw.Size;
        Start_Y           : Single;
        X1                : constant Single := 50.0;
        X2                : Single;
        X3                : Single;
        Score_Text        : constant String :=
                              "Score: " & Integer'Image (Score);
        Speed_Text        : constant String :=
                              "Speed: " & Single'Image (Speed);
        Mission_Time_Text : constant String :=
                              "Mission_Time: " & Float'Image (Mission_Time);
    begin
        Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
        Start_Y := Single (Screen_Height) - 50.0;
        X2 := Single (Screen_Width) / 2.0 - 50.0;
        X3 := Single (Screen_Width) - 250.0;

        Draw_Text (Screen, Score_Text, X1, Start_Y, 0.0, 1.0, 0.0);
        Draw_Text (Screen, Speed_Text, X2, Start_Y, 0.0, 1.0, 0.0);
        Draw_Text (Screen, Mission_Time_Text, X3, Start_Y, 0.0, 1.0, 0.0);

    end Draw_UI;

    --  -------------------------------------------------------------------------

    procedure Enable_2D (Screen : in out Input_Callback.Callback_Window) is
        use GL.Types;
        use Shader_Manager_UI;
        Screen_Width      : Glfw.Size;
        Screen_Height     : Glfw.Size;
        Projection_Matrix : Singles.Matrix4;
    begin
        Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
        Use_2D_Program;
        Maths.Init_Orthographic_Transform
          (Top => Single (Screen_Height), Bottom => 0.0,
           Left => 0.0, Right => Single (Screen_Width),
           Z_Near => 0.0, Z_Far => 1.0, Transform => Projection_Matrix);
        Shader_Manager_UI.Set_Projection_Matrix (Projection_Matrix);
    end Enable_2D;

    --  -------------------------------------------------------------------------

    procedure Initialize_2D is
    begin
        Shader_Manager_UI.Init_Shaders;
        Sprite_Manager.Init;
    end Initialize_2D;

    --  ------------------------------------------------------------------------

    procedure Load_Splash (Screen : in out Input_Callback.Callback_Window) is
        use Levels_Manager;
        use Sprite_Manager;
        Screen_Width  : Glfw.Size;
        Screen_Height : Glfw.Size;
    begin
        Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
        Set_Game_State (Game_Splash);
        Set_Frame_Size (Splash_Screen, Float (Screen_Width),
                        Float (Screen_Height));
        Set_Number_Of_Frames (Splash_Screen, 1);
        Set_Active (Splash_Screen, True);
        Set_Visible (Splash_Screen, True);
        Add_Texture (Splash_Screen, "src/resources/splash.png", False);
    end Load_Splash;

    ----------------------------------------------------------------------------
    --  LoadTextures
    procedure Load_Sprites (Screen : in out Input_Callback.Callback_Window) is
        use GL.Types;
        use Sprite_Manager;
        Screen_Width    : Glfw.Size;
        Screen_Height   : Glfw.Size;
        VP_Width        : Float;
        VP_Height       : Float;
    begin
        Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
        VP_Width := Float (Screen_Width);
        VP_Height := Float (Screen_Height);
        GL.Window.Set_Viewport (0, 0, Int (Screen_Width), Int (Screen_Height));

        Input_Manager.Load_Buttons (Screen);

        Set_Frame_Size (Menu_Screen, VP_Width, VP_Height);
        Set_Number_Of_Frames (Menu_Screen, 1);
        Set_Active (Menu_Screen, True);
        Set_Visible (Menu_Screen, True);
        Set_Position (Menu_Screen, 0.0, 0.0);
        Add_Texture (Menu_Screen, "src/resources/main_menu.png", False);

        Set_Frame_Size (Credits_Screen, VP_Width, VP_Height);
        Set_Number_Of_Frames (Credits_Screen, 1);
        Set_Active (Menu_Screen, True);
        Set_Visible (Credits_Screen, True);
        Add_Texture (Credits_Screen, "src/resources/credits.png", False);

        Set_Frame_Size (Game_Over_Screen, VP_Width, VP_Height);
        Set_Number_Of_Frames (Game_Over_Screen, 1);
        Set_Active (Game_Over_Screen, True);
        Set_Visible (Game_Over_Screen, True);
        Add_Texture (Game_Over_Screen, "src/resources/gameover.png", False);

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Main_Loop.Load_Sprites.");
            Put_Line (Exception_Information (anError));
            raise;
    end Load_Sprites;

    --  -------------------------------------------------------------------------

    procedure Process_Input_Command (Screen : in out Input_Callback.Callback_Window) is
        use GL.Types;
        use Input_Manager;
        use Model;
        Rotation : Singles.Vector3;
        aCommand : constant Command := Get_Current_Command;
    begin
        Input_Manager.Update_Command (Screen);
        case aCommand is
            when Command_Stop =>
                if not Command_Done then
                    if Velocity (Ship) > 0.0 then
                        Set_Velocity (Ship, 0.0);
                    else  --  Start
                        Set_Velocity (Ship, 0.1);
                    end if;
                    Command_Done := True;
                end if;

            when Command_Down =>
                Rotation := Heading_Rotation (Ship);
                Rotation (GL.X) := Rotation (GL.X) - 1.0;
                if Rotation (GL.X) < 0.0 then
                    Rotation (GL.X) := 359.0;
                end if;
                if Rotation (GL.X) > 180.0 and Rotation (GL.X) < 359.0 then
                    if Rotation (GL.X) < 315.0 then
                        Rotation (GL.X) := 315.0;
                    end if;
                end if;
                Set_Heading_Rotation (Ship, Rotation);

            when Command_Up =>
                Rotation := Heading_Rotation (Ship);
                Rotation (GL.X) := Rotation (GL.X) + 1.0;
                if Rotation (GL.X) > 359.0 then
                    Rotation (GL.X) := 0.0;
                end if;
                if Rotation (GL.X) > 0.0 and Rotation (GL.X) < 180.0 then
                    if Rotation (GL.X) > 45.0 then
                        Rotation (GL.X) := 45.0;
                    end if;
                end if;
                Set_Heading_Rotation (Ship, Rotation);

            when Command_Left =>
                Rotation := Heading_Rotation (Ship);
                Rotation (GL.Z) := Rotation (GL.Z) + 1.0;
                if Rotation (GL.Z) > 359.0 then
                    Rotation (GL.Z) := 0.0;
                end if;
                if Rotation (GL.Z) > 0.0 and Rotation (GL.Z) < 180.0 then
                    if Rotation (GL.Z) > 45.0 then
                        Rotation (GL.Z) := 45.0;
                    end if;
                end if;
                Set_Heading_Rotation (Ship, Rotation);

            when Command_Right =>
                Rotation := Heading_Rotation (Ship);
                Rotation (GL.Z) := Rotation (GL.Z) - 1.0;
                if Rotation (GL.Z) < 0.0 then
                    Rotation (GL.Z) := 359.0;
                end if;
                if Rotation (GL.Z) > 180.0 and Rotation (GL.Z) < 359.0 then
                    if Rotation (GL.Z) < 315.0 then
                        Rotation (GL.Z) := 315.0;
                    end if;
                end if;
                Set_Heading_Rotation (Ship, Rotation);
            when Command_UI => Process_UI_Command (Screen);
            when others =>
                Command_Done := False;
        end case;

    end Process_Input_Command;

    --  -------------------------------------------------------------------------

    procedure Process_UI_Command (Screen : in out Input_Callback.Callback_Window) is
        use Input_Manager;
        use Levels_Manager;
    begin
        if Is_Clicked (Play_Button) then
            Set_Clicked (Play_Button, False);
            Set_Active (Play_Button, False);
            Set_Clicked (Exit_Button, False);
            Set_Active (Credits_Button, False);
            Set_Game_State (Game_Running);

        elsif Is_Clicked (Credits_Button) then
            Set_Clicked (Credits_Button, False);
            Set_Active (Credits_Button, False);
            Set_Clicked (Play_Button, False);
            Set_Active (Exit_Button, False);
            Set_Game_State (Game_Credits);

        elsif Is_Clicked (Menu_Button) then
            Set_Clicked (Menu_Button, False);
            Set_Active (Menu_Button, False);
            Set_Active (Play_Button, True);
            Set_Active (Exit_Button, True);
            Set_Game_State (Game_Credits);
            case Get_Game_State is
            when Game_Credits => Set_Game_State (Game_Menu);
            when Game_Over => Start_Game (Screen);
            when others => null;
            end case;

        elsif Is_Clicked (Exit_Button) then
            Set_Clicked (Exit_Button, False);
            Set_Active (Exit_Button, False);
            Set_Clicked (Play_Button, False);
            Set_Active (Play_Button, True);
            Set_Active (Credits_Button, True);
            Set_Game_State (Game_Quit);
        end if;

    end Process_UI_Command;

    --  -------------------------------------------------------------------------

    procedure Render_2D (Screen : in out Input_Callback.Callback_Window) is
        use Input_Manager;
        use Levels_Manager;
    begin
        Enable_2D (Screen);
        Put_Line ("Main_Loop.Render_2D Game_State: " &
                    Game_Status'Image (Get_Game_State));
        case Get_Game_State is
        when Game_Loading =>
            Sprite_Manager.Render (Splash_Screen);
        when Game_Menu =>
            Sprite_Manager.Render (Menu_Screen);
            Render_Button (Play_Button);
            Render_Button (Credits_Button);
            Render_Button (Exit_Button);
        when Game_Credits =>
            Sprite_Manager.Render (Credits_Screen);
            Render_Button (Menu_Button);
            Draw_Credits (Screen);
        when Game_Running => Draw_UI (Screen);
        when Game_Splash =>
            Put_Line ("Main_Loop.Render_2D Splash_Screen");
            Sprite_Manager.Render (Splash_Screen);
        when Game_Over =>
            Sprite_Manager.Render (Game_Over_Screen);
            Draw_Stats  (Screen);
        when others => null;
        end case;
        Disable_2D;
    end Render_2D;

    --  ------------------------------------------------------------------------

    procedure Render_3D is
        use Levels_Manager;
    begin
        if Get_Game_State = Game_Running then
            for index in Asteriods'Range loop
                Model.Render (Asteriods (index));
            end loop;
            Model.Render (Ship);
        end if;
    end Render_3D;

    --  ------------------------------------------------------------------------

    procedure Render (Screen : in out Input_Callback.Callback_Window) is
    begin
        Utilities.Clear_Colour_Buffer_And_Depth;
        Resize_GL_Scene (Screen);
        Render_3D;
        Render_2D (Screen);
    end Render;

    --  ------------------------------------------------------------------------

    procedure Resize_GL_Scene (Screen : in out Input_Callback.Callback_Window) is
        use GL.Types;
        Screen_Width      : Glfw.Size;
        Screen_Height     : Glfw.Size;
        Projection_Matrix : Singles.Matrix4 := Singles.Identity4;
    begin
        Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
        if Integer (Screen_Height) = 0 then
            Screen_Height := 1;
        end if;
        GL.Window.Set_Viewport (0, 0, GL.Types.Size (Screen_Width),
                                GL.Types.Size (Screen_Height));

        Maths.Init_Perspective_Transform
          (Maths.Degree (45.0), Single (Screen_Width), Single (Screen_Height),
           0.1, 100.0, Projection_Matrix);
        Model.Set_Perspective (Projection_Matrix);

    end Resize_GL_Scene;

    --  ------------------------------------------------------------------------

    procedure Start_Game  (Screen : in out Input_Callback.Callback_Window) is
        use GL.Types;
    begin
        Utilities.Clear_Background_Colour_And_Depth (Back);
        GL.Buffers.Set_Depth_Function (LEqual);
        Input_Callback.Clear_All_Keys;
        --          GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);

        Text_Manager.Initialize (Screen);
        Initialize_2D;

        Model.Initialize_3D (Ship, "src/resources/ship.obj", Ship_Colour);
        Model.Set_Is_Ship (Ship, True);
        Model.Set_Position (Ship, (0.0, -0.5, -4.0));
        Model.Set_Base_Rotation (Ship, (90.0, 0.0, 0.0));
        Model.Set_Velocity (Ship, 0.1);

        Model.Initialize_3D (Asteriods (1), "src/resources/tri_asteroid.obj", (1.0, 0.0, 0.0));
        Model.Set_Position (Asteriods (1), (0.0, 0.0, -10.0));

        Model.Initialize_3D (Asteriods (2), "src/resources/tri_asteroid.obj", (0.0, 1.0, 0.0));
        Model.Set_Position (Asteriods (2), (5.0, 0.0, -15.0));

        Model.Initialize_3D (Asteriods (3), "src/resources/tri_asteroid.obj", (0.0, 1.0, 1.0));
        Model.Set_Position (Asteriods (3), (5.0, 5.0, -20.0));

        Load_Splash (Screen);
        Load_Sprites (Screen);

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Start_Game.");
            Put_Line (Exception_Information (anError));
            raise;
    end Start_Game;

    --  -------------------------------------------------------------------------

    procedure Update (Window : in out Input_Callback.Callback_Window;
                      Running : in out Boolean) is
        use GL.Types;
        use Input_Manager;
        use Levels_Manager;
        use Model;
        Current_Time : constant Float := Float (Glfw.Time);
        Delta_Time   : constant Float := Current_Time - Last_Time;
    begin
        Last_Time := Current_Time;

        case Get_Game_State is
            when Game_Splash | Game_Loading =>
                Sprite_Manager.Update (Splash_Screen, Delta_Time);

            when Game_Running =>
                Input_Manager.Update_Command (Window);
                Process_Input_Command (Window);
                Update (Ship, Delta_Time);
                Set_Velocity (Ship, Model.Velocity (Ship) *
                                Single (1.0 + Delta_Time / 10.0));
                Speed := 1000.0 * Model.Velocity (Ship);
                if Max_Speed < Speed then
                    Max_Speed := Speed;
                end if;
                Mission_Time := Mission_Time + 100.0 * Delta_Time;
                Check_Collisions;
                if Position (Ship) (GL.Z) > 10.0 then
                    Set_Game_State (Game_Over);
                    Input_Manager.Set_Active (Menu_Button, True);
                end if;

            when Game_Over =>
                Sprite_Manager.Update (Game_Over_Screen, Delta_Time);
                Set_Active (Replay_Button, True);
                Update_Button (Replay_Button, Delta_Time);
                Set_Active (Exit_Button, True);
                Update_Button (Exit_Button, Delta_Time);
                Update (Delta_Time);
                Process_Input_Command (Window);
                Update (Ship, Delta_Time);
                Check_Collisions;

            when Game_Quit =>
                Running := False;
            when others => null;
        end case;

        if Running then
            Check_Collisions;
            Model.Update (Ship, Delta_Time);
            for index in Asteriods'Range loop
                Model.Update (Asteriods (index), Delta_Time);
            end loop;
        end if;
    end Update;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running  : Boolean := True;
begin
    Start_Game (Main_Window);
    while Running loop
        Update (Main_Window, Running);
        if Running then
            Render (Main_Window);
        end if;
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Running := Running and not
          (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and not Main_Window.Should_Close;
    end loop;

exception
    when anError : others =>
        Put_Line ("An exception occurred in Main_Loop.");
        Put_Line (Exception_Information (anError));
        raise;

end Main_Loop;
