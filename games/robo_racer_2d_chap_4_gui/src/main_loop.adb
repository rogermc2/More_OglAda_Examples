
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows;
with Glfw.Windows.Context;

with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Maths;
with Program_Loader;
with Utilities;

with Input_Manager;
with Player_Manager;
with Sprite_Manager;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Input_Callback.Callback_Window) is
    type Game_Status is (Game_Running, Game_Paused);

    Back               : constant GL.Types.Colors.Color := (0.6, 0.6, 0.6, 1.0);
    UI_Threshold       : constant float := 0.2;
    Last_Time          : Float := 0.0;
    Game_Program       : GL.Objects.Programs.Program;
    Model_Uniform      : GL.Uniforms.Uniform;
    Projection_Uniform : GL.Uniforms.Uniform;
    Texture_Uniform    : GL.Uniforms.Uniform;
    Background         : Sprite_Manager.Sprite;
    Pause_Button       : Sprite_Manager.Sprite;
    Resume_Button      : Sprite_Manager.Sprite;
    Game_State         : Game_Status := Game_Running;
    UI_Timer           : Float := 0.0;

    procedure Resize_GL_Scene  (Screen : in out Input_Callback.Callback_Window);

    --  ------------------------------------------------------------------------

    --     procedure Enable_Mouse_Callbacks (Window : in out Input_Callback.Callback_Window;
    --                                       Enable : Boolean) is
    --        use Glfw.Windows.Callbacks;
    --     begin
    --        if Enable then
    --           Window.Enable_Callback (Mouse_Position);
    --           Window.Enable_Callback (Mouse_Enter);
    --           Window.Enable_Callback (Mouse_Button);
    --           Window.Enable_Callback (Mouse_Scroll);
    --        else
    --           null;
    --           Window.Disable_Callback (Mouse_Position);
    --           Window.Disable_Callback (Mouse_Enter);
    --           Window.Disable_Callback (Mouse_Button);
    --           Window.Disable_Callback (Mouse_Scroll);
    --        end if;
    --     end Enable_Mouse_Callbacks;

    ----------------------------------------------------------------------------
    --  LoadTextures
    procedure Load_Sprites (Screen : in out Input_Callback.Callback_Window) is
        use GL.Types;
        use Sprite_Manager;
        Screen_Width    : Glfw.Size;
        Screen_Height   : Glfw.Size;
        Border_Width    : constant Size := 10;
        VP_Width        : Size;
        VP_Height       : Size;
    begin
        Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
        VP_Width := Size (Screen_Width) - 2 * Border_Width;
        VP_Height := Size (Screen_Height) - 2 * Border_Width;
        GL.Window.Set_Viewport (Border_Width, Border_Width, VP_Width, VP_Height);

        Set_Frame_Size (Background, 1877.0, 600.0);
        Set_Number_Of_Frames (Background, 1);
        Add_Texture (Background, "src/resources/background.png", False);

        Player_Manager.Init_Players;

        Set_Visible (Background, True);
        Set_Active (Background, True);

        Set_Frame_Size (Pause_Button, 75.0, 38.0);
        Set_Number_Of_Frames (Pause_Button, 1);
        Set_Position (Pause_Button, 10.0, 5.0);
        Add_Texture (Pause_Button, "src/resources/pauseButton.png", False);
        Set_Visible (Pause_Button, True);
        Set_Active (Pause_Button, True);
        Input_Manager.Add_UI_Element (Pause_Button);

        Set_Frame_Size (Resume_Button, 75.0, 38.0);
        Set_Number_Of_Frames (Resume_Button, 1);
        Set_Position (Resume_Button, 80.0, 10.0);
        Add_Texture (Resume_Button, "src/resources/resumeButton.png", False);
        Input_Manager.Add_UI_Element (Resume_Button);

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Load_Sprites.");
            Put_Line (Exception_Information (anError));
            raise;
    end Load_Sprites;

    --  -------------------------------------------------------------------------

    procedure Process_Input (Delta_Time : Float) is
        use Input_Manager;
        use Player_Manager;
        use Sprite_Manager;
        Step      : constant Float := 100.0;
        Player    : constant Player_Index := Get_Current_Player;
        aCommand  : Command := Get_Command;
    begin
        if Game_State = Game_Paused then
            aCommand := Command_UI;
        end if;
        UI_Timer := UI_Timer + Delta_Time;
        if UI_Timer > UI_Threshold then
            UI_Timer := 0.0;
        end if;

        case aCommand is
            when Command_UI =>
                if Is_Clicked (Pause_Button) then
                    Set_Clicked (Pause_Button, False);
                    Set_Visible (Pause_Button, False);
                    Set_Active (Pause_Button, False);

                    Set_Visible (Resume_Button, True);
                    Set_Active (Resume_Button, True);
                    Game_State := Game_Paused;
                elsif Is_Clicked (Resume_Button) then
                    Set_Clicked (Resume_Button, False);
                    Set_Visible (Resume_Button, False);
                    Set_Active (Resume_Button, False);

                    Set_Visible (Pause_Button, True);
                    Set_Active (Pause_Button, True);
                    Game_State := Game_Running;
                end if;
            when Command_Left =>
                if Player = Robot_Right then
                    Set_Active (Robot_Right, False);
                    Set_Visible (Robot_Right, False);
                    Set_Position (Robot_Left, Get_Position (Robot_Right));
                end if;
                Set_Current_Player (Robot_Left);
                Set_Active (Robot_Left, True);
                Set_Visible (Robot_Left, True);
                Set_Velocity (Robot_Left, -Step);
                Set_Velocity (Background, Step);
            when Command_Right =>
                if Player = Robot_Left then
                    Set_Active (Robot_Left, False);
                    Set_Visible (Robot_Left, False);
                    Set_Position (Robot_Right, Get_Position (Robot_Left));
                end if;
                Set_Current_Player (Robot_Right);
                Set_Active (Robot_Right, True);
                Set_Visible (Robot_Right, True);
                Set_Velocity (Robot_Right, Step);
                Set_Velocity (Background, -Step);
            when Command_Stop =>
                Set_Velocity (Background, 0.0);
                Set_Velocity (Player, 0.0);
            when Command_Up => Jump (Player, Sprite_Up);
            when Command_Down => Jump (Player, Sprite_Down);
            when Command_Invalid => null;
        end case;
        Set_Command_Invalid;
    end Process_Input;

    --  -------------------------------------------------------------------------

    procedure Render_Sprites (Screen : in out Input_Callback.Callback_Window) is
    begin

        Utilities.Clear_Colour;
        Sprite_Manager.Clear_Buffers;
        Resize_GL_Scene (Screen);
        GL.Objects.Programs.Use_Program (Game_Program);
        GL.Uniforms.Set_Single (Model_Uniform, GL.Types.Singles.Identity4);
        GL.Uniforms.Set_Int (Texture_Uniform, 0);

        Sprite_Manager.Render (Background);
        Player_Manager.Render_Players;
        Sprite_Manager.Render (Pause_Button);
        Sprite_Manager.Render (Resume_Button);
    end Render_Sprites;

    --  ----------------------------------------------------------------------------

    procedure Resize_GL_Scene (Screen : in out Input_Callback.Callback_Window) is
        use GL.Objects.Programs;
        use GL.Types;
        Screen_Width      : Glfw.Size;
        Screen_Height     : Glfw.Size;
        VP_Width          : Size;
        VP_Height         : Size;
        Border_Width      : constant Size := 2;
        Projection_Matrix : Singles.Matrix4 := Singles.Identity4;
    begin
        Screen.Get_Framebuffer_Size (Screen_Width, Screen_Height);
        VP_Width := Size (Screen_Width) - 2 * Border_Width;
        VP_Height := Size (Screen_Height) - 2 * Border_Width;
        GL.Window.Set_Viewport (Border_Width, Border_Width, VP_Width, VP_Height);

        Maths.Init_Orthographic_Transform
          (Single (VP_Height), 0.0, 0.0, Single (VP_Width), 0.0, 1.0,
           Projection_Matrix);

        Use_Program (Game_Program);
        GL.Uniforms.Set_Single (Projection_Uniform, Projection_Matrix);
    end Resize_GL_Scene;

    --  ----------------------------------------------------------------------------

    procedure Start_Game (Screen : in out Input_Callback.Callback_Window) is
        use Program_Loader;
        use GL.Objects.Shaders;
        use GL.Types;
        use Glfw.Input;
        Window_Width       : Glfw.Size;
        Window_Height      : Glfw.Size;
    begin
--          Screen.Set_Input_Toggle (Sticky_Keys, True);
        Screen.Set_Cursor_Mode (Mouse.Normal);
        Screen'Access.Get_Size (Window_Width, Window_Height);
        Screen'Access.Set_Cursor_Pos (Mouse.Coordinate (0.5 * Single (Window_Width)),
                                      Mouse.Coordinate (0.5 * Single (Window_Height)));

        Utilities.Clear_Background_Colour (Back);
        Input_Callback.Clear_All_Keys;
        Load_Sprites (Screen);

        Game_Program := Program_From
          ((Src ("src/shaders/robo2d_vertext_gui_shader.glsl", Vertex_Shader),
           Src ("src/shaders/robo2d_fragment_gui_shader.glsl", Fragment_Shader)));
        GL.Objects.Programs.Use_Program (Game_Program);

        Model_Uniform :=
          GL.Objects.Programs.Uniform_Location (Game_Program, "model_matrix");
        Projection_Uniform :=
          GL.Objects.Programs.Uniform_Location (Game_Program, "projection_matrix");
        Texture_Uniform :=
          GL.Objects.Programs.Uniform_Location (Game_Program, "texture2d");

        Sprite_Manager.Init;
        --          Enable_Mouse_Callbacks (Screen, True);
        --          Screen.Enable_Callback (Glfw.Windows.Callbacks.Char);
        --          Screen.Enable_Callback (Glfw.Windows.Callbacks.Position);

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Start_Game.");
            Put_Line (Exception_Information (anError));
            raise;
    end Start_Game;

    --  -------------------------------------------------------------------------

    procedure Update (Window : in out Input_Callback.Callback_Window) is
        Current_Time : constant Float := Float (Glfw.Time);
        Delta_Time   : constant Float := Current_Time - Last_Time;
        X_Position   : Glfw.Input.Mouse.Coordinate := 0.00001;
        Y_Position   : Glfw.Input.Mouse.Coordinate := 0.00002;
    begin
        Last_Time := Current_Time;

        Render_Sprites (Window);
        Input_Manager.Update_Command (Window);
        Process_Input (Delta_Time);

        Window'Access.Get_Cursor_Pos (X_Position, Y_Position);
        if Game_State = Game_Running then
            Sprite_Manager.Update (Background, Delta_Time);
            Player_Manager.Update (Delta_Time);
            Sprite_Manager.Update (Pause_Button, Delta_Time);
            Sprite_Manager.Update (Resume_Button, Delta_Time);
        end if;
    end Update;

    --  ----------------------------------------------------------------------------

    use Glfw.Input;
    Running  : Boolean := True;
begin
    Start_Game (Main_Window);
    while Running loop
        Update (Main_Window);
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Running := Running and not
          (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and not Main_Window.Should_Close;
    end loop;

exception
    when Program_Loader.Shader_Loading_Error =>
        -- message was already written to stdout
        null;
    when anError : others =>
        Put_Line ("An exception occurred in Main_Loop.");
        Put_Line (Exception_Information (anError));
        raise;
end Main_Loop;
