
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows;
with Glfw.Windows.Context;

with GL.Buffers;
with GL.Types.Colors;
with GL.Window;

with Maths;
with Utilities;

with Input_Manager;
with Model;
with Sprite_Manager;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Input_Callback.Callback_Window) is

    Back          : constant GL.Types.Colors.Color :=
                      (0.6, 0.6, 0.6, 0.0);
    GUI_Threshold : constant float := 0.1;
    Ship          : Model.Model_Data;
    Ship_Colour   : constant GL.Types.Colors.Basic_Color := (0.0, 0.0, 1.0);
    Asteriods     : array (1 .. 3) of Model.Model_Data;
    Last_Time     : Float := Float (Glfw.Time);
    GUI_Timer     : Float := 0.0;

    procedure Resize_GL_Scene  (Screen : in out Input_Callback.Callback_Window);

    --  ------------------------------------------------------------------------

    procedure Enable_Mouse_Callbacks
      (Window : in out Input_Callback.Callback_Window;
       Enable : Boolean) is
        use Glfw.Windows.Callbacks;
    begin
        if Enable then
            Window.Enable_Callback (Mouse_Position);
            Window.Enable_Callback (Mouse_Enter);
            Window.Enable_Callback (Mouse_Button);
            Window.Enable_Callback (Mouse_Scroll);
        else
            Window.Disable_Callback (Mouse_Position);
            Window.Disable_Callback (Mouse_Enter);
            Window.Disable_Callback (Mouse_Button);
            Window.Disable_Callback (Mouse_Scroll);
        end if;
    end Enable_Mouse_Callbacks;

   --  ------------------------------------------------------------------------

    procedure Process_Input_Command (Delta_Time : Float) is
        use GL.Types;
        use Input_Manager;
        use Model;
        Rotation : Singles.Vector3 := Heading (Ship);
        aCommand : constant Command := Get_Current_Command;
        --          Continue : Boolean := True;
    begin
        GUI_Timer := GUI_Timer + Delta_Time;
        if GUI_Timer > GUI_Threshold then
            GUI_Timer := 0.0;
        end if;

--          Put_Line ("Main_Loop.Process_Input_Command, Command" &
--                   Command'Image (aCommand));
        case aCommand is
            when Command_Stop =>
                if Velocity (Ship) > 0.0 then
                    Set_Velocity (Ship, 0.0);
                else
                    Set_Velocity (Ship, 0.1);
                end if;

            when Command_Down =>
                Put_Line ("Main_Loop.Process_Input_Command, Command_Down");
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
                Rotation (GL.X) := Rotation (GL.Z) + 1.0;
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
                Rotation (GL.X) := Rotation (GL.Z) - 1.0;
                if Rotation (GL.Z) < 0.0 then
                    Rotation (GL.Z) := 359.0;
                end if;
                if Rotation (GL.Z) > 180.0 and Rotation (GL.Z) < 359.0 then
                    if Rotation (GL.Z) < 315.0 then
                        Rotation (GL.Z) := 315.0;
                    end if;
                end if;
                Set_Heading_Rotation (Ship, Rotation);

            when others => null;
        end case;
        --          Set_Command_Invalid;

    end Process_Input_Command;

    --  -------------------------------------------------------------------------

    procedure Render (Screen : in out Input_Callback.Callback_Window) is
    begin
        Utilities.Clear_Colour_Buffer_And_Depth;
        Resize_GL_Scene (Screen);
        for index in Asteriods'Range loop
            Model.Render (Asteriods (index));
        end loop;
        Model.Render (Ship);
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

    procedure Start_Game (Screen : in out Input_Callback.Callback_Window) is
        use GL.Types;
        use Glfw.Input;
        Window_Width  : Glfw.Size;
        Window_Height : Glfw.Size;
    begin
        Screen.Set_Cursor_Mode (Mouse.Normal);
        Screen'Access.Get_Size (Window_Width, Window_Height);
        Screen'Access.Set_Cursor_Pos
          (Mouse.Coordinate (0.5 * Single (Window_Width)),
           Mouse.Coordinate (0.5 * Single (Window_Height)));

        Utilities.Clear_Background_Colour_And_Depth (Back);
        GL.Buffers.Set_Depth_Function (LEqual);
        Input_Callback.Clear_All_Keys;

        Model.Initialize (Ship, "src/resources/ship.obj", Ship_Colour);
        Model.Set_Is_Ship (Ship, True);
        Model.Set_Position (Ship, (0.0, 0.0, -4.0));
        Model.Set_Base_Rotation (Ship, (90.0, 0.0, 0.0));
        Model.Set_Velocity (Ship, 0.1);

        Model.Initialize (Asteriods (1), "src/resources/tri_asteroid.obj", (1.0, 0.0, 0.0));
        Model.Set_Position (Asteriods (1), (0.0, 0.0, -10.0));

        Model.Initialize (Asteriods (2), "src/resources/tri_asteroid.obj", (0.0, 1.0, 0.0));
        Model.Set_Position (Asteriods (2), (5.0, 0.0, -15.0));

        Model.Initialize (Asteriods (3), "src/resources/tri_asteroid.obj", (0.0, 1.0, 1.0));
        Model.Set_Position (Asteriods (3), (5.0, 5.0, -20.0));

        Sprite_Manager.Init;
        Enable_Mouse_Callbacks (Screen, True);
--          Screen.Enable_Callback (Glfw.Windows.Callbacks.Key);
--          Screen.Enable_Callback (Glfw.Windows.Callbacks.Char);

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
    begin
        Last_Time := Current_Time;
        Input_Callback.Clear_All_Keys;
        Input_Manager.Update_Command (Window);
        Process_Input_Command (Delta_Time);
        Model.Update (Ship, Delta_Time);
        for index in Asteriods'Range loop
            Model.Update (Asteriods (index), Delta_Time);
        end loop;
    end Update;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running  : Boolean := True;
begin
    Start_Game (Main_Window);
    while Running loop
        Update (Main_Window);
        Render (Main_Window);
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
