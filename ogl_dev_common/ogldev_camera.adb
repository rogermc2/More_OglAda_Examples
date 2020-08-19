
with Interfaces.C;

with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input;
with Glfw.Input.Keys;

package body Ogldev_Camera is
    Step_Scale       : GL.Types.Single := 0.004;
    Edge_Step        : constant Maths.Degree := 0.01;
    Margin           : constant Glfw.Input.Mouse.Coordinate := 10.0;

    procedure Update (theCamera : in out Camera);
    procedure Update_Render (theCamera : in out Camera);

    --  -------------------------------------------------------------------------

    function Get_Position (theCamera : Camera) return Singles.Vector3 is
    begin
        return theCamera.Position;
    end Get_Position;

    --  -------------------------------------------------------------------------

    function Get_Target (theCamera : Camera) return Singles.Vector3 is
    begin
        return theCamera.Target;
    end Get_Target;

    --  -------------------------------------------------------------------------

    function Get_Up (theCamera : Camera) return Singles.Vector3 is
    begin
        return theCamera.Up;
    end Get_Up;

    --  -------------------------------------------------------------------------

    procedure Initialize_Camera (theCamera : in out Camera) is
        use Maths;
        use Maths.Single_Math_Functions;

        function To_Degree (Angle : Single) return Degree is
        begin
            return Degrees (Radian (Angle));
        end To_Degree;

        H_Target : Singles.Vector3 :=
                     (theCamera.Target (GL.X), 0.0, theCamera.Target (GL.Z));
    begin
        if Length (H_Target) /= 0.0 then
            H_Target := Normalized (H_Target);
        end if;

        if theCamera.Target (GL.Z)  >= 0.0 then
            if theCamera.Target (GL.X)  >= 0.0 then
                theCamera.Angle_H := 360.0 - To_Degree (Arcsin (H_Target (GL.Z)));
            else
                theCamera.Angle_H := 180.0 + To_Degree (Arcsin (H_Target (GL.Z)));
            end if;
        else
            if theCamera.Target (GL.X)  >= 0.0 then
                theCamera.Angle_H := To_Degree (Arcsin (-H_Target (GL.Z)));
            else
                theCamera.Angle_H := 180.0 - To_Degree (Arcsin (-H_Target (GL.Z)));
            end if;
        end if;

        theCamera.Angle_V := -To_Degree (Arcsin (theCamera.Target (GL.Y)));

        theCamera.On_Upper_Edge := False;
        theCamera.On_Lower_Edge := False;
        theCamera.On_Left_Edge := False;
        theCamera.On_Right_Edge := False;

        theCamera.Mouse_X := Glfw.Input.Mouse.Coordinate (theCamera.Window_Width / 2);
        theCamera.Mouse_Y := Glfw.Input.Mouse.Coordinate (theCamera.Window_Height / 2);

    exception
        when  others =>
            Put_Line ("An exception occurred in Ogldev_Camera.Initialize_Camera.");
            raise;
    end Initialize_Camera;

    --  -------------------------------------------------------------------------

    --  From camera.cpp Camera::Camera
    procedure Init_Camera (theCamera : in out Camera;
                           Window    : in out Glfw.Windows.Window) is
        Window_Width     : Glfw.Size;
        Window_Height    : Glfw.Size;
    begin
        Window.Get_Framebuffer_Size (Window_Width, Window_Height);
        theCamera.Window_Width := GL.Types.Int (Window_Width);
        theCamera.Window_Height := GL.Types.Int (Window_Height);
        Initialize_Camera (theCamera);
        Window'Access.Set_Cursor_Pos (theCamera.Mouse_X, theCamera.Mouse_Y);

    exception
        when  others =>
            Put_Line ("An exception occurred in Ogldev_Camera.Init_Camera 1.");
            raise;
    end Init_Camera;

    --  -------------------------------------------------------------------------

    --  From camera.cpp Camera::Camera
    procedure Init_Camera (theCamera                   : in out Camera;
                           Window    : in out Glfw.Windows.Window;
                           Camera_Position, Target_Position, Up : Singles.Vector3) is
        Window_Width     : Glfw.Size;
        Window_Height    : Glfw.Size;
    begin
        Window.Get_Framebuffer_Size (Window_Width, Window_Height);
        theCamera.Window_Width := GL.Types.Int (Window_Width);
        theCamera.Window_Height := GL.Types.Int (Window_Height);
        theCamera.Position := Camera_Position;

        if Maths.Length (Target_Position) /= 0.0 then
            theCamera.Target := Maths.Normalized (Target_Position);
        else
            raise Camera_Exception with
              "Ogldev_Camera.Init_Camera, zero length Target vector detected.";
        end if;

        if Maths.Length (Up) /= 0.0 then
            theCamera.Up := Maths.Normalized (Up);
        else
            Put_Line ("Ogldev_Camera.Init_Camera, zero length Up vector detected.");
            Put_Line ("Settin Up vector to (0.0, 1.0, 0.0).");
            theCamera.Up := (0.0, 1.0, 0.0);
        end if;
        Initialize_Camera (theCamera);
        Window'Access.Set_Cursor_Pos (theCamera.Mouse_X, theCamera.Mouse_Y);

    exception
        when  others =>
            Put_Line ("An exception occurred in Ogldev_Camera.Init_Camera 2.");
            raise;
    end Init_Camera;

    --  -------------------------------------------------------------------------

    procedure Process_Keyboard (theCamera : in out Camera;
                                Window    : in out Glfw.Windows.Window) is
        use Glfw.Input;
        use GL.Types.Singles;  --  Needed to support arithmetic operations.
        use Maths;
        Vec : Vector3 := (0.0, 0.0, 0.0);
    begin
        if Window'Access.Key_State (Keys.Up) = Pressed then
            theCamera.Position := theCamera.Position + theCamera.Target * Step_Scale;
        elsif Window'Access.Key_State (Keys.Down) = Pressed then
            theCamera.Position := theCamera.Position - theCamera.Target * Step_Scale;
        elsif Window'Access.Key_State (Keys.Left) = Pressed then
            Vec := Normalized (Cross_Product (theCamera.Target, theCamera.Up)) * Step_Scale;
            theCamera.Position := theCamera.Position + Vec;
        elsif Window'Access.Key_State (Keys.Right) = Pressed then
            Vec := Normalized (Cross_Product (theCamera.Up, theCamera.Target)) * Step_Scale;
            theCamera.Position := theCamera.Position + Vec;
        elsif Window'Access.Key_State (Keys.Page_Up) = Pressed then
            theCamera.Position := theCamera.Position + (0.0, 0.5 * Step_Scale, 0.0);
        elsif Window'Access.Key_State (Keys.Page_Down) = Pressed then
            theCamera.Position := theCamera.Position - (0.0, 0.5 * Step_Scale, 0.0);
        end if;

        Update (theCamera);
    end Process_Keyboard;

    --  -------------------------------------------------------------------------

    procedure Process_Mouse (theCamera : in out Camera;
                             Window    : in out Glfw.Windows.Window) is
        use Interfaces.C;
        use Glfw.Input.Mouse;
        use Maths;
        Cursor_X   : Coordinate;
        Cursor_Y   : Coordinate;
        Delta_X    : Coordinate;
        Delta_Y    : Coordinate;
    begin
        Window'Access.Get_Cursor_Pos (Cursor_X, Cursor_Y);
        Delta_X := Cursor_X - theCamera.Mouse_X;
        Delta_Y := Cursor_Y - theCamera.Mouse_Y;

        theCamera.Mouse_X := Cursor_X;
        theCamera.Mouse_Y := Cursor_Y;

        theCamera.On_Left_Edge := False;
        theCamera.On_Right_Edge := False;
        theCamera.On_Upper_Edge := False;
        theCamera.On_Lower_Edge := False;
        --  The camera should continuously move until the mouse moves away from the edge.
        if Delta_X = 0.0 then
            theCamera.On_Left_Edge := (Cursor_X <= Margin);
            if not theCamera.On_Left_Edge then
                theCamera.On_Right_Edge :=
                  (Cursor_X >= (Coordinate (theCamera.Window_Width) - Margin));
            end if;
        else
            theCamera.Angle_H := theCamera.Angle_H + Degree (Delta_X) / 20.0;
        end if;

        if Delta_Y = 0.0 then
            theCamera.On_Upper_Edge := (Cursor_Y <= Margin);
            if not theCamera.On_Upper_Edge then
                theCamera.On_Lower_Edge :=
                  (Cursor_Y >= (Coordinate (theCamera.Window_Height) - Margin));
            end if;
        else
            theCamera.Angle_V := theCamera.Angle_V + Degree (Delta_Y) / 20.0;
        end if;

        Update (theCamera);
    end Process_Mouse;

    --  -------------------------------------------------------------------------

    procedure Set_Step_Size (Step_Size : GL.Types.Single) is
    begin
        Step_Scale := Step_Size;
    end Set_Step_Size;

    --  -------------------------------------------------------------------------

    --  Update_Render implements void Camera::OnRender()
    procedure Update_Render (theCamera : in out Camera) is
        use Maths;
        Should_Update : Boolean :=
                          theCamera.On_Left_Edge or theCamera.On_Right_Edge;
    begin
        if Should_Update then
            --  The camera should continuously move until the mouse moves away from the edge.
            if theCamera.On_Left_Edge then
                theCamera.Angle_H := theCamera.Angle_H - Edge_Step;
            elsif theCamera.On_Right_Edge then
                theCamera.Angle_H := theCamera.Angle_H + Edge_Step;
            end if;
        end if;

        if theCamera.On_Upper_Edge then
            Should_Update := theCamera.Angle_V > -90.0;
            theCamera.Angle_V := theCamera.Angle_V - Edge_Step;
        elsif theCamera.On_Lower_Edge then
            Should_Update := theCamera.Angle_V < 90.0;
            theCamera.Angle_H := theCamera.Angle_H + Edge_Step;
        end if;

        if Should_Update then
            Update (theCamera);
        end if;

    exception
        when  others =>
            Put_Line ("An exception occurred in Ogldev_Camera.Update_Render.");
            raise;
    end Update_Render;

    --  -------------------------------------------------------------------------

    procedure Update (theCamera : in out Camera) is
        use GL.Types.Singles;
        use Maths;
        H_Axis : Vector3;
        V_Axis : constant Vector3 := (0.0, 1.0, 0.0);
        View   : GL.Types.Singles.Vector4 := (1.0, 0.0, 0.0, 1.0);
    begin
        --  Rotate the view vector by the horizontal angle around the vertical axis
        View := Normalized (Maths.Rotation_Matrix (theCamera.Angle_H, V_Axis) * View);
        --  Rotate the view vector by the vertical angle around the horizontal axis
        H_Axis := Normalized (Cross_Product (V_Axis, To_Vector3 (View)));
        View := Normalized (Maths.Rotation_Matrix (theCamera.Angle_V, H_Axis) * View);

        theCamera.Target := Normalized (To_Vector3 (View));
        theCamera.Up := Normalized (Cross_Product (theCamera.Target, H_Axis));
    end Update;

    --  -------------------------------------------------------------------------

    Procedure Update_Camera (theCamera : in out Camera;
                             Window    : in out Glfw.Windows.Window) is
    begin
        Process_Keyboard (theCamera, Window);  --  OnKeyboard
        Process_Mouse (theCamera, Window);     --  PassiveMouseCB.OnMouse
        Update_Render (theCamera);             --  OnRender
        theCamera.On_Left_Edge := False;
        theCamera.On_Right_Edge := False;

    end Update_Camera;

    --  ------------------------------------------------------------------------

end Ogldev_Camera;
