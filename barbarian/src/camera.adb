
with FB_Effects;
with Frustum;
with Settings;

package body Camera is

    G_Camera      : Camera_Data;
    Prev_Cam_Pos  : Singles.Vector3 := (0.0, 0.0, 0.0);
    Far_Point_Dir : Singles.Vector3 := (0.0, 0.0, -1.0);
    First_Person  : Boolean := False;

    --  ------------------------------------------------------------------------

    procedure Camera_Wind_In is
    begin
        G_Camera.Wind_In_Countdown := 2.0;
    end Camera_Wind_In;

    --  ------------------------------------------------------------------------

    procedure Init is
        use Singles;
        use Maths;
        Dir               : Singles.Vector3;
        First_Person_Pos  : Singles.Vector3;
    begin
--          G_Camera.World_Position := (2.0, 10.0, 2.0);  -- orig 2.0
        G_Camera.World_Position := (0.0, 0.0, 2.0);  -- orig 2.0
        Prev_Cam_Pos := G_Camera.World_Position;
        G_Camera.Shake_Mod_Position := (0.0, 0.0, 0.0);
        G_Camera.Original_Screen_Shake_Time := 0.0;
        G_Camera.Screen_Shake_Countdown_Secs := 0.0;
        G_Camera.Wind_In_Countdown := 0.0;
        G_Camera.Original_Screen_Shake_Amplitude := 1.0;
        G_Camera.Screen_Shake_Amplitude := 0.0;
        G_Camera.Screen_Shake_Frequency := 0.0;
        G_Camera.Wind_In_Angle := 0.0;
        G_Camera.FOY_Y := 67.0;
        G_Camera.Aspect := Single (Settings.Framebuffer_Height) /
          Single (Settings.Framebuffer_Width);
        G_Camera.Near := 0.1;  --  0.1
        G_Camera.Far := Settings.Far_Clip;
        Far_Point_Dir := (0.0, 0.0, -1.0);
        if First_Person then
            Dir := G_Camera.World_Position - Prev_Cam_Pos;
            Dir (GL.Y) := 0.0;
            First_Person_Pos := G_Camera.World_Position;
            First_Person_Pos (GL.Y) := First_Person_Pos (GL.Y) - 11.0;
            Maths.Init_Lookat_Transform (Position => First_Person_Pos,
                                         Target   => First_Person_Pos + Dir,
                                         Up       => (0.0, 1.0, 0.0),
                                         Look_At  => G_Camera.View_Matrix);
        else
            Maths.Init_Lookat_Transform (Position => G_Camera.World_Position,
                                         Target   => (2.0, 0.0, 2.0),
                                         Up       => (0.0, 0.0, -1.0),
                                         Look_At  => G_Camera.View_Matrix);
        end if;

        G_Camera.Projection_Matrix := Perspective_Matrix
          (G_Camera.FOY_Y, G_Camera.Aspect, G_Camera.Near, G_Camera.Far);
        G_Camera.GUI_Proj_Matrix := Perspective_Matrix
          (G_Camera.FOY_Y, G_Camera.Aspect, 0.01, 1000.0);
        G_Camera.Clip_Plane := Perspective_Matrix
          (G_Camera.FOY_Y, G_Camera.Aspect, 0.1, 1000.0);
        G_Camera.PV := G_Camera.Projection_Matrix * G_Camera.View_Matrix;
        G_Camera.Is_Dirty  := True;
        G_Camera.Manual_Override := False;
        G_Camera.Height := 13.0;
    end Init;

    --  ------------------------------------------------------------------------

    function Default_Camera return Camera_Data is
    begin
        return G_Camera;
    end Default_Camera;

    --  ------------------------------------------------------------------------

    function Far return Single is
    begin
        return G_Camera.Far;
    end Far;

    --  ------------------------------------------------------------------------

    function Field_Of_View_Y return Maths.Degree is
    begin
        return G_Camera.FOY_Y;
    end Field_Of_View_Y;

    --  ------------------------------------------------------------------------

    function GUI_Proj_Matrix return Singles.Matrix4 is
    begin
        return G_Camera.GUI_Proj_Matrix;
    end GUI_Proj_Matrix;

    --  ------------------------------------------------------------------------

    function Is_Dirty return Boolean is
    begin
        return G_Camera.Is_Dirty;
    end Is_Dirty;

    --  ------------------------------------------------------------------------

    function Near return Single is
    begin
        return G_Camera.Near;
    end Near;

    --  ------------------------------------------------------------------------

    function Projection_Matrix return Singles.Matrix4 is
    begin
        return G_Camera.Projection_Matrix;
    end Projection_Matrix;

    --  ------------------------------------------------------------------------

    function PV_Matrix return Singles.Matrix4 is
    begin
        return G_Camera.PV;
    end PV_Matrix;

    --  ------------------------------------------------------------------------

    procedure Recalculate_Perspective (FOV_Y             : Maths.Degree;
                                       Width, Height, Near, Far : Single) is
        use GL.Types.Singles;
        use Maths;
    begin
        G_Camera.FOY_Y := FOV_Y;
        G_Camera.Aspect := Width / Height;
        G_Camera.Near := Near;
        G_Camera.Far := Far;
        Init_Perspective_Transform
          (FOV_Y, Width, Height, Near, Far, G_Camera.Projection_Matrix);
        Init_Perspective_Transform
          (FOV_Y, Width, Height, 0.01, 1000.0, G_Camera.GUI_Proj_Matrix);
        G_Camera.PV := G_Camera.Projection_Matrix * G_Camera.View_Matrix;
        G_Camera.Is_Dirty := True;
        Frustum.Re_Extract_Frustum_Planes
          (FOV_Y, G_Camera.Aspect, Near, Far, G_Camera.World_Position, G_Camera.View_Matrix);
    end Recalculate_Perspective;

    --  ------------------------------------------------------------------------

    procedure Screen_Shake (Seconds, Amplitude, Hz : Float) is
    begin
        if Seconds > 0.0 then
            if G_Camera.Screen_Shake_Countdown_Secs <= 0.0 or
              Amplitude >= G_Camera.Original_Screen_Shake_Amplitude then
                G_Camera.Screen_Shake_Countdown_Secs := Seconds;
                G_Camera.Screen_Shake_Amplitude := Amplitude;
                G_Camera.Original_Screen_Shake_Amplitude := Amplitude;
                G_Camera.Screen_Shake_Frequency := Hz;
                G_Camera.Original_Screen_Shake_Time := Seconds;
            end if;
        end if;
    end Screen_Shake;

    --  ------------------------------------------------------------------------

    procedure Set_Camera_Height (Height : Single) is
    begin
        G_Camera.Height := Height;
    end Set_Camera_Height;

    --  ------------------------------------------------------------------------

    procedure Set_Camera_Position (World_Position : Singles.Vector3) is
        use GL.Types.Singles;
        use Maths;
        Cam_Target    : Singles.Vector3;
        Dir           : Singles.Vector3;
        Far_Point_Pos : Singles.Vector3;
        Rot_Matrix    : Singles.Matrix4;
    begin
        if not G_Camera.Manual_Override then
            Prev_Cam_Pos := G_Camera.World_Position;
            G_Camera.World_Position := World_Position;
            Cam_Target := World_Position + G_Camera.Shake_Mod_Position;
            Cam_Target (GL.Y) := Cam_Target (GL.Y) - 1.0;
            if not First_Person then
                Maths.Init_Lookat_Transform
                  (World_Position + G_Camera.Shake_Mod_Position, Cam_Target,
                   (0.0, 0.0, 1.0), G_Camera.View_Matrix);
                if G_Camera.Wind_In_Angle > 0.0 then
                    Rot_Matrix := Rotate_Z_Degree (Identity4, G_Camera.Wind_In_Angle);
                    G_Camera.View_Matrix := Rot_Matrix * G_Camera.View_Matrix;
                end if;
            else
                Dir := G_Camera.World_Position - Prev_Cam_Pos;
                Dir (GL.Y) := 0.0;
                if Abs (Dir (GL.X) + Dir (GL.Z)) > 0.0 then
                    Far_Point_Dir := Dir;
                end if;
                Far_Point_Pos := G_Camera.World_Position;
                Far_Point_Pos (GL.Y) := Far_Point_Pos (GL.Y) - 11.0 ;
                Init_Lookat_Transform (Far_Point_Pos + G_Camera.Shake_Mod_Position,
                                       Far_Point_Pos + Far_Point_Dir,
                                       (0.0, 1.0, 0.0), G_Camera.View_Matrix);
            end if;
            G_Camera.PV := G_Camera.Projection_Matrix * G_Camera.View_Matrix;
            G_Camera.Is_Dirty := True;
            Frustum.Re_Extract_Frustum_Planes
              (G_Camera.FOY_Y, G_Camera.Aspect, G_Camera.Near, G_Camera.Far,
               G_Camera.World_Position, G_Camera.View_Matrix);
        end if;
    end Set_Camera_Position;

    --  ------------------------------------------------------------------------

    procedure Set_First_Person (State : Boolean) is
    begin
        First_Person := State;
        Set_Camera_Position (G_Camera.World_Position);
        Frustum.Enable_Frustum_Cull (not State);
    end Set_First_Person;

    --  ------------------------------------------------------------------------

    procedure Set_Is_Dirty (State : Boolean) is
    begin
        G_Camera.Is_Dirty := State;
    end Set_Is_Dirty;

    --  ------------------------------------------------------------------------

    procedure Set_Screen_Shake_Countdown (Countdown : Float) is
    begin
        G_Camera.Screen_Shake_Countdown_Secs := Countdown;
    end Set_Screen_Shake_Countdown;

    --  ------------------------------------------------------------------------

    function View_Matrix return Singles.Matrix4 is
    begin
        return G_Camera.View_Matrix;
    end View_Matrix;

    --  ------------------------------------------------------------------------

    procedure Update_Camera_Effects (Delta_Time : Float) is
        use GL.Types;
        use Maths.Single_Math_Functions;
        Time_Factor : Float;
        XZ          : Single;
        CD          : Float;
        Dist        : Single;
    begin
        if G_Camera.Screen_Shake_Countdown_Secs > 0.0 then
            G_Camera.Screen_Shake_Countdown_Secs :=
              G_Camera.Screen_Shake_Countdown_Secs - Delta_Time;
            if G_Camera.Screen_Shake_Countdown_Secs <= 0.0 then
                G_Camera.Shake_Mod_Position := (0.0, 0.0, 0.0);
            end if;
            Time_Factor := 1.0 -
              (G_Camera.Original_Screen_Shake_Time -
                 G_Camera.Screen_Shake_Countdown_Secs) /
                G_Camera.Original_Screen_Shake_Time;
            G_Camera.Screen_Shake_Amplitude :=
              Time_Factor * G_Camera.Original_Screen_Shake_Amplitude;
              XZ := Sin (Single (G_Camera.Screen_Shake_Countdown_Secs *
                            G_Camera.Screen_Shake_Frequency)) *
                Single (G_Camera.Screen_Shake_Amplitude);
            G_Camera.Shake_Mod_Position (GL.X) := XZ;
            G_Camera.Shake_Mod_Position (GL.Z) := XZ;
            Set_Camera_Position (G_Camera.World_Position);
        end if;

        if G_Camera.Wind_In_Countdown >= 0.0 then
            G_Camera.Wind_In_Countdown :=
              G_Camera.Wind_In_Countdown - Delta_Time;
            CD := Maths.Max_Float (G_Camera.Wind_In_Countdown, 0.0);
            Dist := Single (40.0 / 3.0 * CD);
            G_Camera.Shake_Mod_Position := (0.0, Dist, 0.0);
            G_Camera.Wind_In_Angle := Maths.Degree (1024.0 / 3.0 * CD);
            Set_Camera_Position (G_Camera.World_Position);
            FB_Effects.Set_Feedback_Screw (G_Camera.Wind_In_Countdown);
        end if;
    end Update_Camera_Effects;

    --  ------------------------------------------------------------------------

    function World_Position return Singles.Vector3 is
    begin
        return G_Camera.World_Position;
    end World_Position;

    --  ------------------------------------------------------------------------

end Camera;
