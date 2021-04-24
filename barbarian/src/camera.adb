
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

with Blood_Splats;
with FB_Effects;
with Frustum;
with Manifold;
with Prop_Renderer;
with Settings;
with Sprite_Renderer;

package body Camera is

    G_Camera      : Camera_Data;
    Prev_Cam_Pos  : Singles.Vector3 := (0.0, 0.0, 0.0);
    Far_Point_Dir : Singles.Vector3 := (0.0, 0.0, -1.0);
--      First_Person  : Boolean := False;

    --  ------------------------------------------------------------------------

    function Camera_Height return Single is
    begin
        return G_Camera.Height;
    end Camera_Height;

    --  ------------------------------------------------------------------------

    procedure Set_Camera_Wind_In is
    begin
        G_Camera.Wind_In_Countdown := 2.0;
    end Set_Camera_Wind_In;

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
        return G_Camera.Field_Of_View_Y;
    end Field_Of_View_Y;

    --  ------------------------------------------------------------------------

    function GUI_Proj_Matrix return Singles.Matrix4 is
    begin
        return G_Camera.GUI_Proj_Matrix;
    end GUI_Proj_Matrix;

    --  ------------------------------------------------------------------------

    procedure Init is
        use Singles;
        use Maths;
--          Dir               : Singles.Vector3;
--          First_Person_Pos  : Singles.Vector3;
    begin
        G_Camera.World_Position := (2.0, 50.0, 2.0);  --  2.0, 10.0, 2.0
        Prev_Cam_Pos := G_Camera.World_Position;
        G_Camera.Shake_Mod_Position := (0.0, 0.0, 0.0);
        G_Camera.Original_Screen_Shake_Time := 0.0;
        G_Camera.Screen_Shake_Countdown_Secs := 0.0;
        G_Camera.Wind_In_Countdown := 0.0;
        G_Camera.Original_Screen_Shake_Amplitude := 1.0;
        G_Camera.Screen_Shake_Amplitude := 0.0;
        G_Camera.Screen_Shake_Frequency := 0.0;
        G_Camera.Wind_In_Angle := 0.0;
        G_Camera.Field_Of_View_Y := 87.0;  --  67.0
        G_Camera.Aspect := Single (Settings.Framebuffer_Height) /
          Single (Settings.Framebuffer_Width);
        G_Camera.Near := 0.1;  --  0.1
        G_Camera.Far := Settings.Far_Clip;
        Far_Point_Dir := (0.0, 0.0, -1.0);
--          if First_Person then   --  only set true in Debug Mode
--              Dir := G_Camera.World_Position - Prev_Cam_Pos;
--              Dir (GL.Y) := 0.0;
--              First_Person_Pos := G_Camera.World_Position;
--              First_Person_Pos (GL.Y) := First_Person_Pos (GL.Y) - 11.0;
--              Maths.Init_Lookat_Transform (Position => First_Person_Pos,
--                                           Target   => First_Person_Pos + Dir,
--                                           Up       => (0.0, 1.0, 0.0),
--                                           Look_At  => G_Camera.View_Matrix);
--          else
            Maths.Init_Lookat_Transform (Position => G_Camera.World_Position,
                                         Target   => (2.0, 0.0, 2.0),
                                         Up       => (0.0, 0.0, -1.0),
                                         Look_At  => G_Camera.View_Matrix);
--        Utilities.Print_Matrix ("Camera.Init Camera.View_Matrix",
--                                G_Camera.View_Matrix);
--          end if;

        G_Camera.Projection_Matrix := Perspective_Matrix
          (G_Camera.Field_Of_View_Y, G_Camera.Aspect, G_Camera.Near, G_Camera.Far);
--          Utilities.Print_Matrix ("Camera.Projection_Matrix", G_Camera.Projection_Matrix);
        G_Camera.GUI_Proj_Matrix := Perspective_Matrix
          (G_Camera.Field_Of_View_Y, G_Camera.Aspect, 0.01, 1000.0);
        G_Camera.Clip_Plane := Perspective_Matrix
          (G_Camera.Field_Of_View_Y, G_Camera.Aspect, 0.1, 1000.0);
        G_Camera.PV := G_Camera.Projection_Matrix * G_Camera.View_Matrix;
        G_Camera.Is_Dirty  := True;
        G_Camera.Manual_Override := False;
        G_Camera.Height := 13.0;
    end Init;

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

    procedure Recalculate_Perspective (FOV_Y                    : Maths.Degree;
                                       Width, Height, Near, Far : Single) is
        use GL.Types.Singles;
        use Maths;
    begin
        G_Camera.Field_Of_View_Y := FOV_Y;
        G_Camera.Aspect := Width / Height;
        G_Camera.Near := Near;
        G_Camera.Far := Far;
        Init_Perspective_Transform
          (FOV_Y, Width, Height, Near, Far, G_Camera.Projection_Matrix);
        Init_Perspective_Transform
          (FOV_Y, Width, Height, 0.01, 1000.0, G_Camera.GUI_Proj_Matrix);
        G_Camera.PV := G_Camera.Projection_Matrix * G_Camera.View_Matrix;
        G_Camera.Is_Dirty := True;
--          Utilities.Print_Matrix ("Recalculate_Perspective Camera.View_Matrix",
--                                  G_Camera.View_Matrix);
--          Utilities.Print_Matrix ("Recalculate_Perspective Camera.Projection_Matrix",
--                                  G_Camera.Projection_Matrix);
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
--          Dir           : Singles.Vector3;
--          Far_Point_Pos : Singles.Vector3;
        Rot_Matrix    : Singles.Matrix4;
    begin
        if not G_Camera.Manual_Override then
            Prev_Cam_Pos := G_Camera.World_Position;
            G_Camera.World_Position := World_Position;
            Cam_Target := World_Position + G_Camera.Shake_Mod_Position;
            Cam_Target (GL.Y) := Cam_Target (GL.Y) - 1.0;
            Utilities.Print_Vector ("Camera.Set_Camera_Position Target",
                                    Cam_Target);
--              if not First_Person then
                Maths.Init_Lookat_Transform
                  (World_Position + G_Camera.Shake_Mod_Position, Cam_Target,
                   (0.0, 0.0, -1.0), G_Camera.View_Matrix);
                if G_Camera.Wind_In_Angle > 0.0 then
                    Rot_Matrix := Rotate_Z_Degree (Identity4, G_Camera.Wind_In_Angle);
                    G_Camera.View_Matrix := Rot_Matrix * G_Camera.View_Matrix;
                end if;
--              else --  First_Person
--                  Dir := G_Camera.World_Position - Prev_Cam_Pos;
--                  Dir (GL.Y) := 0.0;
--                  if Abs (Dir (GL.X) + Dir (GL.Z)) > 0.0 then
--                      Far_Point_Dir := Dir;
--                  end if;
--                  Far_Point_Pos := G_Camera.World_Position;
--                  Far_Point_Pos (GL.Y) := Far_Point_Pos (GL.Y) - 11.0;
--                  Init_Lookat_Transform (Far_Point_Pos + G_Camera.Shake_Mod_Position,
--                                         Far_Point_Pos + Far_Point_Dir,
--                                         (0.0, 1.0, 0.0), G_Camera.View_Matrix);
--              end if;
--              G_Camera.PV := G_Camera.Projection_Matrix * G_Camera.View_Matrix;
--              G_Camera.Is_Dirty := True;
--              Frustum.Re_Extract_Frustum_Planes
--                (G_Camera.Field_Of_View_Y, G_Camera.Aspect, G_Camera.Near, G_Camera.Far,
--                 G_Camera.World_Position, G_Camera.View_Matrix);
        end if;
    end Set_Camera_Position;

    --  ------------------------------------------------------------------------

    procedure Set_End_Camera is
        use GL.Types.Singles;
        Ambient : constant Vector3 := (0.04, 0.04, 0.04);
    begin
        G_Camera.World_Position := Prop_Renderer.Get_End_Camera_Position;
        G_Camera.View_Matrix := Prop_Renderer.Get_End_Camera_Matrix;
        G_Camera.PV := G_Camera.Projection_Matrix * G_Camera.View_Matrix;
        G_Camera.Is_Dirty := True;
        Frustum.Re_Extract_Frustum_Planes
          (G_Camera.Field_Of_View_Y, G_Camera.Aspect, G_Camera.Near, G_Camera.Far,
           Frustum.Frustum_Camera_Position, G_Camera.View_Matrix);

        --  Raise brightness a little because torch won't be there
        Sprite_Renderer.Set_Ambient_Light_Level (Ambient);
        Manifold.Set_Manifold_Ambient_Light (Ambient);
        Prop_Renderer.Set_Ambient_Light_Level (Ambient);
        Blood_Splats.Set_Ambient_Light_Level (Ambient);

        FB_Effects.Set_WW_FB_Effect (FB_Effects.FB_Default_Effect);

    end Set_End_Camera;

    --  ------------------------------------------------------------------------

--      procedure Set_First_Person (State : Boolean) is
--      begin
--          First_Person := State;
--          Set_Camera_Position (G_Camera.World_Position);
--          Frustum.Enable_Frustum_Cull (not State);
--      end Set_First_Person;

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

    procedure Update_Camera_Effects (Elapsed_Time : Float) is
        use GL.Types;
        use Maths.Single_Math_Functions;

        Time_Factor : Float;
        XZ          : Single;
        Count_Down  : Float;
        Dist        : Single;
    begin
        if G_Camera.Screen_Shake_Countdown_Secs >= 0.0 then
            G_Camera.Screen_Shake_Countdown_Secs :=
              G_Camera.Screen_Shake_Countdown_Secs - Elapsed_Time;
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

            --  Update Camera_Position with Shake_Mod_Position
            Set_Camera_Position (G_Camera.World_Position);
        end if;

        if G_Camera.Wind_In_Countdown >= 0.0 then
            G_Camera.Wind_In_Countdown :=
              G_Camera.Wind_In_Countdown - Elapsed_Time;
            Count_Down := Maths.Max_Float (G_Camera.Wind_In_Countdown, 0.0);
            Dist := Single (40.0 / 3.0 * Count_Down);
            G_Camera.Shake_Mod_Position := (0.0, Dist, 0.0);
            G_Camera.Wind_In_Angle := Maths.Degree (1024.0 / 3.0 * Count_Down);

            --  Update Camera_Position with Shake_Mod_Position
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
