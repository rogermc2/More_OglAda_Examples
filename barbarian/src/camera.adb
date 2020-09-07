
with Settings;

package body Camera is
    G_Cam         : Camera_Data;
    Prev_Cam_Pos  : Singles.Vector3 := (0.0, 0.0, 0.0);
    Far_Point_Dir : Singles.Vector3 := (0.0, 0.0, -1.0);
    First_Person  : Boolean := False;

    --  ------------------------------------------------------------------------

    function Projection_Matrix return Singles.Matrix4 is
    begin
        return G_Cam.Projection_Matrix;
    end Projection_Matrix;

    --  ------------------------------------------------------------------------
    procedure Init_Camera is
        use Singles;
        use Maths;
        Dir     : Singles.Vector3;
        FP_Pos  : Singles.Vector3;
    begin
        G_Cam.World_Position := (2.0, 10.0, 2.0);
        Prev_Cam_Pos := G_Cam.World_Position;
        G_Cam.Shake_Mod_Position := (0.0, 0.0, 0.0);
        G_Cam.Original_Screen_Shake_Time := 0.0;
        G_Cam.Screen_Shake_Countdown_Secs := 0.0;
        G_Cam.Wind_In_Countdown := 0.0;
        G_Cam.Original_Screen_Shake_Amplitude := 1.0;
        G_Cam.Screen_Shake_Amplitude := 0.0;
        G_Cam.Screen_Shake_Frequency := 0.0;
        G_Cam.Wind_In_Angle := 0.0;
        G_Cam.Field_Of_View_Y := 67.0;
        G_Cam.Aspect := Single (Settings.Framebuffer_Height) /
          Single (Settings.Framebuffer_Width);
        G_Cam.Near := 0.1;
        G_Cam.Far := Settings.Far_Clip;
        Far_Point_Dir := (0.0, 0.0, -1.0);
        if First_Person then
            Dir := G_Cam.World_Position - Prev_Cam_Pos;
            Dir (GL.X) := 0.0;
            FP_Pos := G_Cam.World_Position;
            FP_Pos (GL.X) := FP_Pos (GL.X) - 11.0;
             Maths.Init_Lookat_Transform (FP_Pos, FP_Pos + Dir,
                                          (0.0, 1.0, 0.0), G_Cam.View_Matrix);
        else
            Maths.Init_Lookat_Transform (G_Cam.World_Position, (2.0, 0.0, 2.0),
                                          (0.0, 0.0, -1.0), G_Cam.View_Matrix);
        end if;

        G_Cam.Projection_Matrix := Perspective_Matrix
          (G_Cam.Field_Of_View_Y, G_Cam.Aspect, G_Cam.Near, G_Cam.Far);
        G_Cam.Clip_Plane := Perspective_Matrix
          (G_Cam.Field_Of_View_Y, G_Cam.Aspect, 0.1, 1000.0);
        G_Cam.PV := G_Cam.Projection_Matrix * G_Cam.View_Matrix;
        G_Cam.Is_Dirty  := True;
        G_Cam.Manual_Override := False;
        G_Cam.Height := 13.0;
    end Init_Camera;

    --  ------------------------------------------------------------------------

    function Default_Camera return Camera_Data is
    begin
        return G_Cam;
    end Default_Camera;

    --  ------------------------------------------------------------------------

    procedure Set_Camera_Height (Height : Single) is
    begin
        G_Cam.Height := Height;
    end Set_Camera_Height;

    --  ------------------------------------------------------------------------
end Camera;
