
with Frustum;
with Settings;

package body Camera is
    G_Cam         : Camera_Data;
    Prev_Cam_Pos  : Singles.Vector3 := (0.0, 0.0, 0.0);
    Far_Point_Dir : Singles.Vector3 := (0.0, 0.0, 0.0);
    First_Person  : Boolean := False;

    --  ------------------------------------------------------------------------

   procedure Cam_Wind_In is
   begin
      G_Cam.Wind_In_Countdown := 2.0;
   end Cam_Wind_In;

    --  ------------------------------------------------------------------------

    procedure Init is
        use Singles;
        use Maths;
        Dir               : Singles.Vector3;
        First_Person_Pos  : Singles.Vector3;
    begin
        G_Cam.World_Position := (2.0, 10.0, -2.0);  -- orig 2.0
        Prev_Cam_Pos := G_Cam.World_Position;
        G_Cam.Shake_Mod_Position := (0.0, 0.0, 0.0);
        G_Cam.Original_Screen_Shake_Time := 0.0;
        G_Cam.Screen_Shake_Countdown_Secs := 0.0;
        G_Cam.Wind_In_Countdown := 0.0;
        G_Cam.Original_Screen_Shake_Amplitude := 1.0;
        G_Cam.Screen_Shake_Amplitude := 0.0;
        G_Cam.Screen_Shake_Frequency := 0.0;
        G_Cam.Wind_In_Angle := 0.0;
        G_Cam.FOY_Y := 67.0;
        G_Cam.Aspect := Single (Settings.Framebuffer_Height) /
          Single (Settings.Framebuffer_Width);
        G_Cam.Near := 0.1;
        G_Cam.Far := Settings.Far_Clip;
        Far_Point_Dir := (0.0, 0.0, 1.0);  --  orig z -1.0
        if First_Person then
            Dir := G_Cam.World_Position - Prev_Cam_Pos;
            Dir (GL.X) := 0.0;
            First_Person_Pos := G_Cam.World_Position;
            First_Person_Pos (GL.X) := First_Person_Pos (GL.X) - 11.0;
            Maths.Init_Lookat_Transform (First_Person_Pos, First_Person_Pos + Dir,
                                          (0.0, 1.0, 0.0), G_Cam.View_Matrix);
        else
            Maths.Init_Lookat_Transform (G_Cam.World_Position, (2.0, 0.0, 2.0),
                                          (0.0, 0.0, -1.0), G_Cam.View_Matrix);
        end if;

        G_Cam.Projection_Matrix := Perspective_Matrix
          (G_Cam.FOY_Y, G_Cam.Aspect, G_Cam.Near, G_Cam.Far);
        G_Cam.GUI_Proj_Matrix := Perspective_Matrix
          (G_Cam.FOY_Y, G_Cam.Aspect, 0.01, 1000.0);
        G_Cam.Clip_Plane := Perspective_Matrix
          (G_Cam.FOY_Y, G_Cam.Aspect, 0.1, 1000.0);
        G_Cam.PV := G_Cam.Projection_Matrix * G_Cam.View_Matrix;
        G_Cam.Is_Dirty  := True;
        G_Cam.Manual_Override := False;
        G_Cam.Height := 13.0;
    end Init;

    --  ------------------------------------------------------------------------

    function Default_Camera return Camera_Data is
    begin
        return G_Cam;
    end Default_Camera;

    --  ------------------------------------------------------------------------

    function Far return Single is
    begin
        return G_Cam.Far;
    end Far;

    --  ------------------------------------------------------------------------

    function Field_Of_View_Y return Maths.Degree is
    begin
        return G_Cam.FOY_Y;
    end Field_Of_View_Y;

    --  ------------------------------------------------------------------------

    function GUI_Proj_Matrix return Singles.Matrix4 is
    begin
        return G_Cam.GUI_Proj_Matrix;
    end GUI_Proj_Matrix;

    --  ------------------------------------------------------------------------

    function Near return Single is
    begin
        return G_Cam.Near;
    end Near;

    --  ------------------------------------------------------------------------

    function Projection_Matrix return Singles.Matrix4 is
    begin
        return G_Cam.Projection_Matrix;
    end Projection_Matrix;

    --  ------------------------------------------------------------------------

    function PV_Matrix return Singles.Matrix4 is
    begin
        return G_Cam.PV;
    end PV_Matrix;

    --  ------------------------------------------------------------------------

   procedure Recalculate_Perspective (FOV_Y             : Maths.Degree;
                                      Width, Height, Near, Far : Single) is
      use GL.Types.Singles;
      use Maths;
    begin
      G_Cam.FOY_Y := FOV_Y;
      G_Cam.Aspect := Width / Height;
      G_Cam.Near := Near;
      G_Cam.Far := Far;
      Init_Perspective_Transform
        (FOV_Y, Width, Height, Near, Far, G_Cam.Projection_Matrix);
      Init_Perspective_Transform
        (FOV_Y, Width, Height, 0.01, 1000.0, G_Cam.GUI_Proj_Matrix);
      G_Cam.PV := G_Cam.Projection_Matrix * G_Cam.View_Matrix;
      G_Cam.Is_Dirty := True;
      Frustum.Re_Extract_Frustum_Planes
        (FOV_Y, G_Cam.Aspect, Near, Far, G_Cam.World_Position, G_Cam.View_Matrix);
    end Recalculate_Perspective;

    --  ------------------------------------------------------------------------

    procedure Set_Camera_Height (Height : Single) is
    begin
        G_Cam.Height := Height;
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
      if not G_Cam.Manual_Override then
         Prev_Cam_Pos := G_Cam.World_Position;
         G_Cam.World_Position := World_Position;
         Cam_Target := World_Position + G_Cam.Shake_Mod_Position;
         Cam_Target (GL.Y) := Cam_Target (GL.Y) - 1.0;
         if not First_Person then
            Maths.Init_Lookat_Transform
              (World_Position + G_Cam.Shake_Mod_Position, Cam_Target,
               (0.0, 0.0, 1.0), G_Cam.View_Matrix);
            if G_Cam.Wind_In_Angle > 0.0 then
               Rot_Matrix := Rotate_Z_Degree (Identity4, G_Cam.Wind_In_Angle);
               G_Cam.View_Matrix := Rot_Matrix * G_Cam.View_Matrix;
            end if;
         else
            Dir := G_Cam.World_Position - Prev_Cam_Pos;
            Dir (GL.Y) := 0.0;
            if Abs (Dir (GL.X) + Dir (GL.Z)) > 0.0 then
               Far_Point_Dir := Dir;
            end if;
            Far_Point_Pos := G_Cam.World_Position;
            Far_Point_Pos (GL.Y) := Far_Point_Pos (GL.Y) - 11.0 ;
            Init_Lookat_Transform (Far_Point_Pos + G_Cam.Shake_Mod_Position,
                                   Far_Point_Pos + Far_Point_Dir,
                                   (0.0, 1.0, 0.0), G_Cam.View_Matrix);
         end if;
         G_Cam.PV := G_Cam.Projection_Matrix * G_Cam.View_Matrix;
         G_Cam.Is_Dirty := True;
         Frustum.Re_Extract_Frustum_Planes
           (G_Cam.FOY_Y, G_Cam.Aspect, G_Cam.Near, G_Cam.Far,
            G_Cam.World_Position, G_Cam.View_Matrix);
      end if;
    end Set_Camera_Position;

    --  ------------------------------------------------------------------------

    procedure Set_First_Person (State : Boolean) is
    begin
      First_Person := State;
      Set_Camera_Position (G_Cam.World_Position);
      Frustum.Enable_Frustum_Cull (not State);
    end Set_First_Person;

    --  ------------------------------------------------------------------------

    function World_Position return Singles.Vector3 is
    begin
        return G_Cam.World_Position;
    end World_Position;

    --  ------------------------------------------------------------------------

end Camera;
