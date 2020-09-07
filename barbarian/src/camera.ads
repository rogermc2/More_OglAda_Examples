
with GL.Types; use GL.Types;

with Maths;

package Camera is

    type Camera_Data is record
        PV                 : Singles.Matrix4 := Singles.Identity4;
        View_Matrix        : Singles.Matrix4 := (others => (others => 0.0));
        Projection_Matrix  : Singles.Matrix4 := (others => (others => 0.0));
        Clip_Plane         : Singles.Matrix4 := (others => (others => 0.0));
        World_Position     : Singles.Vector3 := (0.0, 0.0, 0.0);
        Shake_Mod_Position : Singles.Vector3 := (0.0, 0.0, 0.0);
        Original_Screen_Shake_Time      : Float := 1.0;
        Screen_Shake_Countdown_Secs     : Float := 0.0;
        Wind_In_Countdown               : Float := 0.0;
        Original_Screen_Shake_Amplitude : Float := 0.0;
        Screen_Shake_Amplitude          : Float := 0.0;
        Screen_Shake_Frequency          : Float := 0.0;
        Wind_In_Angle                   : Float := 0.0;
        Field_Of_View_Y                 : Maths.Degree := 67.0;
        Aspect                          : Single := 0.0;
        Near                            : Single := 0.0;
        Far                             : Single := 0.0;
        Height                          : Single := 13.0;
        Is_Dirty                        : Boolean := True;
        Manual_Override                 : Boolean := False;
    end record;

    function Default_Camera return Camera_Data;
    function Projection_Matrix return Singles.Matrix4;
    procedure Init_Camera;
    procedure Set_Camera_Height (Height : Single);

end Camera;
