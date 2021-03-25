
with GL.Types; use GL.Types;

with Maths;

package Camera is

   type Camera_Data is private;

   procedure Camera_Wind_In;
   function Default_Camera return Camera_Data;
   function Far return Single;
   function Field_Of_View_Y return Maths.Degree;
   function GUI_Proj_Matrix return Singles.Matrix4;
   procedure Init;
   function Is_Dirty return Boolean;
   function Near return Single;
   function Projection_Matrix return Singles.Matrix4;
   function PV_Matrix return Singles.Matrix4;
   procedure Recalculate_Perspective (FOV_Y                    : Maths.Degree;
                                      Width, Height, Near, Far : Single);
   procedure Screen_Shake (Seconds, Amplitude, Hz : Float);
   procedure Set_Camera_Height (Height : Single);
   procedure Set_Camera_Position (World_Position : Singles.Vector3);
   procedure Set_End_Camera;
   procedure Set_First_Person (State : Boolean);
   procedure Set_Is_Dirty (State : Boolean);
   procedure Set_Screen_Shake_Countdown (Countdown : Float);
   function View_Matrix return Singles.Matrix4;
   procedure Update_Camera_Effects (Delta_Time : Float);
   function World_Position return Singles.Vector3;

private

   type Camera_Data is record
      PV                              : Singles.Matrix4 := Singles.Identity4;
      View_Matrix                     : Singles.Matrix4 := Singles.Identity4;
      Projection_Matrix               : Singles.Matrix4 :=
                                            (others => (others => 0.0));
      Clip_Plane                      : Singles.Matrix4 :=
                                            (others => (others => 0.0));
      GUI_Proj_Matrix                 : Singles.Matrix4 := Singles.Identity4;
      World_Position                  : Singles.Vector3 := (0.0, 0.0, 0.0);
      Shake_Mod_Position              : Singles.Vector3 := (0.0, 0.0, 0.0);
      Original_Screen_Shake_Time      : Float := 1.0;
      Screen_Shake_Countdown_Secs     : Float := 0.0;
      Wind_In_Countdown               : Float := 0.0;
      Original_Screen_Shake_Amplitude : Float := 0.0;
      Screen_Shake_Amplitude          : Float := 0.0;
      Screen_Shake_Frequency          : Float := 0.0;
      Wind_In_Angle                   : Maths.Degree := 0.0;
      FOY_Y                           : Maths.Degree := 67.0;
      Aspect                          : Single := 0.0;
      Near                            : Single := 0.0;
      Far                             : Single := 0.0;
      Height                          : Single := 13.0;
      Is_Dirty                        : Boolean := True;
      Manual_Override                 : Boolean := False;
   end record;


end Camera;
