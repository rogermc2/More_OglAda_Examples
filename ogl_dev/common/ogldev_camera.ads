

with Maths;
with GL.Types; use GL.Types;

with Glfw.Windows;

Package Ogldev_Camera is

   type Camera is private;

   function Get_Position (theCamera : Camera) return Singles.Vector3;
   function Get_Target (theCamera : Camera) return Singles.Vector3;
   function Get_Up (theCamera : Camera) return Singles.Vector3;

   procedure Init_Camera (theCamera : in out Camera;
                          Window_Width, Window_Height : Int);
   procedure Init_Camera (theCamera : in out Camera;
                          Window_Width, Window_Height : Int;
                          Position, Target, Up : Singles.Vector3);
   procedure Update_Camera (theCamera : in out Camera;
                            Window : in out Glfw.Windows.Window);

private
   type Camera is record
      Position       : Singles.Vector3 := (0.0, 0.0, -3.0);
      Target         : Singles.Vector3 := (0.0, 0.0, 1.0);
      Up             : Singles.Vector3 := (0.0, 1.0, 0.0);
      Window_Width   : Int := 800;
      Window_Height  : Int := 600;
      Angle_H        : Maths.Degree := 0.0;
      Angle_V        : Maths.Degree := 0.0;
      On_Upper_Edge  : Boolean := False;
      On_Lower_Edge  : Boolean := False;
      On_Left_Edge   : Boolean := False;
      On_Right_Edge  : Boolean := False;
      Mouse_Position : Ints.Vector2 := (0, 0);
   end record;

end Ogldev_Camera;
