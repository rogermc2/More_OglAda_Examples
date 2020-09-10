

with Maths;
with GL.Types; use GL.Types;

with Glfw.Windows;
with Glfw.Input.Mouse;

Package Ogldev_Camera is

   type Camera is private;

   Camera_Exception : Exception;

   function Get_Position (theCamera : Camera) return Singles.Vector3;
   function Get_Target (theCamera : Camera) return Singles.Vector3;
   function Get_Up (theCamera : Camera) return Singles.Vector3;

   procedure Init_Camera (theCamera : in out Camera;
                          Window_Width, Window_Height : Int;
                          Camera_Position, Target_Position, Up : Singles.Vector3);
   procedure Init_Camera (theCamera : in out Camera;
                          Window    : in out Glfw.Windows.Window);
   procedure Init_Camera (theCamera : in out Camera;
                          Window    : in out Glfw.Windows.Window;
                          Camera_Position, Target_Position, Up : Singles.Vector3);
   procedure Process_Mouse (theCamera : in out Camera;
                            Window    : in out Glfw.Windows.Window);
   procedure Set_Step_Size (Step_Size : GL.Types.Single);
   procedure Update_Camera (theCamera : in out Camera;
                            Window    : in out Glfw.Windows.Window);
   procedure Update_Render (theCamera : in out Camera);

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
      Mouse_X        : Glfw.Input.Mouse.Coordinate := 0.0;
      Mouse_Y        : Glfw.Input.Mouse.Coordinate := 0.0;
   end record;

end Ogldev_Camera;
