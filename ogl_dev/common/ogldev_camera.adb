
with Interfaces.C;

with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Input.Mouse;

package body Ogldev_Camera is
   Step_Scale  : constant GL.Types.Single := 0.01;  --  orig: 1.0
   Edge_Step   : constant Maths.Degree := 0.5;
   Margin      : constant Glfw.Input.Mouse.Coordinate := 10.0;

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

   procedure Init_Camera (theCamera : in out Camera) is
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
      end if;

      theCamera.Angle_V := -To_Degree (Arcsin (theCamera.Target (GL.Y)));

      theCamera.On_Upper_Edge := False;
      theCamera.On_Lower_Edge := False;
      theCamera.On_Left_Edge := False;
      theCamera.On_Right_Edge := False;

      theCamera.Mouse_Position (GL.X) := theCamera.Window_Width / 2;
      theCamera.Mouse_Position (GL.Y) := theCamera.Window_Height / 2;

   exception
      when  others =>
         Put_Line ("An exception occurred in Ogldev_Camera.Init_Camera 1.");
         raise;

   end Init_Camera;

   --  -------------------------------------------------------------------------

   --  From camera.cpp Camera::Camera
   procedure Init_Camera (theCamera                   : in out Camera;
                          Window_Width, Window_Height : Int) is
   begin
      theCamera.Window_Width := Window_Width;
      theCamera.Window_Height := Window_Height;
      theCamera.Position := (0.0, 0.0, 0.0);
      theCamera.Target := (0.0, 0.0, 1.0);
      theCamera.Target := Maths.Normalized (theCamera.Target);
      theCamera.Up := (0.0, 1.0, 0.0);
      Init_Camera (theCamera);

   exception
      when  others =>
         Put_Line ("An exception occurred in Ogldev_Camera.Init_Camera 2.");
         raise;
   end Init_Camera;

   --  -------------------------------------------------------------------------

   --  From camera.cpp Camera::Camera
   procedure Init_Camera (theCamera                   : in out Camera;
                          Window_Width, Window_Height : Int;
                          Camera_Position, Target_Position, Up : Singles.Vector3) is
   begin
      theCamera.Window_Width := Window_Width;
      theCamera.Window_Height := Window_Height;
      if Maths.Length (Camera_Position) /= 0.0 then
         theCamera.Position := Maths.Normalized (Camera_Position);
      else
         theCamera.Position := Camera_Position;
      end if;

      if Maths.Length (Target_Position) /= 0.0 then
         theCamera.Target := Maths.Normalized (Target_Position);
      else
         theCamera.Target := Target_Position;
      end if;

      if Maths.Length (Up) /= 0.0 then
         theCamera.Up := Maths.Normalized (Up);
      else
         Put_Line ("Ogldev_Camera.Init_Camera, Invalid Up vector detected.");
         Put_Line ("Settin Up vector to (0.0, 1.0, 0.0).");
         theCamera.Up := (0.0, 1.0, 0.0);
      end if;
      Init_Camera (theCamera);
   exception
      when  others =>
         Put_Line ("An exception occurred in Ogldev_Camera.Init_Camera 3.");
         raise;
   end Init_Camera;

   --  -------------------------------------------------------------------------

   procedure Process_Keyboard (theCamera : in out Camera;
                               Window    : in out Glfw.Windows.Window) is
      use Glfw.Input;
      use GL.Types.Singles;  --  Needed to support arithmetic operations.
      use Maths;
      Vec : Vector3;
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
                            Window    : in out Glfw.Windows.Window;
                            X_Position, Y_Position : in out Glfw.Input.Mouse.Coordinate) is
      use Interfaces.C;
      use Glfw.Input.Mouse;
      use Maths;
      Delta_X  : Coordinate;
      Delta_Y  : Coordinate;
   begin
      Window'Access.Get_Cursor_Pos (X_Position, Y_Position);
      Delta_X := X_Position - Coordinate (theCamera.Mouse_Position (GL.X));
      Delta_Y := Y_Position - Coordinate (theCamera.Mouse_Position (GL.Y));

      theCamera.Mouse_Position := (GL.Types.Int (X_Position), GL.Types.Int (Y_Position));
      theCamera.Angle_H := theCamera.Angle_H + Degree (Delta_X) / 20.0;
      theCamera.Angle_V := theCamera.Angle_V + Degree (Delta_Y) / 20.0;

      if Delta_X = 0.0 then
         theCamera.On_Left_Edge := (X_Position <= Margin);
         theCamera.On_Right_Edge :=
           (X_Position >= (Coordinate (theCamera.Window_Width) - Margin));
      end if;
      if Delta_Y = 0.0 then
         theCamera.On_Upper_Edge := (Y_Position <= Margin);
         theCamera.On_Lower_Edge :=
           (Y_Position >= (Coordinate (theCamera.Window_Height) - Margin));
      end if;

      Update (theCamera);
   end Process_Mouse;

   --  -------------------------------------------------------------------------

   --  Update_Render implements void Camera::OnRender()
   procedure Update_Render (theCamera : in out Camera) is
      use Maths;
      Should_Update : Boolean := False;
   begin
      if theCamera.On_Left_Edge then
         theCamera.Angle_H := theCamera.Angle_H - Edge_Step;
         Should_Update := True;
      elsif theCamera.On_Right_Edge then
         theCamera.Angle_H := theCamera.Angle_H + Edge_Step;
         Should_Update := True;
      end if;

      if theCamera.On_Upper_Edge then
         if theCamera.Angle_V > -90.0 then
            theCamera.Angle_V := theCamera.Angle_V - Edge_Step;
            Should_Update := True;
         end if;
      elsif theCamera.On_Lower_Edge then
         if theCamera.Angle_V < 90.0 then
            theCamera.Angle_H := theCamera.Angle_H + Edge_Step;
            Should_Update := True;
         end if;
      end if;

      if Should_Update then
         Update (theCamera);
      end if;

   exception
      when  others =>
         Put_Line ("An exception occurred in Ogldev_Camera.Update.");
         raise;
   end Update_Render;

   --  -------------------------------------------------------------------------

   procedure Update (theCamera : in out Camera) is
        use GL.Types.Singles;
        use Maths;
        H_Axis : Vector3;
        V_Axis : constant Vector3 := (0.0, 1.0, 0.0);
        View   : Vector4 := (1.0, 0.0, 0.0, 1.0);
   begin
        --  Rotate the view vector by the horizontal angle around the vertical axis
        View := Normalized (Maths.Rotation_Matrix (theCamera.Angle_H, V_Axis) * View);
        --  Rotate the view vector by the vertical angle around the horizontal axis
        H_Axis := Normalized (Cross_Product (V_Axis, To_Vector3 (View)));
        theCamera.Target := Normalized (To_Vector3 (View));
        theCamera.Up := Normalized (Cross_Product(theCamera.Target, H_Axis));
   end Update;

   --  -------------------------------------------------------------------------

   Procedure Update_Camera (theCamera : in out Camera;
                            Window    : in out Glfw.Windows.Window) is
      use Glfw.Input;
      use Glfw.Input.Mouse;
--        Current_Time       : constant GL.Types.Double := GL.Types.Double (Glfw.Time);
--        Window_Width       : Glfw.Size;
--        Window_Height      : Glfw.Size;
--        Half_Window_Width  : Single;
--        Half_Window_Height : Single;
      X_Position         : Coordinate := 0.00001;
      Y_Position         : Coordinate := 0.00002;
   begin
      Process_Mouse (theCamera, Window, X_Position, Y_Position);  --  PassiveMouseCB.OnMouse
      Process_Keyboard (theCamera, Window);                       --  OnKeyboard
      Update_Render (theCamera);                                  --  OnRender

--        Window'Access.Get_Size (Window_Width, Window_Height);
--        theCamera.Window_Width := GL.Types.Int (Window_Width);
--        theCamera.Window_Height := GL.Types.Int (Window_Height);
--        Half_Window_Width := 0.5 * Single (theCamera.Window_Width);
--        Half_Window_Height := 0.5 * Single (theCamera.Window_Height);
--
--        Reset the cursor to the center of the screen
--        otherwise it will soon go outside the window.
--
--        Window'Access.Set_Cursor_Pos (Mouse.Coordinate (Half_Window_Width),
--                                      Mouse.Coordinate (Half_Window_Height));

   end Update_Camera;

   --  ------------------------------------------------------------------------

end Ogldev_Camera;
