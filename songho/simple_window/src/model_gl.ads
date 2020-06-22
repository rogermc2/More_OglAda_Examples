
with GL.Types; use GL.Types;

package Model_GL is

   type Model is private;

   procedure Draw (theModel : in out Model);
   procedure Initialize (theModel : in out Model);
   procedure Rotate_Camera (theModel : in out Model;
                            X, Y     : Int);
   procedure Set_Camera (theModel         : in out Model;
                         Position, Target : Singles.Vector3);
   procedure Set_Mouse_Left (theModel : in out Model;
                             Flag     : Boolean);
   procedure Set_Mouse_Right (theModel : in out Model;
                              Flag     : Boolean);
   procedure Set_Mouse_Position (theModel : in out Model;
                                 X, Y     : Int);
   procedure Set_Viewport (theModel      : in out Model;
                           Width, Height : Int);
   procedure Zoom_Camera (theModel : in out Model;
                          Distance : Int);

private
   type Model is record
    Mouse_Left_Down  : Boolean := False;
    Mouse_Right_Down : Boolean := False;
    Mouse_X          : Int := 0;
    Mouse_Y          : Int := 0;
    Camera_Angle_X   : Single := 0.0;
    Camera_Angle_Y   : Single := 0.0;
    Camera_Distance  : Single := 0.0;
   end record;

end Model_GL;
