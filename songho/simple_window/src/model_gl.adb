
package body Model_GL is

   --     procedure Init_Lights;

   --  --------------------------------------------------------

   procedure Draw (theModel : in out Model) is
   begin
      null;
   end Draw;

   --  --------------------------------------------------------

   procedure Initialize (theModel : in out Model) is
   begin
      null;
   end Initialize;

   --  --------------------------------------------------------

--     procedure Init_Lights is
--     begin
--        null;
--     end Init_Lights;

   --  --------------------------------------------------------

   procedure Rotate_Camera (theModel : in out Model;
                            X, Y     : Int) is
   begin
      null;
   end Rotate_Camera;

   --  --------------------------------------------------------

   procedure Set_Camera (theModel         : in out Model;
                         Position, Target : Singles.Vector3) is
   begin
      null;
   end Set_Camera;

   --  --------------------------------------------------------

   procedure Set_Mouse_Left (theModel : in out Model;
                             Flag     : Boolean) is
   begin
      theModel.Mouse_Left_Down := Flag;
   end Set_Mouse_Left;

   --  --------------------------------------------------------

   procedure Set_Mouse_Right (theModel : in out Model;
                              Flag     : Boolean) is
   begin
      theModel.Mouse_Right_Down := Flag;
   end Set_Mouse_Right;

   --  --------------------------------------------------------

   procedure Set_Mouse_Position (theModel : in out Model;
                                 X, Y     : Int) is
   begin
      theModel.Mouse_X := X;
      theModel.Mouse_Y := Y;
   end Set_Mouse_Position;

   --  --------------------------------------------------------

   procedure Set_Viewport (theModel      : in out Model;
                           Width, Height : Int) is
   begin
      null;
   end Set_Viewport;

   --  --------------------------------------------------------

   procedure Zoom_Camera (theModel : in out Model;
                          Distance : Int) is
   begin
      null;
   end Zoom_Camera;

   --  --------------------------------------------------------

end Model_GL;
