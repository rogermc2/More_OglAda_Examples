
with GL.Types.Colors;

package Model is

   type Model_Data is private;

   procedure Bind_Model_VAO;
   procedure Initialize (aModel : in out Model_Data; File_Path : String;
                         Colour : GL.Types.Colors.Basic_Color);
   procedure Render;
   procedure Update (aModel : in out Model_Data; Delta_Time : Float);

private
   type Model_Data is record
   Position             : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
   Heading              : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
   Base_Rotation        : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
   Heading_Rotation     : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
   Model_Colour         : GL.Types.Colors.Basic_Color := (0.0, 0.0, 0.0);
   Velocity             : Float := 0.0;
   Radius               : Float := 1.0;
   Is_Ship              : Boolean := False;
   Is_Visible           : Boolean := True;
   Is_Collidable        : Boolean := True;
   end record;

end Model;
