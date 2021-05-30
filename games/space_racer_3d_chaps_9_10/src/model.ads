
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types.Colors;

package Model is

   type Model_Data is private;

   procedure Bind_Model_VAO (aModel : in out Model_Data);
   procedure Initialize (aModel : in out Model_Data; File_Path : String;
                         Colour : GL.Types.Colors.Basic_Color);
   procedure Render (aModel : in out Model_Data);
   procedure Update (aModel : in out Model_Data; Delta_Time : Float);

private

--     type Rotation_Vector is
--        array (GL.Index_3D range GL.X .. GL.Z) of Maths.Degree;

   type Model_Data is record
   Model_VAO            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Model_Vertex_Buffer  : GL.Objects.Buffers.Buffer;
   Model_Normals_Buffer : GL.Objects.Buffers.Buffer;
   Model_UVs_Buffer     : GL.Objects.Buffers.Buffer;
   Position             : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
   Heading              : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
   Base_Rotation        : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
   Heading_Rotation     : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
   Model_Colour         : GL.Types.Colors.Basic_Color := (0.0, 0.0, 0.0);
   Velocity             : GL.Types.Single := 0.0;
   Radius               : GL.Types.Single := 1.0;
   Is_Ship              : Boolean := False;
   Is_Visible           : Boolean := True;
   Is_Collidable        : Boolean := True;
   end record;

end Model;
