
--  with Ada.Containers.Doubly_Linked_Lists;

with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types.Colors;

package Model is

    type Model_Data is private;

    procedure Bind_Element_VBO (aModel : in out Model_Data);
    procedure Bind_Model_VAO (aModel : in out Model_Data);
    procedure Bind_Vertex_VBO (aModel : in out Model_Data);
    procedure Initialize (aModel : in out Model_Data; File_Path : String;
                          Colour : GL.Types.Colors.Basic_Color);
    procedure Initialize_VBOs (aModel : in out Model_Data);
    procedure Render (aModel : in out Model_Data);
    procedure Set_Base_Rotation (aModel   : in out Model_Data;
                                 Rotation : GL.Types.Singles.Vector3);
    procedure Set_Is_Ship (aModel : in out Model_Data; State : Boolean);
    procedure Set_Perspective (Projection_Matrix : GL.Types.Singles.Matrix4);
    procedure Set_Position (aModel   : in out Model_Data;
                            Position : GL.Types.Singles.Vector3);
    procedure Set_Velocity (aModel   : in out Model_Data;
                            Velocity : GL.Types.Single);
    procedure Update (aModel : in out Model_Data; Delta_Time : Float);

private
    use GL.Types.Singles;

    type Model_Data is record
        Model_VAO            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        Model_Vertex_Buffer  : GL.Objects.Buffers.Buffer;
        Model_Element_Buffer : GL.Objects.Buffers.Buffer;
        Vertex_Count         : GL.Types.Int := 0;
        Indices_Size         : GL.Types.Int := 0;
        Position             : Vector3 := (0.0, 0.0, 0.0);
        Heading              : Vector3 := (0.0, 0.0, 0.0);
        Base_Rotation        : Vector3 := (0.0, 0.0, 0.0);
        Heading_Rotation     : Vector3 := (0.0, 0.0, 0.0);
        Model_Colour         : GL.Types.Colors.Basic_Color := (0.0, 0.0, 0.0);
        Velocity             : GL.Types.Single := 0.0;
        Radius               : GL.Types.Single := 1.0;
        Is_Ship              : Boolean := False;
        Is_Visible           : Boolean := True;
        Is_Collidable        : Boolean := True;
    end record;

end Model;
