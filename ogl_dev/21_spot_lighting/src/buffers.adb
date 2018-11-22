
with Maths;
with Utilities;

package body Buffers is

    procedure Create_Vertex_Buffer (VBO : in out GL.Objects.Buffers.Buffer) is
    use GL.Objects.Buffers;
    use GL.Types;
    Field_Depth : constant Gl.Types.Single := 20.0;
    Field_Width : constant Gl.Types.Single := 10.0;

        Vertices : constant Maths.Vector8_Array :=
         ((0.0, 0.0, 0.0,                   0.0, 0.0,  0.0, 1.0, 0.0),
           (0.0, 0.0, Field_Depth,          0.0, 1.0,  0.0, 1.0, 0.0),
           (Field_Width, 0.0, 0.0,          1.0, 0.0,  0.0, 1.0, 0.0),
           (Field_Width, 0.0, 0.0,          1.0, 0.0,  0.0, 1.0, 0.0),
           (0.0,  0.0, Field_Depth,         0.0, 1.0,  0.0, 1.0, 0.0),
           (Field_Width,  0.0, Field_Depth, 1.0, 1.0,  0.0, 1.0, 0.0));
    begin
        VBO.Initialize_Id;
        Array_Buffer.Bind (VBO);
        Utilities.Load_Vector8_Buffer (Array_Buffer, Vertices, Static_Draw);
    end Create_Vertex_Buffer;

    --  ------------------------------------------------------------------------

 end Buffers;
