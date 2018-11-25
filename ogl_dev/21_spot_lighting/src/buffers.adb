
with Maths;
with Utilities;

package body Buffers is

    procedure Create_Vertex_Buffer (VBO : in out GL.Objects.Buffers.Buffer;
                                   Field_Depth, Field_Width : Gl.Types.Single) is
    use GL.Objects.Buffers;
    use GL.Types;

        Vertices : constant Maths.Vector8_Array :=
         ((0.0, 0.0, 0.0,                   0.0, 0.0,  0.0, 1.0, 0.0),
           (0.0, 0.1, Field_Depth,          0.0, 1.0,  0.0, 1.0, 0.0),
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
