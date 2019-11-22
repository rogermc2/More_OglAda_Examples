
with Maths;
with Utilities;

package body Buffers_20 is

    Stride : GL.Types.Int;

    procedure Create_Vertex_Buffer (VBO : in out GL.Objects.Buffers.Buffer;
                                   Field_Depth, Field_Width : Gl.Types.Single) is
    use GL.Objects.Buffers;
    use GL.Types;

        Vertices : constant Maths.Vector8_Array :=
         ((0.0, 0.0, 0.0,                   0.0, 0.0,  0.0, 1.0, 0.0),
          (0.0, 0.0, Field_Depth,          0.0, 1.0,  0.0, 1.0, 0.0),
          (Field_Width, 0.0, 0.0,          1.0, 0.0,  0.0, 1.0, 0.0),

          (Field_Width, 0.0, 0.0,          1.0, 0.0,  0.0, 1.0, 0.0),
          (0.0,  0.0, Field_Depth,         0.0, 1.0,  0.0, 1.0, 0.0),
          (Field_Width, 0.0, Field_Depth,  1.0, 1.0,  0.0, 1.0, 0.0));

    begin
        VBO.Initialize_Id;
        Array_Buffer.Bind (VBO);
        Utilities.Load_Vector8_Buffer (Array_Buffer, Vertices, Static_Draw);
        Stride := Maths.Vector8'Size / Single'Size;
    end Create_Vertex_Buffer;

    --  ------------------------------------------------------------------------

    function Vertex_Buffer_Stride return GL.Types.Int is
    begin
      return Stride;
    end Vertex_Buffer_Stride;

    --  ------------------------------------------------------------------------

 end Buffers_20;
