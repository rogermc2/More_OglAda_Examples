
with GL.Objects.Buffers;
with GL.Types;

package Buffers_20 is
    use GL.Types.Singles;
    type Vertex is record
        Position : Vector3;
        Texture  : Vector2;
    end record;

    type Vertices_Array is array (GL.Types.UInt range <>) of Vertex;

    procedure Create_Vertex_Buffer (VBO : in out GL.Objects.Buffers.Buffer;
                                   Field_Depth, Field_Width : Gl.Types.Single);
    function Vertex_Buffer_Stride return GL.Types.Int;

    --  ------------------------------------------------------------------------

end Buffers_20;
