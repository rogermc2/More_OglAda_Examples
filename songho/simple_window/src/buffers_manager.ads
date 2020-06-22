
with GL.Objects.Buffers;
with GL.Types;

package Buffers_Manager is
    use GL.Types.Singles;

    type Vertex is private;
    type Vertices_Array is array (GL.Types.UInt range <>) of Vertex;

    procedure Create_Index_Buffer
      (IBO_1 : in out GL.Objects.Buffers.Buffer);
    procedure Create_Vertex_Buffer
      (VBO_1 : in out GL.Objects.Buffers.Buffer);
    procedure Load_Vertex_Buffer (Vertex_Buffer : GL.Objects.Buffers.Buffer);

private
    type Vertex is record
        Position : Vector3;
        Texture  : Vector2;
    end record;

end Buffers_Manager;
