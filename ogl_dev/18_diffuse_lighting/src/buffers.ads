
with GL.Objects.Buffers;

package Buffers is

    procedure Create_Buffers (Vertex_Buffer, UVs_Buffer, Normals_Buffer,
                              IBO : in out GL.Objects.Buffers.Buffer);

    --  ------------------------------------------------------------------------

end Buffers;
