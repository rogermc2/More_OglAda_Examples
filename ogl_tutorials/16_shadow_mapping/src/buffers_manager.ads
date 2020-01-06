
with GL.Objects.Buffers;

with GL.Types;

package Buffers_Manager is

    procedure Load_Buffers (Normals_Buffer : in out GL.Objects.Buffers.Buffer;
                            UVs_Buffer : in out GL.Objects.Buffers.Buffer;
                            Vertex_Buffer : in out GL.Objects.Buffers.Buffer;
                            Element_Buffer : in out GL.Objects.Buffers.Buffer;
                            Indices_Size : out GL.Types.Int);

    --  ------------------------------------------------------------------------

end Buffers_Manager;
