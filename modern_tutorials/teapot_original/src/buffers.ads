
with GL.Objects.Buffers;
with GL.Types;

with Pascal_Teapot;
with Teapot_Data;

package Buffers is
    procedure Create_Vertex_Buffer (VBO : in out GL.Objects.Buffers.Buffer;
                                    Vertices : GL.Types.Singles.Vector3_Array);
 end Buffers;
