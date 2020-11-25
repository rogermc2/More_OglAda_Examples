
with GL.Objects.Buffers;
with GL.Types;

with Graph_Data;

package Buffers is

    procedure Create_Vertex_Buffer (VBO : in out GL.Objects.Buffers.Buffer;
                                    Vertices : Graph_Data.Point_Data);
 end Buffers;
