
with GL.Objects.Buffers;
with GL.Types;

with MT_Teapot;
with Teapot_Data;

package Buffers is

    procedure Create_Colour_Buffer (Colour_Buffer : in out GL.Objects.Buffers.Buffer;
                                    Colours : MT_Teapot.Colours_Array);
    procedure Create_Elements_Buffer (IBO : in out GL.Objects.Buffers.Buffer;
                                      Indices : GL.Types.Int_Array);
    procedure Create_Vertex_Buffer (VBO : in out GL.Objects.Buffers.Buffer;
                                    Vertices : MT_Teapot.Vertices_Array);
 end Buffers;
