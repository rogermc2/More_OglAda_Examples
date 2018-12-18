
with GL.Objects.Buffers;
with GL.Types;

with Pascal_Teapot;
with Teapot_Data;

package Buffers is

    procedure Create_Colour_Buffer (Colour_Buffer : in out GL.Objects.Buffers.Buffer;
                                    Colours : Pascal_Teapot.Colours_Array);
    procedure Create_CP_Colour_Buffer (CP_Colour_Buffer : in out GL.Objects.Buffers.Buffer;
                                       CP_Colours : Pascal_Teapot.CP_Colours_Array);
    procedure Create_CP_Elements_Buffer (CP_IBO : in out GL.Objects.Buffers.Buffer;
                                         CP_Indices : Pascal_Teapot.Patch_Element_Array);
    procedure Create_Elements_Buffer (IBO : in out GL.Objects.Buffers.Buffer;
                                      Indices : GL.Types.Int_Array);
    procedure Create_Vertex_Buffer (VBO : in out GL.Objects.Buffers.Buffer;
                                    Vertices : GL.Types.Singles.Vector3_Array);
    procedure Create_CP_Vertex_Buffer (CP_VBO : in out GL.Objects.Buffers.Buffer;
                                       CP_Vertices : Teapot_Data.CP_Data);
 end Buffers;
