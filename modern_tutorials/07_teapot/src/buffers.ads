
with GL.Objects.Buffers;
with GL.Types;

with MT_Teapot;
with Teapot_Data;

package Buffers is

   type CP_Element_Array_Type is array
    (GL.Types.Int range <>, GL.Types.Int range <>, GL.Types.Int range <>) of GL.Types.Int;
   type CP_Element_Array is new CP_Element_Array_Type
     (1 .. Teapot_Data.Num_Patchs, 1 .. Teapot_Data.Order, 1 .. Teapot_Data.Order);

    procedure Create_Colour_Buffer (Colour_Buffer : in out GL.Objects.Buffers.Buffer;
                                    Colours : MT_Teapot.Colours_Array);
    procedure Create_Colour_Buffer (CP_Colour_Buffer : in out GL.Objects.Buffers.Buffer;
                                    CP_Colours : MT_Teapot.Teapot_CP_Colours);
    procedure Create_Elements_Buffer (CP_IBO : in out GL.Objects.Buffers.Buffer;
                                      CP_Indices : CP_Element_Array);
    procedure Create_Elements_Buffer (IBO : in out GL.Objects.Buffers.Buffer;
                                      Indices : GL.Types.Int_Array);
    procedure Create_Vertex_Buffer (VBO : in out GL.Objects.Buffers.Buffer;
                                    Vertices : MT_Teapot.Vertices_Array);
    procedure Create_Vertex_Buffer (CP_VBO : in out GL.Objects.Buffers.Buffer;
                                    CP_Vertices : Teapot_Data.Vertex_Data);
 end Buffers;
