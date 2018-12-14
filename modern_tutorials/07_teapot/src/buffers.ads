
with GL.Objects.Buffers;
with GL.Types;

with Teapot_Data;

package Buffers is

   type CP_Element_Array_Type is array
    (GL.Types.Int range <>, GL.Types.Int range <>, GL.Types.Int range <>) of GL.Types.Int;
   type CP_Element_Array is new CP_Element_Array_Type
     (1 .. Teapot_Data.Num_Patchs, 1 .. Teapot_Data.Order, 1 .. Teapot_Data.Order);

    procedure Create_Colour_Buffer (Colour_Buffer : in out GL.Objects.Buffers.Buffer;
                                    Colours : GL.Types.Singles.Vector3_Array);
    procedure Create_CP_Colour_Buffer (Colour_Buffer : in out GL.Objects.Buffers.Buffer;
                                       Colours : GL.Types.Singles.Vector3_Array);
    procedure Create_CP_Elements_Buffer (IBO : in out GL.Objects.Buffers.Buffer;
                                         Indices : CP_Element_Array);
    procedure Create_CP_Vertex_Buffer (Vertex_Buffer : in out GL.Objects.Buffers.Buffer;
                                       Vertices : GL.Types.Singles.Vector3_Array);
    procedure Create_Elements_Buffer (IBO : in out GL.Objects.Buffers.Buffer;
                                      Indices : GL.Types.Int_Array);
    procedure Create_Vertex_Buffer (VBO : in out GL.Objects.Buffers.Buffer;
                                    Vertices : GL.Types.Singles.Vector3_Array);
 end Buffers;
