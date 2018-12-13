
with GL.Objects.Buffers;
with GL.Types;

with Teapot_Data;

package Buffers is

   Order : constant GL.Types.Int := 3;
   type CP_Element_Array_Type is array
    (GL.Types.Int range <>, GL.Types.Int range <>, GL.Types.Int range <>) of GL.Types.Int;
   type CP_Element_Array is new CP_Element_Array_Type
     (1 .. Teapot_Data.Num_Patchs, 1 .. Order, 1 .. Order);

    procedure Create_CP_Index_Buffer (IBO : in out GL.Objects.Buffers.Buffer;
                                   Indices : CP_Element_Array);
    procedure Create_Index_Buffer (IBO : in out GL.Objects.Buffers.Buffer;
                                   Indices : GL.Types.Int_Array);
    procedure Create_Vertex_Buffer (VBO : in out GL.Objects.Buffers.Buffer;
                                    Vertices : GL.Types.Singles.Vector3_Array);
 end Buffers;
