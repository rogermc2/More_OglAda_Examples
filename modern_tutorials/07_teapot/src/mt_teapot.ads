
with GL.Types;

with Teapot_Data;

package MT_Teapot is
    use GL.Types;

    Res_U  : constant GL.Types.Int := 10;
    Res_V  : constant GL.Types.Int := 10;
    Element_Array_Size : GL.Types.Int
     := 2 * 3 * Teapot_Data.Num_Patchs * (MT_Teapot.Res_U) * (MT_Teapot.Res_V);
    subtype Element_Array is GL.Types.Int_Array (1 .. Element_Array_Size);

   type Vertices_Array is new
     GL.Types.Singles.Vector3_Array (1 .. Teapot_Data.Num_Patchs * Res_U * Res_V);
   type Colours_Array is new
     GL.Types.Single_Array (1 .. 3 * Teapot_Data.Num_Patchs * Res_U * Res_V);
    type Teapot_CP_Elements is
      array (GL.Types.Int range <>, GL.Types.Int range <>, GL.Types.Int range <>) of Int;
   type Teapot_CP_Colours is array
     (GL.Types.Int range 1 .. 3 * Teapot_Data.Num_Vertices) of GL.Types.Single;

    procedure Build_CP_Elements (CP_Elements : out Teapot_CP_Elements);  --  For debugging
    procedure Build_Teapot (Vertices : out Vertices_Array;
                            Colours : out Colours_Array;
                            Elements : out Element_Array);
end MT_Teapot;
