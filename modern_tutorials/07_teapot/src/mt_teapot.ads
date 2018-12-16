
with GL.Types;

with Teapot_Data;

package MT_Teapot is
    use GL.Types;

    Res_U  : constant Int := 10;
    Res_V  : constant Int := 10;

    Patch_Size : constant Int := Teapot_Data.Patchs'Length;
    Element_Array_Size : Int
     := 2 * 3 * Teapot_Data.Patchs'Length * (MT_Teapot.Res_U) * (MT_Teapot.Res_V);
   subtype Element_Array is GL.Types.Int_Array (1 .. Element_Array_Size);

   type CP_Element_Array_Type is array
    (Int range <>, Int range <>, GL.Types.Int range <>) of Int;
   type CP_Element_Array is new CP_Element_Array_Type
     (1 .. Patch_Size, 1 .. Teapot_Data.Bezier_Patch'Length,
      1 .. Teapot_Data.Bezier_Patch'Length (2));

   type Vertices_Array is new
     Singles.Vector3_Array (1 .. Patch_Size * Res_U * Res_V);
   type Colours_Array is new
     Single_Array (1 .. 3 * Teapot_Data.Num_Patchs * Res_U * Res_V);
   type CP_Colours_Array is array
     (Int range 1 .. 3 * Teapot_Data.Num_Vertices) of GL.Types.Single;

    procedure Build_CP_Colours (CP_Colours : out CP_Colours_Array);
    procedure Build_CP_Elements (CP_Elements : out CP_Element_Array);  --  For debugging
    procedure Build_Teapot (Vertices : out Vertices_Array;
                            Colours : out Colours_Array;
                            Elements : out Element_Array);

end MT_Teapot;
