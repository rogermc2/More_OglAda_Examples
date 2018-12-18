
with GL.Types;

with Teapot_Data;

package Pascal_Teapot is
    use GL.Types;

    Res_U  : constant Int := 10;
    Res_V  : constant Int := 10;

    Patch_Size : constant Int := Teapot_Data.Patchs'Length;
    Element_Array_Size : Int
     := 2 * 3 * Teapot_Data.Patchs'Length * (Pascal_Teapot.Res_U) * (Pascal_Teapot.Res_V);
   subtype Element_Array is GL.Types.Int_Array (1 .. Element_Array_Size);

   type CP_Element_Array_Type is array
    (Int range <>, Int range <>) of Int;
   type CP_Element_Array is new CP_Element_Array_Type
     (1 .. Teapot_Data.Bezier_Patch'Length,
      1 .. Teapot_Data.Bezier_Patch'Length (2));

   type Patch_Element_Array is array (Int range 1 .. Teapot_Data.Num_Patchs) of CP_Element_Array;

   type Vertices_Array is new
     Singles.Vector3_Array (1 .. Patch_Size * Res_U * Res_V);
   type Colours_Array is new
     Single_Array (1 .. 3 * Teapot_Data.Num_Patchs * Res_U * Res_V);
   type CP_Colours_Array is array
     (Int range 1 .. 3 * Teapot_Data.Num_Control_Points) of GL.Types.Single;

    procedure Build_CP_Colours (CP_Colours : out CP_Colours_Array);
    procedure Build_CP_Elements (CP_Elements : out Patch_Element_Array);  --  For debugging
   function Build_Teapot (Patchs : Teapot_Data.Patch_Data; Num_Steps : Int)
                          return Singles.Vector3_Array;
end Pascal_Teapot;
