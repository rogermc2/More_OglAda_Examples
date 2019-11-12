
with GL.Types; use GL.Types;

with Teapot_Data;

package MT_Teapot is
   use Teapot_Data;

    Res_U  : constant Int := 10;
    Res_V  : constant Int := 10;

--     Patch_Size : constant Int := Patch_Data'Length;
--      Element_Array_Size : Int
--       := 2 * 3 * Teapot_Data.Patch_Data'Length * (MT_Teapot.Res_U) * (MT_Teapot.Res_V);
    Element_Array_Size : Int
     := 2 * 3 * Num_Patches * MT_Teapot.Res_U * MT_Teapot.Res_V;
   subtype Element_Array is GL.Types.Int_Array (1 .. Element_Array_Size);

   type Vertices_Array is new
     Singles.Vector3_Array (1 .. Num_Patches * (Res_U + 1) * (Res_V + 1));
   type Colours_Array is new
     Single_Array (1 .. 3 * Num_Patches * (Res_U + 1) * (Res_V + 1));
   type CP_Colours_Array is new Singles.Vector3_Array
     (Int range 1 .. Num_Control_Points);

    procedure Build_Teapot (Vertices : out Vertices_Array;
                            Colours : out Colours_Array;
                            Elements : out Element_Array);

end MT_Teapot;
