
with GL.Types;

with Teapot_Data;

package Pascal_Teapot is
    use GL.Types;

   Patch_Size : constant Int := Teapot_Data.Patchs'Length;

   type Vertices_Array is new
     Singles.Vector3_Array (1 .. Patch_Size);
   type Colours_Array is new
     Single_Array (1 .. 3 * Teapot_Data.Num_Patchs);
   type CP_Colours_Array is array
     (Int range 1 .. 3 * Teapot_Data.Num_Control_Points) of GL.Types.Single;

   procedure Build_CP_Colours (CP_Colours : out CP_Colours_Array);
   function Build_Teapot (Patchs : Teapot_Data.Patch_Data; Num_Steps : Int)
                          return Singles.Vector3_Array;
end Pascal_Teapot;
