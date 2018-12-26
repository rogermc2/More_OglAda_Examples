
with GL.Types;

with Patch_Data;

package Surface_Patch is

   procedure Build_Patch (thePatch : Patch_Data.Patch_Type; Num_Steps : GL.Types.Int;
                          Patch_Array : out GL.Types.Singles.Vector3_Array);
end Surface_Patch;
