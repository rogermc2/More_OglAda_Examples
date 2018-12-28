
with GL.Types;

with Teapot_Data;

package Pascal_Teapot is

   procedure Build_Teapot (Num_Steps : GL.Types.Int;
                           theTeapot : out GL.Types.Singles.Vector3_Array);
end Pascal_Teapot;
