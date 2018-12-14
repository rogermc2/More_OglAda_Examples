
with GL.Types;

with Teapot_Data;

package MT_Teapot is
    use GL.Types;

    Res_U  : constant GL.Types.Int := 10;
    Res_V  : constant GL.Types.Int := 10;
    Res_UV : constant Int := Res_U * Res_V;

    type Teapot_Colours is new GL.Types.Single_Array (1 .. 3 * Teapot_Data.Num_Patchs * Res_UV);

    procedure Build_Vertices (Vertices : out GL.Types.Singles.Vector3_Array;
                              Colours : out Teapot_Colours);
end MT_Teapot;
