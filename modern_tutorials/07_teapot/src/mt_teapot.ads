
with GL.Types;

with Teapot_Data;

package MT_Teapot is
    use GL.Types;

    Res_U  : constant GL.Types.Int := 10;
    Res_V  : constant GL.Types.Int := 10;

    type Teapot_Colours is new GL.Types.Single_Array (1 .. 3 * Teapot_Data.Num_Patchs * Res_U * Res_V);
    type Teapot_CP_Elements is
      array (GL.Types.Int range <>, GL.Types.Int range <>, GL.Types.Int range <>) of Int;

    procedure Build_CP_Elements (CP_Elements : out Teapot_CP_Elements);  --  For debugging
    procedure Build_Teapot (Vertices : out Teapot_Data.Vertex_Data;
                            Colours : out Teapot_Colours;
                            Elements : out GL.Types.Int_Array);
end MT_Teapot;
