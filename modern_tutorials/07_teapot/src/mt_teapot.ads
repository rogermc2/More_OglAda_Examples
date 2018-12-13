
with GL.Types;

package MT_Teapot is

procedure Draw_Geometry_Solid(Vertices, Nnormals, Text_CS : GL.Types.Singles.Vector3_Array;
                              Num_Vertices : GL.Types.Int;
                              Indices : GL.Types.Int_Array;
                              NumParts, Num_Indices_Per_Part : GL.Types.Int);
end MT_Teapot;
