
package body MT_Teapot is

procedure Draw_Geometry_Solid (Vertices, Nnormals, Text_CS : GL.Types.Singles.Vector3_Array;
                              Num_Vertices : GL.Types.Int;
                              Indices : GL.Types.Int_Array;
                              NumParts, Num_Indices_Per_Part : GL.Types.Int) is
   begin
      null;
   end Draw_Geometry_Solid;

 procedure Build_control_points (int p, struct vertex control_points_k[][ORDER+1])
   begin
  for (int i = 0; i <= ORDER; i++)
    for (int j = 0; j <= ORDER; j++)
      control_points_k[i][j] = teapot_cp_vertices[teapot_patches[p][i][j] - 1];
   end

end MT_Teapot;
