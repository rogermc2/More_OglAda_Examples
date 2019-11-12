
with GL.Types;

package Vertex_Data is
   use  GL.Types;

   Ground_Vertices : Singles.Vector4_Array (1 .. 4)
     :=  ((-500.0, -50.0, -500.0, 1.0),
          (-500.0, -50.0,  500.0, 1.0),
          (500.0,  -50.0,  500.0, 1.0),
          (500.0,  -50.0, -500.0, 1.0));

   Ground_Normals : Singles.Vector3_Array (1 .. 4)
     :=  ((0.0, 1.0, 0.0),
          (0.0, 1.0, 0.0),
          (0.0, 1.0, 0.0),
          (0.0, 1.0, 0.0));

end Vertex_Data;
