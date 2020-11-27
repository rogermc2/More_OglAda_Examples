
with GL.Types;

package Vertex_Data is
   use  GL.Types;

   Vertices : Singles.Vector4_Array (1 .. 6) :=
   --             positions   texture coords
                ((0.0,   1.0,   0.0, 1.0),  -- first triangle
                 (1.0,   0.0,   1.0, 0.0),
                 (-0.5, -0.5,   0.0, 0.0),

                 (0.0,   1.0,   0.0, 1.0),  -- xecond triangle
                 (1.0,   1.0,   1.0, 1.0),
                 (1.0,   0.0,   1.0, 0.0));

end Vertex_Data;
