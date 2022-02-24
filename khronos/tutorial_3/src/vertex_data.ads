
with GL.Types;

package Vertex_Data is
    use  GL.Types;

    Tetrahedron_Vertices : Singles.Vector3_Array (1 .. 12)
                                --  Face 1
      :=  ((-1.0, 1.0, -1.0),   --  Vertex 1
           (1.0, -1.0, -1.0),   --  Vertex 1
           (-1.0, -1.0, 1.0),   --  Vertex 3
                                --  Face 2
           (1.0, 1.0, 1.0),     --  Vertex 1
           (-1.0, -1.0, 1.0),   --  Vertex 2
           (1.0, -1.0, -1.0),   --  Vertex 3
                                --  Face 3
           (1.0, 1.0, 1.0),     --  Vertex 1
           (-1.0, 1.0, -1.0),   --  Vertex 2
           (-1.0, -1.0, 1.0),   --  Vertex 3
                                --  Face 4
           (1.0, 1.0, 1.0),     --  Vertex 1
           (1.0, -1.0, -1.0),   --  Vertex 2
           (-1.0, 1.0, -1.0));  --  Vertex 3

    Colour_Data : Singles.Vector3_Array (1 .. 4)
      :=  ((1.0, 0.0, 0.0),  -- Red
           (0.0, 1.0, 0.0),  -- Green
           (0.0, 0.0, 1.0),  -- Blue
           (1.0, 1.0, 1.0)); -- White
end Vertex_Data;
