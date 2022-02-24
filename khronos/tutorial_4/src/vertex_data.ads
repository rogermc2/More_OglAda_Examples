
with GL.Types;

package Vertex_Data is
    use  GL.Types;

    Tetrahedron_Vertices : constant Singles.Vector3_Array (1 .. 4)
      :=  ((1.0, 1.0, 1.0),     --  Vertex 0
           (-1.0, -1.0, 1.0),   --  Vertex 1
           (-1.0, 1.0, -1.0),   --  Vertex 2
           (1.0, -1.0, -1.0));  --  Vertex 3

    Tetrahedron_Indices : constant UInt_Array (1 .. 6)
      :=  (0, 1, 2, 3, 0, 1);

    Colour_Data : constant Singles.Vector3_Array (1 .. 4)
      :=  ((1.0, 0.0, 0.0),  -- Red
           (0.0, 1.0, 0.0),  -- Green
           (0.0, 0.0, 1.0),  -- Blue
           (1.0, 1.0, 1.0)); -- White

end Vertex_Data;
