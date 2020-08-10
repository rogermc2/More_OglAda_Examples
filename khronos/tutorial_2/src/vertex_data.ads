
with GL.Types;

package Vertex_Data is
    use  GL.Types;

    Diamond_Vertices : Singles.Vector2_Array (1 .. 4)
      :=  ((0.0, 1.0),    --  Top
           (1.0, 0.0),    --  Right
           (0.0, -1.0),   --  Bottom
           (-1.0, 0.0));  --  Left

    Colour_Data : Singles.Vector3_Array (1 .. 4)
      :=  ((1.0, 0.0, 0.0),  -- Red
           (0.0, 1.0, 0.0),  -- Green
           (0.0, 0.0, 1.0),  -- Blue
           (1.0, 1.0, 1.0)); -- White
end Vertex_Data;
