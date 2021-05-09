
with GL.Types;

package Vertex_Data is
   use  GL.Types;

   Quad_Vertices : constant GL.Types.Singles.Vector2_Array (1 .. 6) :=
                     ((0.0, 0.5),   --  top left
                      (0.0, 0.0),   --  bottom left
                      (0.5, 0.0),   --  bottom right
                      (0.5, 0.0),   --  bottom right
                      (0.5, 0.5),   --  top right
                      (0.0, 0.5));  --  top left

   Texture_Coords : constant Singles.Vector2_Array (1 .. 6) :=
                      ((0.0, 1.0),
                       (0.0, 0.0),
                       (1.0, 0.0),
                       (1.0, 0.0),
                       (1.0, 1.0),
                       (0.0, 1.0));

end Vertex_Data;
