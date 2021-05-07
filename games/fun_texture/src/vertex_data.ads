
with GL.Types;

package Vertex_Data is
   use  GL.Types;

   Quad_Vertices : constant GL.Types.Singles.Vector2_Array (1 .. 6) :=
                     ((0.1, -0.1),   --  top left
                      (0.1, -0.6),   --  bottom left
                      (0.6, -0.6),   --  bottom right
                      (0.6, -0.6),   --  bottom right
                      (0.6, -0.1),   --  top right
                      (0.1, -0.1));  --  top left

   Texture_Coords : constant Singles.Vector2_Array (1 .. 6) :=
                      ((0.0, 0.0),
                       (0.0, 1.0),
                       (1.0, 1.0),
                       (1.0, 1.0),
                       (1.0, 0.0),
                       (0.0, 0.0));


end Vertex_Data;
