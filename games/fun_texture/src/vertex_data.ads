
with GL.Types;

package Vertex_Data is
   use  GL.Types;

   Quad_Vertices : constant GL.Types.Singles.Vector2_Array (1 .. 6) :=
                     ((0.1, -0.1),
                      (0.1, -0.6),
                      (0.6, -0.6),
                      (0.6, -0.6),
                      (0.6, -0.1),
                      (0.1, -0.1));
   Quad_Colours : constant GL.Types.Singles.Vector3_Array (1 .. 6) :=
                     ((0.0, 1.0, 0.0),
                      (0.0, 1.0, 0.0),
                      (0.0, 1.0, 0.0),
                      (0.0, 1.0, 0.0),
                      (0.0, 1.0, 0.0),
                      (0.0, 1.0, 0.0));


   Texture_Coords : constant Singles.Vector2_Array (1 .. 8) :=
                      ((0.0, 0.0),
                       (0.0, 0.0),
                       (1.0, 0.0),
                       (0.5, 0.0),
                       (1.0, 1.0),
                       (0.5, 0.5),
                       (0.0, 1.0),
                       (0.0, 0.5));


end Vertex_Data;
