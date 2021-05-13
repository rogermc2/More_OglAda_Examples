
with GL.Types;

package Vertex_Data is
   use  GL.Types;
   Points_Data : constant GL.Types.Singles.Vector2_Array (1 .. 3) :=
                   ((0.1, -0.6),
                    (0.7, -0.6),
                    (0.4, -0.1));
   Lines_Data  : constant GL.Types.Singles.Vector2_Array (1 .. 4) :=
                   ((0.1, -0.6),
                    (0.7, -0.6),
                    (0.7, -0.6),
                    (0.4, -0.1));

end Vertex_Data;
