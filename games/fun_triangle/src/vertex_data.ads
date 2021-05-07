
with GL.Types;

package Vertex_Data is
   use  GL.Types;

   Solid_Vertices : constant GL.Types.Singles.Vector2_Array (1 .. 3) :=
                   ((0.1, -0.6),
                    (0.7, -0.6),
                    (0.4, -0.1));
   Solid_Colours : constant GL.Types.Singles.Vector3_Array (1 .. 3) :=
                   ((0.0, 0.0, 1.0),
                    (0.0, 0.0, 1.0),
                    (0.0, 0.0, 1.0));

   Gradient_Vertices : constant GL.Types.Singles.Vector2_Array (1 .. 3) :=
                   ((0.3, -0.4),
                    (0.9, -0.4),
                    (0.6, -0.9));
   Gradient_Colours : constant GL.Types.Singles.Vector3_Array (1 .. 3) :=
                   ((1.0, 0.0, 0.0),
                    (0.0, 1.0, 0.0),
                    (0.0, 0.0, 1.0));

end Vertex_Data;
