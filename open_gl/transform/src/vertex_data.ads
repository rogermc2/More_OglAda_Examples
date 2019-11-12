
with GL.Types;
with Maths;

package Vertex_Data is
   use  GL.Types;

   Vertices  : constant Maths.Vector8_Array (1 .. 4) :=
                 --  Position        Color             Texcoords
                 ((-0.5, 0.5, 1.0,  0.0, 0.0, 0.0,  0.0, 0.0),   -- Top-left
                  (0.5, 0.5, 0.0,   1.0, 0.0, 1.0,  0.0, 0.0),   --  Top-right
                  (0.5, -0.5, 0.0,  0.0, 1.0, 1.0,  1.0, 1.0),   --  Bottom-right
                  (-0.5, -0.5, 1.0, 1.0, 1.0, 0.0,  1.0, 1.0));  --  Bottom-left);

   Elements : constant Int_Array (1 .. 6) :=
                (0, 1, 2,
                 2, 3, 0);

end Vertex_Data;
