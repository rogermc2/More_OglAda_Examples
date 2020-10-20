
with GL.Types;

with Maths;

package Vertex_Data is
   use  GL.Types;

   Vertices : Maths.Vector5_Array (1 .. 4) :=
       --   positions      texture coords
        ((0.5,  0.5, 0.0,   1.0, 1.0),    --  top right
         (0.5, -0.5, 0.0,   1.0, 0.0),    --  bottom right
        (-0.5, -0.5, 0.0,   0.0, 0.0),    --  bottom let
        (-0.5,  0.5, 0.0,   0.0, 1.0));   --  top let

   Indices  : Int_Array (1 .. 6) := (0, 1, 3,   -- first triangle
                                     1, 2, 3);  -- second triangle

end Vertex_Data;
