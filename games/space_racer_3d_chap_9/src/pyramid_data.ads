
with GL.Types; use GL.Types;

package Pyramid_Data is

    Vertex_Data : Singles.Vector3_Array (1 .. 36) :=
                    ((1.0, 1.0, -1.0),    -- top triangle 1 : begin
                     (-1.0, 1.0, -1.0),
                     (-1.0, 1.0, 1.0),    -- top triangle 1 : end
                     (-1.0, 1.0, 1.0),    -- top triangle 2 : begin
                     (1.0, 1.0, 1.0),
                     (1.0, 1.0, -1.0),      -- top triangle 2 : end

                     (1.0, -1.0, -1.0),     -- Bottom
                     (-1.0, 1.0, -1.0),
                     (-1.0, -1.0, 1.0),
                     (-1.0, -1.0, 1.0),
                     (1.0, -1.0, 1.0),
                     (1.0, -1.0, -1.0),

                     (1.0, 1.0, 1.0),      -- Front
                     (-1.0, 1.0, 1.0),
                     (-1.0, -1.0, 1.0),
                     (-1.0, -1.0, 1.0),
                     (1.0, -1.0, 1.0),
                     (1.0, 1.0, 1.0),

                     (1.0, 1.0, -1.0),     --  Back
                     (-1.0, 1.0, -1.0),
                     (-1.0, -1.0, -1.0),
                     (-1.0, -1.0, -1.0),
                     (1.0, -1.0, -1.0),
                     (1.0, 1.0, -1.0),

                     (-1.0, 1.0, -1.0),      --  Left
                     (-1.0, 1.0, 1.0),
                     (-1.0, -1.0, 1.0),
                     (-1.0, -1.0, 1.0),
                     (-1.0, -1.0, -1.0),
                     (-1.0, 1.0, -1.0),

                     (1.0, 1.0, -1.0),        -- Right
                     (1.0, 1.0, 1.0),
                     (1.0, -1.0, 1.0),
                     (1.0, -1.0, 1.0),
                     (1.0, -1.0, -1.0),
                     (1.0, 1.0, -1.0));

    --  One colour for each vertex.
    Colour_Data : Singles.Vector3_Array (1 .. 36) :=
                    ((0.0,  1.0,  0.0),
                     (0.0,  1.0,  0.0),
                     (0.0,  1.0,  0.0),
                     (0.0,  1.0,  0.0),
                     (0.0,  1.0,  0.0),
                     (0.0,  1.0,  0.0),

                     (1.0,  0.5,  0.0),
                     (1.0,  0.5,  0.0),
                     (1.0,  0.5,  0.0),
                     (1.0,  0.5,  0.0),
                     (1.0,  0.5,  0.0),
                     (1.0,  0.5,  0.0),

                     (1.0,  0.0,  0.0),
                     (1.0,  0.0,  0.0),
                     (1.0,  0.0,  0.0),
                     (1.0,  0.0,  0.0),
                     (1.0,  0.0,  0.0),
                     (1.0,  0.0,  0.0),

                     (1.0,  1.0,  0.0),
                     (1.0,  1.0,  0.0),
                     (1.0,  1.0,  0.0),
                     (1.0,  1.0,  0.0),
                     (1.0,  1.0,  0.0),
                     (1.0,  1.0,  0.0),

                     (0.0,  0.0,  1.0),
                     (0.0,  0.0,  1.0),
                     (0.0,  0.0,  1.0),
                     (0.0,  0.0,  1.0),
                     (0.0,  0.0,  1.0),
                     (0.0,  0.0,  1.0),

                     (1.0,  0.0,  1.0),
                     (1.0,  0.0,  1.0),
                     (1.0,  0.0,  1.0),
                     (1.0,  0.0,  1.0),
                     (1.0,  0.0,  1.0),
                     (1.0,  0.0,  1.0));

end Pyramid_Data;
