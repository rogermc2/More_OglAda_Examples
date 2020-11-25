
with GL.Types; use GL.Types;

package Cube_Data is

   type Point is new Singles.Vector3;

   type Vertices_Data is new Singles.Vector3_Array (1 .. 24);
   type Tex_Coords is new Singles.Vector2_Array (1 .. 4);

   Vertices : constant Vertices_Data :=
                ( --  front
                    (-1.0, -1.0,  1.0),
                  (1.0, -1.0,  1.0),
                  (1.0,  1.0,  1.0),
                  (-1.0,  1.0,  1.0),
                  --  top
                  (-1.0,  1.0,  1.0),
                  (1.0,  1.0,  1.0),
                  (1.0,  1.0, -1.0),
                  (-1.0,  1.0, -1.0),
                  --  back
                  (1.0, -1.0, -1.0),
                  (-1.0, -1.0, -1.0),
                  (-1.0,  1.0, -1.0),
                  (1.0,  1.0, -1.0),
                  --  bottom
                  (-1.0, -1.0, -1.0),
                  (1.0, -1.0, -1.0),
                  (1.0, -1.0,  1.0),
                  (-1.0, -1.0,  1.0),
                  --  left
                  (-1.0, -1.0, -1.0),
                  (-1.0, -1.0,  1.0),
                  (-1.0,  1.0,  1.0),
                  (-1.0,  1.0, -1.0),
                  --  right
                  (1.0, -1.0,  1.0),
                  (1.0, -1.0, -1.0),
                  (1.0,  1.0, -1.0),
                  (1.0,  1.0,  1.0));

   Texture_Coords : constant Tex_Coords :=
                      ((0.0, 0.0),
                       (1.0, 0.0),
                       (1.0, 1.0),
                       (0.0, 1.0));

   Elements       : constant GL.Types.Int_Array :=
                      (--  front
                       0,  1,  2,
                       2,  3,  0,
                       --  top
                       4,  5,  6,
                       6,  7,  4,
                       --  back
                       8,  9, 10,
                       10, 11,  8,
                       --  bottom
                       12, 13, 14,
                       14, 15, 12,
                       --  left
                       16, 17, 18,
                       18, 19, 16,
                       --  right
                       20, 21, 22,
                       22, 23, 20);

end Cube_Data;
