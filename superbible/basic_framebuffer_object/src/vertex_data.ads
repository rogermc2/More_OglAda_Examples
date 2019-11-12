

with  Interfaces.C.Pointers;
with GL.Types;

package Vertex_Data is

   type Point_Coord is new GL.Types.Single;
   type Point_Index is new GL.Types.Int;
   type Points_Array is array (Point_Index range <>) of aliased Point_Coord;
   type Index_Array is array (Point_Index range <>) of aliased Point_Index;

   Default_Point_Terminator : Point_Coord := 0.0;
   Default_Index_Terminator : Point_Index := 0;

    package pVertex_Pointers is new Interfaces.C.Pointers (Point_Index, Point_Coord, Points_Array,
                                                   Default_Point_Terminator);
    package pIndex_Pointers is new Interfaces.C.Pointers (Point_Index, Point_Index, Index_Array,
                                                   Default_Index_Terminator);

    Vertex_Indices : Index_Array (1 .. 36) :=
                       (0, 1, 2,
                        2, 1, 3,
                        2, 3, 4,
                        4, 3, 5,
                        4, 5, 6,
                        6, 5, 7,
                        6, 7, 0,
                        0, 7, 1,
                        6, 0, 2,
                        2, 4, 6,
                        7, 5, 3,
                        7, 3, 1);

    Vertices_With_Tex : Points_Array (1 .. 36 * 5) :=
         --  Position                 Texture Coord
           (-0.25, -0.25,  0.25,      0.0, 1.0,
            -0.25, -0.25, -0.25,      0.0, 0.0,
             0.25, -0.25, -0.25,      1.0, 0.0,

             0.25, -0.25, -0.25,      1.0, 0.0,
             0.25, -0.25,  0.25,      1.0, 1.0,
            -0.25, -0.25,  0.25,      0.0, 1.0,

             0.25, -0.25, -0.25,      0.0, 0.0,
             0.25,  0.25, -0.25,      1.0, 0.0,
             0.25, -0.25,  0.25,      0.0, 1.0,

             0.25,  0.25, -0.25,      1.0, 0.0,
             0.25,  0.25,  0.25,      1.0, 1.0,
             0.25, -0.25,  0.25,      0.0, 1.0,

             0.25,  0.25, -0.25,      1.0, 0.0,
            -0.25,  0.25, -0.25,      0.0, 0.0,
             0.25,  0.25,  0.25,      1.0, 1.0,

            -0.25,  0.25, -0.25,      0.0, 0.0,
            -0.25,  0.25,  0.25,      0.0, 1.0,
             0.25,  0.25,  0.25,      1.0, 1.0,

            -0.25,  0.25, -0.25,      1.0, 0.0,
            -0.25, -0.25, -0.25,      0.0, 0.0,
            -0.25,  0.25,  0.25,      1.0, 1.0,

            -0.25, -0.25, -0.25,      0.0, 0.0,
            -0.25, -0.25,  0.25,      0.0, 1.0,
            -0.25,  0.25,  0.25,      1.0, 1.0,

            -0.25,  0.25, -0.25,      0.0, 1.0,
             0.25,  0.25, -0.25,      1.0, 1.0,
             0.25, -0.25, -0.25,      1.0, 0.0,

             0.25, -0.25, -0.25,      1.0, 0.0,
            -0.25, -0.25, -0.25,      0.0, 0.0,
            -0.25,  0.25, -0.25,      0.0, 1.0,

            -0.25, -0.25,  0.25,      0.0, 0.0,
             0.25, -0.25,  0.25,      1.0, 0.0,
             0.25,  0.25,  0.25,      1.0, 1.0,

             0.25,  0.25,  0.25,      1.0, 1.0,
            -0.25,  0.25,  0.25,      0.0, 1.0,
            -0.25, -0.25,  0.25,      0.0, 0.0);

end Vertex_Data;
