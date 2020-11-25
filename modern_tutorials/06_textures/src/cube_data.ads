
with GL.Types; use GL.Types;

package Cube_Data is

   type Point is new Singles.Vector3;

   type Vertices_Data is new Singles.Vector3_Array (1 .. 24);
   type Tex_Coords is new Singles.Vector2_Array (1 .. 4);

   Cube_Vertces : constant Vertices_Data
     := ( --  front
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

   Cube_Texture_Coords : constant Tex_Coords
     := ((0.0, 0.0),
         (1.0, 0.0),
         (1.0, 1.0),
         (0.0, 1.0));

end Cube_Data;
