
with GL.Types; use GL.Types;

package Graph_Data is

   type Point is new Singles.Vector2;

   type Point_Data is new Singles.Vector2_Array (1 .. 10);


   Points : constant Point_Data
     := (
         (1.4, 0.0),            (1.4, -0.784, 2.4),
         (0.784, -1.4),         (0.0, -1.4, 2.4),
         (1.3375, 0.05),     (1.3375, -0.749),
         (0.749, -1.3375, 2.53125),  (0.0, -1.3375),
         (1.4375, 0.0),     (1.4375, -0.805)
      );

end Graph_Data;
