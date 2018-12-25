
with GL.Types; use GL.Types;

package Patch_Data is

   Spline_Degree      : constant Int := 3;
   Num_Control_Points : constant Int := 16;

   subtype CP_Index is Int range 1 .. Num_Control_Points;
   type Patch_Type is array
     (1 .. Spline_Degree + 1, 1 .. Spline_Degree + 1) of CP_Index;

   Patch : constant Patch_Type
     := -- rim
       ((1, 2, 3, 4), (5, 6, 7 , 8), (9, 10, 11, 12), (13, 14, 15, 16));

   Control_Points : constant Singles.Vector3_Array (1 .. Num_Control_Points)
     := ((1.4, 0.0, 2.4),            (1.4, -0.784, 2.4),
         (0.784, -1.4, 2.4),         (0.0, -1.4, 2.4),

         (1.3375, 0.0, 2.53125),     (1.3375, -0.749, 2.53125),
         (0.749, -1.3375, 2.53125),  (0.0, -1.3375, 2.53125),

         (1.4375, 0.0, 2.53125),     (1.4375, -0.805, 2.53125),
         (0.805, -1.4375, 2.53125),  (0.0, -1.4375, 2.53125),

         (1.5, 0.0, 2.4),            (1.5, -0.84, 2.4),
         (0.84, -1.5, 2.4),          (0.0, -1.5, 2.4));

end Patch_Data;
