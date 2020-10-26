
with Ada.Numerics;

with Maths;

package body GL_Maths is

   function Accel_Exp (X : Single; Init, Final : Vector2) return Single is
      R    : constant Vector2 := Final - Init;
      N    : constant Single := (X - Init (GL.X)) / R (GL.X);
      Y    : constant Single := 100.0 * (N - 1.0) ** 2;
   begin
      return R (GL.Y) * Y + Init (GL.Y);
   end Accel_Exp;

   --  -----------------------------------------------------------

   function Decel_Bounce (X : Single; Init, Final : Vector2; Num : Integer)
                          return Single is
      use Maths.Single_Math_Functions;
      use Ada.Numerics;
      R    : constant Vector2 := Final - Init;
      N    : constant Single := (X - Init (GL.X)) / R (GL.X);
      Y    : constant Single := (1.0 - N) * Abs (Sin (N * PI * Single (Num)));
   begin
      return R (GL.Y) * Y + Init (GL.Y);
   end Decel_Bounce;

   --  -----------------------------------------------------------
   function From_Real_Matrix4 (R_Matrix : Single_Matrix)
                               return Singles.Matrix4 is
      GL_Matrix : Singles.Matrix4;
      use GL;
   begin
      for row in 1 .. 4 loop
         for col in 1 .. 4 loop
            GL_Matrix (Index_Homogeneous'Enum_Val (row),
                       Index_Homogeneous'Enum_Val (col)) :=
              R_Matrix (row, col);
         end loop;
      end loop;
      return GL_Matrix;
   end From_Real_Matrix4;

   --  -----------------------------------------------------------

   function From_Real_Vector3 (R_Vec : Single_Vector) return Singles.Vector3 is
      GL_Vec : Singles.Vector3;
      use GL;
   begin
      for index in 1 .. 3 loop
         GL_Vec (Index_Homogeneous'Enum_Val (index)) := R_Vec (index);
      end loop;
      return GL_Vec;
   end From_Real_Vector3;

   --  -----------------------------------------------------------

   function From_Real_Vector4 (R_Vec : Single_Vector)
                               return Singles.Vector4 is
      GL_Vec : Singles.Vector4;
      use GL;
   begin
      for index in 1 .. 4 loop
         GL_Vec (Index_Homogeneous'Enum_Val (index)) := R_Vec (index);
      end loop;
      return GL_Vec;
   end From_Real_Vector4;

   --  -----------------------------------------------------------

   function To_Real_Matrix4 (GL_Matrix : Singles.Matrix4) return Single_Matrix is
      R_Matrix : Single_Matrix (1 .. 4, 1 ..4);
      use GL;
   begin
      for row in 1 .. 4 loop
         for col in 1 .. 4 loop
            R_Matrix (row, col) := GL_Matrix (Index_Homogeneous'Enum_Val (row),
                                              Index_Homogeneous'Enum_Val (col));
         end loop;
      end loop;
      return R_Matrix;
   end To_Real_Matrix4;

   --  -----------------------------------------------------------

   function To_Real_Vector3 (GL_Vec : Singles.Vector3) return Single_Vector is
      R_Vec : Single_Vector (1 .. 3);
      use GL;
   begin
      for index in 1 .. 3 loop
         R_Vec (index) := GL_Vec (Index_Homogeneous'Enum_Val (index));
      end loop;
      return R_Vec;
   end To_Real_Vector3;

   --  -----------------------------------------------------------

   function To_Real_Vector4 (GL_Vec : Singles.Vector4) return Single_Vector is
      R_Vec : Single_Vector (1 .. 4);
      use GL;
   begin
      for index in 1 .. 4 loop
         R_Vec (index) := GL_Vec (Index_Homogeneous'Enum_Val (index));
      end loop;
      return R_Vec;
   end To_Real_Vector4;

   --  -----------------------------------------------------------

end GL_Maths;
