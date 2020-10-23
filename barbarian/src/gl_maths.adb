
package body GL_Maths is

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
