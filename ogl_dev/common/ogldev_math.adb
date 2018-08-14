
with Maths;

package body Ogldev_Math is

   function Init_Camera_Transform (Target, Up : Vector3) return Matrix4 is
      use GL;
      use Maths;
      N     : constant Vector3 := Normalized (Target);
      U     : constant Vector3 := Normalized (Cross_Product (Up, N));
      V     : constant Vector3 := Cross_Product (N, U);
      Trans : Matrix4 := Identity4;
   begin
      Trans (X, X) := U (X);
      Trans (X, Y) := U (Y);
      Trans (X, Z) := U (Z);
      Trans (Y, X) := V (X);
      Trans (Y, Y) := V (Y);
      Trans (Y, Z) := V (Z);
      Trans (Z, X) := N (X);
      Trans (Z, Y) := N (Y);
      Trans (Z, Z) := N (Z);
      return Trans;
   end Init_Camera_Transform;

   --  -------------------------------------------------------------------------

   function To_AI_Map3D (Num_Vecs : UInt := 0;
                         Vectors : Assimp_Types.Vector3_Array)
                         return AI_3D_Map is
      Vec_Map : AI_3D_Map;
      Vec     : Singles.Vector3;
   begin
      for index in 1 .. Num_Vecs loop
         Vec (GL.X) := Single (Vectors (Int (index)).X);
         Vec (GL.Y) := Single (Vectors (Int (index)).Y);
         Vec (GL.Z) := Single (Vectors (Int (index)).Z);
         Vec_Map.Insert (index, Vec);
      end loop;
      return Vec_Map;
   end To_AI_Map3D;

   --  -------------------------------------------------------------------------

   function To_GL_Matrix4 (M4 : API_Vectors_Matrices.API_Matrix_4D)
                           return Singles.Matrix4 is
      Mat : Singles.Matrix4;
   begin
      Mat (GL.X, GL.X) := Single (M4.A1);
      Mat (GL.X, GL.Y) := Single (M4.A2);
      Mat (GL.X, GL.Z) := Single (M4.A3);
      Mat (GL.X, GL.W) := Single (M4.A4);

      Mat (GL.Y, GL.X) := Single (M4.B1);
      Mat (GL.Y, GL.Y) := Single (M4.B2);
      Mat (GL.Y, GL.Z) := Single (M4.B3);
      Mat (GL.Y, GL.W) := Single (M4.B4);

      Mat (GL.Z, GL.X) := Single (M4.C1);
      Mat (GL.Z, GL.Y) := Single (M4.C2);
      Mat (GL.Z, GL.Z) := Single (M4.C3);
      Mat (GL.Z, GL.W) := Single (M4.C4);

      Mat (GL.W, GL.X) := Single (M4.D1);
      Mat (GL.W, GL.Y) := Single (M4.D2);
      Mat (GL.W, GL.Z) := Single (M4.D3);
      Mat (GL.W, GL.W) := Single (M4.D4);

      return Mat;
   end To_GL_Matrix4;

end Ogldev_Math;
