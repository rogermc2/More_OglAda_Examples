
with Ada.Numerics;

with Maths;

package body GL_Maths is

   function Accel_Expand (X : Single; Initial, Final : Vector2) return Single is
      R    : constant Vector2 := Final - Initial;
      X_N  : Single;
      Y    : Single;
   begin
      if Final = Initial then
         raise Maths_Error with
           "Maths.Accel_Expand received equal Final and Initial values.";
      end if;
      X_N := (X - Initial (GL.X)) / R (GL.X);
      Y := 100.0 * (X_N - 1.0) ** 2;
      return R (GL.Y) * Y + Initial (GL.Y);
   end Accel_Expand;

   --  -----------------------------------------------------------

   function Decel_Bounce (X : Single; Initial, Final : Vector2; Num : Integer)
                          return Single is
      use Maths.Single_Math_Functions;
      use Ada.Numerics;
      R      : constant Vector2 := Final - Initial;
      X_N    : Single;
      Y      : Single;
   begin
      if Final = Initial then
         raise Maths_Error with
           "Maths.Decel_Bounce received equal Final and Initial values.";
      end if;

      X_N := (X - Initial (GL.X)) / R (GL.X);
      Y := (1.0 - X_N) * Abs (Sin (X_N * PI * Single (Num)));
      return R (GL.Y) * Y + Initial (GL.Y);
   end Decel_Bounce;

   --  -----------------------------------------------------------

   function Decel_Elastic (X : Single; Initial, Final : Vector2; Num : Integer)
                           return Single is
      use Maths.Single_Math_Functions;
      use Ada.Numerics;
      R    : constant Vector2 := Final - Initial;
      X_N  : Single;
      Y    : Single;
   begin
      if Final = Initial then
         raise Maths_Error with
           "Maths.Decel_Elastic received equal Final and Initial values.";
      end if;

      X_N := (X - Initial (GL.X)) / R (GL.X);
      Y := (1.0 - X_N) * Sin (X_N * PI * Single (Num));
      return R (GL.Y) * Y + Initial (GL.Y);
   end Decel_Elastic;

   --  -----------------------------------------------------------

   function From_Real_Matrix4 (R_Matrix : Single_Matrix)
                               return Singles.Matrix4 is
      GL_Matrix : Singles.Matrix4;
      use GL;
      R_Row     : Integer := 0;
      R_Col     : Integer := 0;
   begin
      for row in Index_Homogeneous'Range loop
         R_Row := R_Row + 1;
         R_Col := 0;
         for col in Index_Homogeneous'Range loop
            R_Col := R_Col + 1;
            GL_Matrix (row, col) := R_Matrix (R_Row, R_Col);
         end loop;
      end loop;
      return GL_Matrix;
   end From_Real_Matrix4;

   --  -----------------------------------------------------------

   function From_Real_Vector3 (R_Vec : Single_Vector) return Singles.Vector3 is
      use GL;
   begin
      return (R_Vec (1), R_Vec (2), R_Vec (3));
   end From_Real_Vector3;

   --  -----------------------------------------------------------

   function From_Real_Vector4 (R_Vec : Single_Vector)
                               return Singles.Vector4 is
      GL_Vec : Singles.Vector4;
      use GL;
   begin
      GL_Vec (X) := R_Vec (1);
      GL_Vec (Y) := R_Vec (2);
      GL_Vec (Z) := R_Vec (3);
      GL_Vec (W) := R_Vec (4);
      return GL_Vec;
   end From_Real_Vector4;

   --  -------------------------------------------------------------------------

   function Lerp (Vec_A, Vec_B : Singles.Vector3; T : Single)
                  return Singles.Vector3 is
      use Singles;
   begin
      return Vec_A * T + Vec_B * (1.0 - T);
   end Lerp;

   --  -------------------------------------------------------------------------

   function Look_At (Camera_Pos, Target_Pos, Up : Singles.Vector3)
                     return Singles.Matrix4 is
      use Singles;
      use Maths;
      -- inverse translation
      P       :  Singles.Matrix4 := Maths.Translation_Matrix (-Camera_Pos);
      -- distance vector
      Dist    : Singles.Vector3 := Target_Pos - Camera_Pos;
      -- forward vector
      Forward : Singles.Vector3 := Maths.Normalized (Dist);
      -- right vector
      Right   : Singles.Vector3 :=
                  Maths.Normalized (Cross_Product (Forward, Up));
      -- real up vector
      Up_New  : Singles.Vector3 :=
                  Maths.Normalized (Cross_Product (Right, Forward));
      Look    : Singles.Matrix4 := Identity4;
      begin
         Look (GL.X, GL.X) := Right (GL.X);     --  r.x
         Look (GL.Y, GL.X) := Right (GL.Y);     --  r.y
         Look (GL.Z, GL.X) := Right (GL.Z);     --  r.z

         Look (GL.X, GL.Y) := Up_New (GL.X);   --  u.x
         Look (GL.Y, GL.Y) := Up_New (GL.Y);   --  u.y
         Look (GL.Z, GL.Y) := Up_New (GL.Z);   --  u.z

         Look (GL.X, GL.Z) := -Forward (GL.X);  --  -f.x;
         Look (GL.Y, GL.Z) := -Forward (GL.Y);  --  -f.y
         Look (GL.Z, GL.Z) := -Forward (GL.Z);  --  -f.z

         return Look * p;
      end Look_At;

      --  -------------------------------------------------------------------------

      function Quat_From_Axis_Degree (Angle : Maths.Degree; X, Y, Z : Single)
                                   return Maths.Single_Quaternion.Quaternion is
      begin
         return Quat_From_Axis_Radian (Maths.To_Radians (Angle), X, Y, Z);
      end Quat_From_Axis_Degree;

      --  -------------------------------------------------------------------------

      function Quat_From_Axis_Radian (Angle : Maths.Radian; X, Y, Z : Single)
                                   return Maths.Single_Quaternion.Quaternion is
         use GL.Types;
         use Maths;
         use Single_Math_Functions;
         Half_Angle : constant Single := Single (Angle / 2.0);
         Quat       : Single_Quaternion.Quaternion;
      begin
         Quat.A := Cos (Half_Angle);
         Quat.B := Sin (Half_Angle) * X;
         Quat.C := Sin (Half_Angle) * Y;
         Quat.D := Sin (Half_Angle) * Z;
         return Quat;
      end Quat_From_Axis_Radian;

      --  -----------------------------------------------------------

      function To_Real_Matrix4 (GL_Matrix : Singles.Matrix4) return Single_Matrix is
         R_Matrix : Single_Matrix (1 .. 4, 1 .. 4);
         use GL;
         R_Row    : Integer := 0;
         R_Col    : Integer := 0;
      begin
         for row in Index_Homogeneous'Range loop
            R_Row := R_Row + 1;
            R_Col := 0;
            for col in Index_Homogeneous'Range loop
               R_Col := R_Col + 1;
               R_Matrix (R_Row, R_Col) := GL_Matrix (row, col);
            end loop;
         end loop;
         return R_Matrix;
      end To_Real_Matrix4;

      --  -----------------------------------------------------------

      function To_Real_Vector3 (GL_Vec : Singles.Vector3) return Single_Vector is
         use GL;
      begin
         return  (GL_Vec (X), GL_Vec (Y), GL_Vec (Z));
      end To_Real_Vector3;

      --  -----------------------------------------------------------

      function To_Real_Vector4 (GL_Vec : Singles.Vector4) return Single_Vector is
         use GL;
      begin
         return (GL_Vec (X), GL_Vec (Y), GL_Vec (Z), GL_Vec (W));
      end To_Real_Vector4;

      --  -----------------------------------------------------------

      function To_Vector2_Array (Vec : Vec2_Package.Vector)
                              return Vector2_Array is
         use Ada.Containers;
         use GL.Types;
         use Vec2_Package;
         Curs      : Cursor := Vec.First;
         Vec_Array : Vector2_Array (0 .. Int (Vec.Length - 1));
      begin
         for index in Int range Vec_Array'Range loop
            Vec_Array (index) := Vec (Curs);
            Next  (Curs);
         end loop;
         return Vec_Array;

      end To_Vector2_Array;

      --  ------------------------------------------------------------------------

      function To_Vector3_Array (Vec : Vec3_Package.List)
                              return Vector3_Array is
         use Ada.Containers;
         use GL.Types;
         use Vec3_Package;
         Curs      : Cursor := Vec.First;
         Vec_Array : Vector3_Array (0 .. Int (Vec.Length - 1));
      begin
         for index in Int range Vec_Array'Range loop
            Vec_Array (index) := Vec (Curs);
            Next  (Curs);
         end loop;
         return Vec_Array;

      end To_Vector3_Array;

      --  ------------------------------------------------------------------------

      function To_Vector4_Array (Vec : Vec4_Package.List)
                              return Vector4_Array is
         use GL.Types;
         use Vec4_Package;
         Curs      : Cursor := Vec.First;
         Vec_Array : Vector4_Array (1 .. Int (Vec.Length));
      begin
         for index in Int range Vec_Array'Range loop
            Vec_Array (index) := Vec (Curs);
            Next  (Curs);
         end loop;
         return Vec_Array;

      end To_Vector4_Array;

      --  ------------------------------------------------------------------------

   end GL_Maths;
