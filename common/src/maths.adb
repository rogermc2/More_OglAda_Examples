
with Ada.Numerics;
with Ada.Numerics.Float_Random;

package body Maths is
   use type GL.Types.Singles.Vector3;

   Radians_Per_Degree : constant Radian := Ada.Numerics.Pi / 180.0;
   Degrees_Per_Radian : constant Degree := 180.0 / Ada.Numerics.Pi;

   Zero_Matrix4 : constant GL.Types.Singles.Matrix4 :=
                    (others => (others => 0.0));
   Gen          : Ada.Numerics.Float_Random.Generator;

   --  ------------------------------------------------------------------------

   function "=" (Left, Right : Maths.Vector8) return Boolean is
   begin
      return Left (X) = Right (X) and Left (Y) = Right (Y) and Left (Z) = Right (Z)
        and Left (NX) = Right (NX) and Left (NY) = Right (NY) and Left (NZ) = Right (NZ)
        and Left (U) = Right (U) and Left (V) = Right (V);
   end "=";

   --   ----------------------------------------------------------------------
   generic
      type Index_Type is (<>);
      type Vector_Type is array (Index_Type) of aliased GL.Types.Single;
   function Stride return Int;

   function Stride return Int is
   begin
      return Vector_Type'Size / GL.Types.Single'Size;
   end Stride;

   function Stride_4 is new Stride (Index_4, Vector4);
   function Stride4 return Int is
   begin
      return Stride_4;
   end Stride4;

   function Stride_5 is new Stride (Index_5, Vector5);
   function Stride5 return Int is
   begin
      return Stride_5;
   end Stride5;

   function Stride_6 is new Stride (Index_8, Vector8);
   function Stride6 return Int is
   begin
      return Stride_6;
   end Stride6;

   function Stride_8 is new Stride (Index_8, Vector8);
   function Stride8 return Int is
   begin
      return Stride_8;
   end Stride8;

   --  ------------------------------------------------------------------------

   function Cube_Root (Value : Single) return Single is
   begin
      return Single_Math_Functions.Exp
        (Single_Math_Functions.Log (Value) / 3.0);
   end Cube_Root;

   --  ------------------------------------------------------------------------
   --  Direction_To_Heading converts an un-normalised direction vector's X,Z
   --  components into a heading in degrees
   function Direction_To_Heading (Dir : Singles.Vector3) return Degree is
   begin
      return To_Degrees (Radian ((Single_Math_Functions.Arctan
                         (-Dir (GL.X), -Dir (GL.Z)))));
   end Direction_To_Heading;

   --  ------------------------------------------------------------------------

   function Factorial (Num : Natural) return Positive is
      Result : Natural := Num;
   begin
      if Num = 0 then
         Result := 1;
      else
         Result := Num * Factorial (Num - 1);  --  recursion
      end if;
      return Positive (Result);

   end Factorial;

   --  ------------------------------------------------------------------------
   --  Transpose of standard frustrum matrix
   function Frustum_Matrix (Left, Right, Bottom, Top, Near, Far : GL.Types.Single)
                            return GL.Types.Singles.Matrix4 is
      use GL;
      Frustrum   : Singles.Matrix4 := Singles.Identity4;
      zDelta     : constant Single := (Far - Near);
      Direction  : constant Single := (Right - Left);
      Height     : constant Single := (Top - Bottom);
      Twice_Near : constant Single := 2.0 * Near;
   begin
      Frustrum (X, X) := 2.0 * Near / Direction;
      Frustrum (Z, X) := (Right + Left) / Direction;

      Frustrum (Y, Y) := Twice_Near / Height;
      Frustrum (Z, Y) := (Top + Bottom) / Height;
      Frustrum (Z, Z) := -(Far + Near) / zDelta;
      Frustrum (W, Z) := -Twice_Near * Far / zDelta;
      Frustrum (Z, W) := -1.0;
      return Frustrum;
   end Frustum_Matrix;

   --  ------------------------------------------------------------------------

   function Heading_To_Direction (Heading : Degree) return Singles.Vector3 is
        Rad_Heading : constant Single := Single (To_Radians (Heading));
   begin
        return (-Single_Math_Functions.Sin (Rad_Heading), 0.0,
                -Single_Math_Functions.Cos (Rad_Heading));
   end Heading_To_Direction;

   --  ------------------------------------------------------------------------
   --  Init_Lookat_Transform is derived from
   --  Computer Graphics Using OpenGL, Chapter 7, transpose of equation 7.2
   --  except, (W, W) = 1 (not 0)
   procedure Init_Lookat_Transform
     (Position, Target, Up : Singles.Vector3;
      Look_At              : out GL.Types.Singles.Matrix4) is
      use GL;
      --  Reference co-ordinate frame (u, v, n)
      --  Forward (n): camera axis
      --  Side (u): axis through side of camera, perpendicular to Forward
      --  Up_New (v): vertical axis of camera, perpendicular to Forward and Side
      Forward : Singles.Vector3 := Position - Target; --  n
      Side    : Singles.Vector3
        := GL.Types.Singles.Cross_Product (Up, Forward);       --  u = Up x n
      Up_New  : Singles.Vector3
        := GL.Types.Singles.Cross_Product (Forward, Side);     --  v = n x u
   begin
      Forward := Normalized (Forward);          --  n / |n|
      Side := Normalized (Side);                --  u / |u|
      Up_New := Normalized (Up_New);            --  v / |v|

      Look_At := GL.Types.Singles.Identity4;
      Look_At (X, X) := Side (X);     --  u.x
      Look_At (Y, X) := Side (Y);     --  u.y
      Look_At (Z, X) := Side (Z);     --  u.z

      Look_At (X, Y) := Up_New (X);   --  v.x
      Look_At (Y, Y) := Up_New (Y);   --  v.y
      Look_At (Z, Y) := Up_New (Z);   --  v.z

      Look_At (X, Z) := Forward (X);  --  n.x;
      Look_At (Y, Z) := Forward (Y);  --  n.y
      Look_At (Z, Z) := Forward (Z);  --  n.z

      Look_At (W, X) := -GL.Types.Singles.Dot_Product (Position, Side);
      Look_At (W, Y) := -GL.Types.Singles.Dot_Product (Position, Up_New);
      Look_At (W, Z) := -GL.Types.Singles.Dot_Product (Position, Forward);
   end Init_Lookat_Transform;

   --  ------------------------------------------------------------------------
   --  Init_Orthographic_Transform is derived from
   --  Computer Graphics Using OpenGL, Chapter 7, transpose of equation 7.18

   procedure Init_Orthographic_Transform (Top, Bottom, Left, Right,
                                          Z_Near, Z_Far : Single;
                                          Transform     : out GL.Types.Singles.Matrix4) is
      use GL;
      dX : constant Single := Right - Left;
      dY : constant Single := Top - Bottom;
      dZ : constant Single := Z_Far - Z_Near;
   begin
      Transform := GL.Types.Singles.Identity4;
      Transform (X, X) := 2.0 / dX;
      Transform (W, X) := -(Right + Left) / dX;
      Transform (Y, Y) := 2.0 / dY;
      Transform (W, Y) := -(Top + Bottom) / dY;
      Transform (Z, Z) := 2.0 / dZ;
      Transform (W, Z) := -(Z_Far + Z_Near) / dZ;
   end Init_Orthographic_Transform;

   --  ------------------------------------------------------------------------

   procedure Init_Perspective_Transform (View_Angle                   : Degree;
                                         Width, Height, Z_Near, Z_Far : Single;
                                         Transform                    : out GL.Types.Singles.Matrix4) is
   begin
      Transform := Perspective_Matrix (View_Angle, Width / Height,
                                       Z_Near, Z_Far);
   end Init_Perspective_Transform;

   --  ------------------------------------------------------------------------

   procedure Init_Rotation_Transform
     (Rotate_X, Rotate_Y, Rotate_Z : Degree; Transform : out Singles.Matrix4) is
      use GL;
      use Single_Math_Functions;
      use Singles;
      Rx   : Matrix4 := Singles.Identity4;
      Ry   : Matrix4 := Singles.Identity4;
      Rz   : Matrix4 := Singles.Identity4;
      Xrad : constant Single := Single (To_Radians (Rotate_X));
      Yrad : constant Single := Single (To_Radians (Rotate_Y));
      Zrad : constant Single := Single (To_Radians (Rotate_Z));
   begin
      Rx (Y, Y) := Cos (Xrad);
      Rx (Y, Z) := -Sin (Xrad);
      Rx (Z, Y) := Sin (Xrad);
      Rx (Z, Z) := Cos (Xrad);

      Ry (X, X) := Cos (Yrad);
      Ry (X, Z) := -Sin (Yrad);
      Ry (Z, X) := Sin (Yrad);
      Ry (Z, Z) := Cos (Yrad);

      Rz (X, X) := Cos (Zrad);
      Rz (X, Y) := -Sin (Zrad);
      Rz (Y, X) := Sin (Zrad);
      Rz (Z, Z) := Cos (Zrad);

      Transform := Rz * Ry * Rx;

   end Init_Rotation_Transform;

   --  ------------------------------------------------------------------------

   procedure Init_Rotation_Transform
     (Rotation_Vec : Singles.Vector3; Transform : out Singles.Matrix4) is
   begin
      Init_Rotation_Transform (Degree (Rotation_Vec (GL.X)),
                               Degree (Rotation_Vec (GL.Y)),
                               Degree (Rotation_Vec (GL.Z)), Transform);
   end Init_Rotation_Transform;

   --  ------------------------------------------------------------------------

   function Length (V : GL.Types.Singles.Vector2) return GL.Types.Single is
      use Single_Math_Functions;
   begin
      return Sqrt (Length_Sq (V));
   end Length;

   --  ------------------------------------------------------------------------

   function Length (V : GL.Types.Singles.Vector3) return GL.Types.Single is
      use Single_Math_Functions;
   begin
      return Sqrt (Length_Sq (V));
   end Length;

   --  ------------------------------------------------------------------------

   function Length_Sq (V : GL.Types.Singles.Vector2) return GL.Types.Single is
      use GL;
   begin
      return V (X) * V (X) + V (Y) * V (Y);
   end Length_Sq;

   --  ------------------------------------------------------------------------

   function Length_Sq (V : GL.Types.Singles.Vector3) return GL.Types.Single is
      use GL;
   begin
      return V (X) * V (X) + V (Y) * V (Y) + V (Z) * V (Z);
   end Length_Sq;

   --  ------------------------------------------------------------------------

   function Min_Int (L, R : GL.Types.Int) return GL.Types.Int is
      Result : GL.Types.Int;
   begin
      if L < R then
         Result := L;
      else
         Result := R;
      end if;
      return Result;
   end Min_Int;

   --  ------------------------------------------------------------------------

   function Min_Integer (L, R : Integer) return Integer is
      Result : Integer;
   begin
      if L < R then
         Result := L;
      else
         Result := R;
      end if;
      return Result;
   end Min_Integer;

   --  ------------------------------------------------------------------------

   function Min (L, R : GL.Types.Single) return GL.Types.Single is
      Result : GL.Types.Single;
   begin
      if L < R then
         Result := L;
      else
         Result := R;
      end if;
      return Result;
   end Min;

   --  ------------------------------------------------------------------------

   function Max_Float (L, R : Float) return Float is
      Result : Float;
   begin
      if R > L then
         Result := R;
      else
         Result := L;
      end if;
      return Result;
   end Max_Float;

   --  ------------------------------------------------------------------------

   function Max_Int (L, R : GL.Types.Int) return GL.Types.Int is
      Result : GL.Types.Int;
   begin
      if R > L then
         Result := R;
      else
         Result := L;
      end if;
      return Result;
   end Max_Int;

   --  ------------------------------------------------------------------------

   function Max_Integer (L, R : Integer) return Integer is
      Result : Integer;
   begin
      if L > R then
         Result := L;
      else
         Result := R;
      end if;
      return Result;
   end Max_Integer;

   --  ------------------------------------------------------------------------

   function Max (L, R : GL.Types.Single) return GL.Types.Single is
      Result : GL.Types.Single;
   begin
      if L > R then
         Result := L;
      else
         Result := R;
      end if;
      return Result;
   end Max;

   --  ------------------------------------------------------------------------

   function New_Quaternion (Angle : Radian; Axis : GL.Types.Singles.Vector3)
                            return Single_Quaternion.Quaternion is
      use Maths.Single_Math_Functions;
      Half_Angle  : constant Single := 0.5 * Single (Angle);
      Sine        : constant Single := Sin (Half_Angle);
   begin
      return (Cos (Half_Angle), Axis (GL.X) * Sine,
              Axis (GL.Y) * Sine, Axis (GL.Z) * Sine);
   end New_Quaternion;

   --  ------------------------------------------------------------------------

   function Normalized (V : Singles.Vector2) return Singles.Vector2 is
      use GL;
      L : constant Single := Length (V);
   begin
      if L /= 0.0 then
         return (V (X) / L, V (Y) / L);
      else
         raise Math_Exception with
              "Maths error, attempted to normalize a zero length vector";
         return V;
      end if;
   end Normalized;

   --  ------------------------------------------------------------------------
   function Normalized (V : Singles.Vector3) return Singles.Vector3 is
      use GL;
      L : constant Single := Length (V);
   begin
      if L /= 0.0 then
         return (V (X) / L, V (Y) / L, V (Z) / L);
      else
         raise Math_Exception with
              "Maths error, attempted to normalize a zero length vector";
         return V;
      end if;
   end Normalized;

   --  ------------------------------------------------------------------------

   function Normalized (V : Singles.Vector4) return Singles.Vector4 is
      use GL;
      L : constant Single := Length (Types.Singles.To_Vector3 (V));
   begin
      if L /= 0.0 then
         return (V (X) / L, V (Y) / L, V (Z) / L, V (W));
      else
         raise Math_Exception with "Maths error, attempted to normalize a zero length vector";
         return V;
      end if;
   end Normalized;

   --  ------------------------------------------------------------------------
   --  Perspective_Matrix is derived from Computer Graphics Using OpenGL
   --  Chapter 7, transpose of equation 7.13
   function Perspective_Matrix (Top, Bottom, Left, Right, Near, Far : Single)
                                return GL.Types.Singles.Matrix4 is
      use GL;
      dX         : constant Single := Right - Left;
      dY         : constant Single := Top - Bottom;
      dZ         : constant Single := Far - Near;
      Matrix     : GL.Types.Singles.Matrix4 := Zero_Matrix4;
   begin
      Matrix (X, X) := 2.0 * Near / dX;
      Matrix (Y, Y) := 2.0 * Near / dY;
      Matrix (Z, X) := (Right + Left) / dX;
      Matrix (Z, Y) :=  (Top + Bottom) / dY;
      Matrix (Z, Z) := -(Far + Near) / dZ;
      Matrix (Z, W) := -1.0;
      Matrix (W, Z) := -2.0 * Far * Near / dZ;
      return Matrix;
   end Perspective_Matrix;

   --  ------------------------------------------------------------------------
   --  Perspective_Matrix is derived from Computer Graphics Using OpenGL
   --  Chapter 7, top, bottom, left and right equations following equation 7.13
   function Perspective_Matrix (View_Angle : Degree; Aspect, Near, Far : Single)
                                return GL.Types.Singles.Matrix4 is
      use Single_Math_Functions;

      Top    : Single;
      Bottom : Single;
      Right  : Single;
      Left   : Single;
   begin
      Top := Near * Tan (Single (0.5 * To_Radians (View_Angle)));
      Bottom := -Top;
      Right  := Top * Aspect;
      Left   := -Right;
      return Perspective_Matrix (Top, Bottom, Left, Right, Near, Far);
   end Perspective_Matrix;

   --  ------------------------------------------------------------------------

   function Quaternion_To_Matrix4 (Quat : Single_Quaternion.Quaternion)
                                   return Singles.Matrix4 is
      Norm_Quat : constant Single_Quaternion.Quaternion :=
                    Single_Quaternion.Normalized (Quat);
      W         : constant Single := Norm_Quat.A;
      X         : constant Single := Norm_Quat.B;
      Y         : constant Single := Norm_Quat.C;
      Z         : constant Single := Norm_Quat.D;
      Result    : Singles.Matrix4 := Singles.Identity4;
   begin
      Result (GL.X, GL.X) := 1.0 - 2.0 * (Y ** 2 + Z ** 2);
      Result (GL.X, GL.Y) := 2.0 * (X * Y + W * Z);
      Result (GL.X, GL.Z) := 2.0 * (X * Z - W * Y);

      Result (GL.Y, GL.X) := 2.0 * (X * Y - W * Z);
      Result (GL.Y, GL.Y) := 1.0 - 2.0 * (X ** 2 + Z ** 2);
      Result (GL.Y, GL.Z) := 2.0 * (X * W + Y * Z);

      Result (GL.Z, GL.X) := 2.0 * (X * Z + W * Y);
      Result (GL.Z, GL.Y) := 2.0 * (X * Z - W * X);
      Result (GL.Z, GL.Z) := 1.0 - 2.0 * (X ** 2 + Y ** 2);

      return Result;
   end Quaternion_To_Matrix4;

   --  ------------------------------------------------------------------------

   function Random_Float return Single is
      use Ada.Numerics.Float_Random;
   begin
      return 2.0 * Single (Random (Gen)) - 1.0;
   end Random_Float;

   --  ------------------------------------------------------------------------

   function Random_Vector (Min_Magnitude, Max_Magnitude : Single)
                            return Singles.Vector3 is
      use Ada.Numerics.Float_Random;
      use GL.Types.Singles;
      R_Vector : Vector3 := (2.0 * Single (Random (Gen)) - 1.0,
                             2.0 * Single (Random (Gen)) - 1.0,
                             2.0 * Single (Random (Gen)) - 1.0);
   begin
      R_Vector := Maths.Normalized (R_Vector);
      R_Vector := R_Vector * (Random_Float *
                              (Max_Magnitude - Min_Magnitude) + Min_Magnitude);
      return R_Vector;
   end Random_Vector;

   --  ------------------------------------------------------------------------

   function Rotate_X_Degree (M     : GL.Types.Singles.Matrix4;
                             Angle : Degree) return GL.Types.Singles.Matrix4 is
      use GL.Types.Singles;
   begin
      return Rotation_Matrix (Angle, (1.0, 0.0, 0.0)) * M;
   end Rotate_X_Degree;

   --  ------------------------------------------------------------------------

   function Rotate_Y_Degree (M     : GL.Types.Singles.Matrix4;
                             Angle : Degree) return GL.Types.Singles.Matrix4 is
      use GL.Types.Singles;
   begin
      return Rotation_Matrix (Angle, (0.0, 1.0, 0.0)) * M;
   end Rotate_Y_Degree;

   --  ------------------------------------------------------------------------

   function Rotate_Z_Degree (M     : GL.Types.Singles.Matrix4;
                             Angle : Degree) return GL.Types.Singles.Matrix4 is
      use GL.Types.Singles;
   begin
      return Rotation_Matrix (Angle, (0.0, 0.0, 1.0)) * M;
   end Rotate_Z_Degree;

   --  ------------------------------------------------------------------------
   --  Rotation_Matrix is based on "Quaternians and spatial rotation" by
   --  en.m.wikipedia.org, with the matrix transposed

   function Rotation_Matrix (Angle : Radian; Axis : GL.Types.Singles.Vector3)
                             return GL.Types.Singles.Matrix3 is
      use GL;
      use Single_Quaternion;

      aQuaternion : Single_Quaternion.Quaternion;
      theMatrix   : GL.Types.Singles.Matrix3 := GL.Types.Singles.Identity3;
      NQ          : Single_Quaternion.Quaternion;
   begin
      aQuaternion := New_Quaternion (Angle, Axis);
      NQ := Normalized (aQuaternion);

      theMatrix (X, X) := 1.0 - 2.0 * (NQ.C * NQ.C + NQ.D * NQ.D);
      theMatrix (Y, X) := 2.0 * (NQ.B * NQ.C - NQ.A * NQ.D);
      theMatrix (Z, X) := 2.0 * (NQ.B * NQ.D + NQ.A * NQ.C);

      theMatrix (X, Y) := 2.0 * (NQ.B * NQ.C + NQ.A * NQ.D);
      theMatrix (Y, Y) := 1.0 - 2.0 * (NQ.B * NQ.B + NQ.D * NQ.D);
      theMatrix (Z, Y) := 2.0 * (NQ.C * NQ.D - NQ.A * NQ.B);

      theMatrix (X, Z) := 2.0 * (NQ.B * NQ.D - NQ.A * NQ.C);
      theMatrix (Y, Z) := 2.0 * (NQ.C * NQ.D + NQ.A * NQ.B);
      theMatrix (Z, Z) := 1.0 - 2.0 * (NQ.B * NQ.B + NQ.C * NQ.C);
      return theMatrix;
   end Rotation_Matrix;

   --  ------------------------------------------------------------------------
   --  Rotation_Matrix is based on "Quaternians and spatial rotation" by
   --  en.m.wikipedia.org, with the matrix transposed

   function Rotation_Matrix (Angle : Radian; Axis : GL.Types.Singles.Vector3)
                             return GL.Types.Singles.Matrix4 is
      use GL;
      use Single_Quaternion;

      aQuaternion : Single_Quaternion.Quaternion;
      theMatrix   : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
      NQ          : Single_Quaternion.Quaternion;
   begin
      aQuaternion := New_Quaternion (Angle, Axis);
      NQ := Normalized (aQuaternion);

      theMatrix (X, X) := 1.0 - 2.0 * (NQ.C * NQ.C + NQ.D * NQ.D);
      theMatrix (Y, X) := 2.0 * (NQ.B * NQ.C - NQ.A * NQ.D);
      theMatrix (Z, X) := 2.0 * (NQ.B * NQ.D + NQ.A * NQ.C);

      theMatrix (X, Y) := 2.0 * (NQ.B * NQ.C + NQ.A * NQ.D);
      theMatrix (Y, Y) := 1.0 - 2.0 * (NQ.B * NQ.B + NQ.D * NQ.D);
      theMatrix (Z, Y) := 2.0 * (NQ.C * NQ.D - NQ.A * NQ.B);

      theMatrix (X, Z) := 2.0 * (NQ.B * NQ.D - NQ.A * NQ.C);
      theMatrix (Y, Z) := 2.0 * (NQ.C * NQ.D + NQ.A * NQ.B);
      theMatrix (Z, Z) := 1.0 - 2.0 * (NQ.B * NQ.B + NQ.C * NQ.C);
      return theMatrix;
   end Rotation_Matrix;

   --  ------------------------------------------------------------------------

   function Rotation_Matrix (Angle : Degree; Axis : GL.Types.Singles.Vector3)
                             return GL.Types.Singles.Matrix4 is
   begin
      return Rotation_Matrix (To_Radians (Angle), Axis);
   end Rotation_Matrix;

   --  ------------------------------------------------------------------------

   function Rotation_Matrix (Angle : Degree; Axis : GL.Types.Singles.Vector3)
                             return GL.Types.Singles.Matrix3 is
   begin
      return Rotation_Matrix (To_Radians (Angle), Axis);
   end Rotation_Matrix;

   --  ------------------------------------------------------------------------

   procedure Rotate (Vec   : in out GL.Types.Singles.Vector3;
                     Angle : Degree; Axis : GL.Types.Singles.Vector3) is
      use Single_Quaternion;
      Rotation_Q  : constant Quaternion :=
                      New_Quaternion (To_Radians (Angle), Axis);
      Conjugate_Q : constant Quaternion := Single_Quaternion.Conj (Rotation_Q);
      W           : constant Quaternion := Rotation_Q * Vec * Conjugate_Q;
      W_Vec       : GL.Types.Singles.Vector4;
   begin
      W_Vec := (W.A, W.B, W.C, W.D);
      Vec := GL.Types.Singles.To_Vector3 (W_Vec);
   end Rotate;

   --  ------------------------------------------------------------------------

   --  Scaling_Matrix is derived from Computer Graphics Using OpenGL
   --  Chapter 5, matrix equation (5.25)

   function Scaling_Matrix (Scale_Factor : Singles.Vector3) return Singles.Matrix4 is
      use GL;
      theMatrix  : Singles.Matrix4 := Singles.Identity4;
   begin
      theMatrix (X, X) := Scale_Factor (X);
      theMatrix (Y, Y) := Scale_Factor (Y);
      theMatrix (Z, Z) := Scale_Factor (Z);
      return theMatrix;
   end Scaling_Matrix;

   --  ------------------------------------------------------------------------

   function Scaling_Matrix (Scale_Factor : Single) return Singles.Matrix4 is
   begin
      return Scaling_Matrix ((Scale_Factor, Scale_Factor, Scale_Factor));
   end Scaling_Matrix;

   --  ------------------------------------------------------------------------

   function To_Degrees (Angle : Radian) return Degree is
   begin
      return Degree (Angle) * Degrees_Per_Radian;
   end To_Degrees;

   --  ------------------------------------------------------------------------

   function To_Radians (Angle : Degree) return Radian is
   begin
      return Radian (Angle) * Radians_Per_Degree;
   end To_Radians;

   --  ------------------------------------------------------------------------
   --  Translation_Matrix is derived from Computer Graphics Using OpenGL
   --  Chapter 5, transpose of equation preceding (5.25)

   function Translation_Matrix (Change : Singles.Vector3)
                                return Singles.Matrix4 is
      use GL;
      theMatrix  : Singles.Matrix4 := Singles.Identity4;
   begin
      theMatrix (W, X) := Change (X);
      theMatrix (W, Y) := Change (Y);
      theMatrix (W, Z) := Change (Z);
      return theMatrix;
   end Translation_Matrix;

   --  ------------------------------------------------------------------------

end Maths;
