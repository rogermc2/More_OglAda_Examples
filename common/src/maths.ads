
with Interfaces.C.Pointers;

with Ada.Numerics.Generic_Elementary_Functions;

with GL.Types; use GL.Types;

with Quaternions;

package Maths is

   type Degree is new Single;
   type Radian is new Single;

   type Index_4 is (X, Y, U, V);
   type Index_5 is (X, Y, Z, U, V);
   type Index_6 is (X, Y, Z, R, G, B);
   type Index_8 is (X, Y, Z, U, V, NX, NY, NZ);

   type Vector4 is array (Index_4) of aliased Single;
   pragma Convention (C, Vector4);
   type Vector5 is array (Index_5) of aliased Single;
   pragma Convention (C, Vector5);
   type Vector6 is array (Index_6) of aliased Single;
   pragma Convention (C, Vector6);
   type Vector8 is array (Index_8) of aliased Single;
   pragma Convention (C, Vector8);

   type Vector5_Array is array (Size range <>) of aliased Vector5;
   pragma Convention (C, Vector5_Array);
   type Vector6_Array is array (Size range <>) of aliased Vector6;
   pragma Convention (C, Vector6_Array);
   type Vector8_Array is array (Size range <>) of aliased Vector8;
   pragma Convention (C, Vector8_Array);

   package Single_Math_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (GL.Types.Single);

   package Single_Quaternion is new Quaternions (GL.Types.Single);

   package Vector5_Pointers is new Interfaces.C.Pointers
     (Size, Vector5, Vector5_Array, Vector5'(others => <>));
   package Vector6_Pointers is new Interfaces.C.Pointers
     (Size, Vector6, Vector6_Array, Vector6'(others => <>));
   package Vector8_Pointers is new Interfaces.C.Pointers
     (Size, Vector8, Vector8_Array, Vector8'(others => <>));

   Vec2_0 : constant Singles.Vector2 := (0.0, 0.0);
   Vec3_0 : constant Singles.Vector3 := (0.0, 0.0, 0.0);
   Vec4_0 : constant Singles.Vector4 := (0.0, 0.0, 0.0, 0.0);

   Math_Exception : Exception;

   function "=" (Left, Right : Maths.Vector8) return Boolean;
   function Cube_Root (Value : Single) return Single;
   function Factorial (Num : Natural) return Positive;
   function Frustum_Matrix (Left, Right, Bottom, Top, Near, Far : GL.Types.Single)
                            return GL.Types.Singles.Matrix4;
   procedure Init_Lookat_Transform
     (Position, Target, Up : Singles.Vector3; Look_At : out Singles.Matrix4);
   procedure Init_Orthographic_Transform (Top, Bottom, Left, Right,
                                          Z_Near, Z_Far : Single;
                                          Transform     : out Singles.Matrix4);
   procedure Init_Perspective_Transform
     (View_Angle : Degree; Width, Height, Z_Near, Z_Far : Single;
      Transform  : out Singles.Matrix4);
   procedure Init_Rotation_Transform
     (Rotate_X, Rotate_Y, Rotate_Z : Degree; Transform : out Singles.Matrix4);
   procedure Init_Rotation_Transform
     (Rotation_Vec : Singles.Vector3; Transform : out Singles.Matrix4);
   function Length (V : Singles.Vector3) return Single;
   function Min_Int (L, R : GL.Types.Int) return GL.Types.Int;
   function Min (L, R : GL.Types.Single) return GL.Types.Single;
   function Max_Int (L, R : GL.Types.Int) return GL.Types.Int;
   function Max (L, R : GL.Types.Single) return GL.Types.Single;
   function New_Quaternion (Angle : Radian; Axis : GL.Types.Singles.Vector3)
                            return Single_Quaternion.Quaternion;
   function Normalized (V : Singles.Vector3) return Singles.Vector3;
   function Normalized (V : Singles.Vector4) return Singles.Vector4;
   function Perspective_Matrix (View_Angle : Degree; Aspect, Near, Far : Single)
                                return Singles.Matrix4;
   function Perspective_Matrix (Top, Bottom, Left, Right, Near, Far : Single)
                                return GL.Types.Singles.Matrix4;
   function Random_Float return Single;
   function Random_Vector (Min_Magnitude, Max_Magnitude : Single)
                           return Singles.Vector3;
   procedure Rotate (Vec : in out GL.Types.Singles.Vector3;
                     Angle : Degree; Axis : GL.Types.Singles.Vector3);
   function Rotation_Matrix (Angle : Degree; Axis : Singles.Vector3)
                             return Singles.Matrix3;
   function Rotation_Matrix (Angle : Degree; Axis : Singles.Vector3)
                             return Singles.Matrix4;
   function Rotation_Matrix (Angle : Radian; Axis : Singles.Vector3)
                             return Singles.Matrix3;
   function Rotation_Matrix (Angle : Radian; Axis : Singles.Vector3)
                             return Singles.Matrix4;
   function Rotate_X_Degree (M : GL.Types.Singles.Matrix4;
                             Angle : Degree) return GL.Types.Singles.Matrix4;
   function Rotate_Y_Degree (M : GL.Types.Singles.Matrix4;
                             Angle : Degree) return GL.Types.Singles.Matrix4;
   function Rotate_Z_Degree (M : GL.Types.Singles.Matrix4;
                             Angle : Degree) return GL.Types.Singles.Matrix4;
   function Scaling_Matrix (Scale_Factor : Single) return Singles.Matrix4;
   function Scaling_Matrix (Scale_Factor : Singles.Vector3) return Singles.Matrix4;
   function Stride4 return Int;
   function Stride5 return Int;
   function Stride6 return Int;
   function Stride8 return Int;
   function To_Degrees (Angle : Radian) return Degree;
   function To_Radians (Angle : Degree) return Radian;
   function Translation_Matrix (Change : Singles.Vector3)
                                return Singles.Matrix4;
end Maths;
