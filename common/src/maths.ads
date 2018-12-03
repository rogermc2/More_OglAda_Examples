
with Interfaces.C.Pointers;

with Ada.Numerics.Generic_Elementary_Functions;

with GL.Types; use GL.Types;

package Maths is

   type Degree is new Single;
   type Radian is new Single;

   type Index_5 is (X, Y, Z, U, V);
   type Index_6 is (X, Y, Z, R, G, B);
   type Index_8 is (X, Y, Z, U, V, NX, NY, NZ);

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

   package Vector5_Pointers is new Interfaces.C.Pointers
     (Size, Vector5, Vector5_Array, Vector5'(others => <>));
   package Vector6_Pointers is new Interfaces.C.Pointers
     (Size, Vector6, Vector6_Array, Vector6'(others => <>));
   package Vector8_Pointers is new Interfaces.C.Pointers
     (Size, Vector8, Vector8_Array, Vector8'(others => <>));

   Math_Exception : Exception;

   function Cube_Root (Value : Single) return Single;
   function Degrees (Angle : Radian) return Degree;
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
   function Normalized (V : Singles.Vector3) return Singles.Vector3;
   function Normalized (V : Singles.Vector4) return Singles.Vector4;
   function Perspective_Matrix (View_Angle : Degree; Aspect, Near, Far : Single)
                                return Singles.Matrix4;
   function Perspective_Matrix (Top, Bottom, Left, Right, Near, Far : Single)
                                return GL.Types.Singles.Matrix4;
   function Radians (Angle : Degree) return Radian;
   procedure Rotate (Vec : in out GL.Types.Singles.Vector3;
                     Angle : Degree; Axis : GL.Types.Singles.Vector3);
   function Rotation_Matrix (Angle : Degree; Axis : Singles.Vector3)
                             return Singles.Matrix4;
   function Rotation_Matrix (Angle : Radian; Axis : Singles.Vector3)
                             return Singles.Matrix4;
   function Scaling_Matrix (Scale_Factor : Single) return Singles.Matrix4;
   function Scaling_Matrix (Scale_Factor : Singles.Vector3) return Singles.Matrix4;
   function Translation_Matrix (Change : Singles.Vector3)
                                return Singles.Matrix4;
end Maths;
