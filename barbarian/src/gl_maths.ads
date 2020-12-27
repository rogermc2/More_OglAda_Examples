
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Numerics.Generic_Real_Arrays;
with GL.Types;

with Maths;

package GL_Maths is
    use GL.Types;
    use Singles;

    package Ints_Package is new Ada.Containers.Vectors (Positive, Int);
    subtype Ints_List is Ints_Package.Vector;

    package Integers_Package is new Ada.Containers.Vectors (Positive, Integer);
    subtype Integers_List is Integers_Package.Vector;

    package Singles_Package is new Ada.Containers.Doubly_Linked_Lists
      (Single);
    subtype Singles_List is Singles_Package.List;

    package Vec2_Package is new Ada.Containers.Doubly_Linked_Lists
      (Singles.Vector2);
    subtype Vec2_List is Vec2_Package.List;

    package Vec3_Package is new Ada.Containers.Doubly_Linked_Lists
      (Singles.Vector3);
    subtype Vec3_List is Vec3_Package.List;

    package Vec4_Package is new Ada.Containers.Doubly_Linked_Lists
      (Singles.Vector4);
   subtype Vec4_List is Vec4_Package.List;

   package Singles_Array_Package is new Ada.Numerics.Generic_Real_Arrays
     (Single);
   subtype Single_Vector is Singles_Array_Package.Real_Vector;
   subtype Single_Matrix is Singles_Array_Package.Real_Matrix;

   Maths_Error : Exception;

   type Character_Array is array (Integer range <>) of Character;
   type Integer_Array is array (Integer range <>) of Integer;

   function Accel_Expand (X : Single; Initial, Final : Vector2) return Single;
   function Decel_Bounce (X : Single; Initial, Final : Vector2; Num : Integer)
                          return Single;
   function Decel_Elastic (X : Single; Initial, Final : Vector2; Num : Integer)
                          return Single;
   function From_Real_Matrix4 (R_Matrix : Single_Matrix) return Singles.Matrix4;
   function From_Real_Vector3 (R_Vec : Single_Vector) return Singles.Vector3;
   function From_Real_Vector4 (R_Vec : Single_Vector) return Singles.Vector4;
   function Lerp (Vec_A, Vec_B : Singles.Vector3; T : Single)
                  return Singles.Vector3;
   function Quat_From_Axis_Degree (Angle : Maths.Degree; X, Y, Z : Single)
                                   return Maths.Single_Quaternion.Quaternion;
   function Quat_From_Axis_Radian (Angle : Maths.Radian; X, Y, Z : Single)
                                   return Maths.Single_Quaternion.Quaternion;
   function To_Real_Matrix4 (GL_Matrix : Singles.Matrix4) return Single_Matrix;
   function To_Real_Vector3 (GL_Vec : Singles.Vector3) return Single_Vector;
   function To_Real_Vector4 (GL_Vec : Singles.Vector4) return Single_Vector;
   function To_Vector2_Array (Vec : Vec2_Package.List)
                              return Vector2_Array;
   function To_Vector3_Array (Vec : Vec3_Package.List)
                              return Vector3_Array;
   function To_Vector4_Array (Vec : Vec4_Package.List)
                              return Vector4_Array;
end GL_Maths;
