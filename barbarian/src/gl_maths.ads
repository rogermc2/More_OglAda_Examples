
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Numerics.Generic_Real_Arrays;
with GL.Types;

with Maths;

package GL_Maths is
    use GL.Types;
    use Singles;

    package Ints_Package is new Ada.Containers.Vectors (Positive, Int);
    type Ints_List is new Ints_Package.Vector with null record;

    package Integers_Package is new Ada.Containers.Vectors (Positive, Integer);
    type Integers_List is new Integers_Package.Vector with null record;

    package Singles_Package is new Ada.Containers.Doubly_Linked_Lists
      (Single);
    type Singles_List is new Singles_Package.List with null record;

    package Vec2_Package is new Ada.Containers.Doubly_Linked_Lists
      (Singles.Vector2);
    type Vector2_List is new Vec2_Package.List with null record;

    package Vec3_Package is new Ada.Containers.Doubly_Linked_Lists
      (Singles.Vector3);
    type Vector3_List is new Vec3_Package.List with null record;

    package Vec4_Package is new Ada.Containers.Doubly_Linked_Lists
      (Singles.Vector4);
   type Vector4_List is new Vec4_Package.List with null record;

   package Singles_Array_Package is new Ada.Numerics.Generic_Real_Arrays
     (Single);
   subtype Single_Vector is Singles_Array_Package.Real_Vector;
   subtype Single_Matrix is Singles_Array_Package.Real_Matrix;

   type Character_Array is array (Integer range <>) of Character;
   type Integer_Array is array (Integer range <>) of Integer;

   function Accel_Exp (X : Single; Init, Final : Vector2) return Single;
   function Decel_Bounce (X : Single; Init, Final : Vector2; Num : Integer)
                          return Single;
   function From_Real_Matrix4 (R_Matrix : Single_Matrix) return Singles.Matrix4;
   function From_Real_Vector3 (R_Vec : Single_Vector) return Singles.Vector3;
   function From_Real_Vector4 (R_Vec : Single_Vector) return Singles.Vector4;
   function Quat_From_Axis_Degree (Angle : Maths.Degree; X, Y, Z : Single)
                                   return Maths.Single_Quaternion.Quaternion;
   function Quat_From_Axis_Radian (Angle : Maths.Radian; X, Y, Z : Single)
                                   return Maths.Single_Quaternion.Quaternion;
   function To_Real_Matrix4 (GL_Matrix : Singles.Matrix4) return Single_Matrix;
   function To_Real_Vector3 (GL_Vec : Singles.Vector3) return Single_Vector;
   function To_Real_Vector4 (GL_Vec : Singles.Vector4) return Single_Vector;

end GL_Maths;
