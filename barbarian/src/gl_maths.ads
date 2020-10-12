
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with GL.Types;

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

   type Character_Array is array (Integer range <>) of Character;
   type Integer_Array is array (Integer range <>) of Integer;

end GL_Maths;
