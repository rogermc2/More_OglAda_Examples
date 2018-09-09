
with Interfaces.C;
with Interfaces.C.Pointers;

package API_Vectors_Matrices is

   API_Max_Colour_Sets    : constant := 8;
   API_Max_Texture_Coords : constant := 8;

   type API_Colour_3D is record
      R : Interfaces.C.C_float;
      G : Interfaces.C.C_float;
      B : Interfaces.C.C_float;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Colour_3D);

   type API_Colour_4D is record
      R : Interfaces.C.C_float;
      G : Interfaces.C.C_float;
      B : Interfaces.C.C_float;
      A : Interfaces.C.C_float;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Colour_4D);

   type API_Colour_4D_Ptr_Array is array (Interfaces.C.unsigned range 1 .. API_Max_Colour_Sets) of access API_Colour_4D;
   pragma Convention (C, API_Colour_4D_Ptr_Array);

   type API_Matrix_4D is record
      A1, A2, A3, A4 : Interfaces.C.C_float;
      B1, B2, B3, B4 : Interfaces.C.C_float;
      C1, C2, C3, C4 : Interfaces.C.C_float;
      D1, D2, D3, D4 : Interfaces.C.C_float;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Matrix_4D);

   type API_Quaternion is record
      W : Interfaces.C.C_float;
      X : Interfaces.C.C_float;
      Y : Interfaces.C.C_float;
      Z : Interfaces.C.C_float;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Quaternion);

  --  It is erroneous to dereference a Pointer that does not designate an aliased Element.
  type API_Vector_2D is record
      X : aliased Interfaces.C.C_float;
      Y : aliased Interfaces.C.C_float;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Vector_2D);

   type API_Vector_3D is record
      X : aliased Interfaces.C.C_float;
      Y : aliased Interfaces.C.C_float;
      Z : aliased Interfaces.C.C_float;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Vector_3D);

   type API_Vector_3D_Array is array
     (Interfaces.C.unsigned range <>) of aliased API_Vector_3D;
   pragma Convention (C, API_Vector_3D_Array);

   package Vector_3D_Array_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Vector_3D, API_Vector_3D_Array,
      API_Vector_3D'(others => <>));
--     subtype Vector_3D_Array_Pointer is Vector_3D_Array_Pointers.Pointer;

   type API_Vector_3D_Ptr_Array is array (Interfaces.C.unsigned range 1 .. API_Max_Texture_Coords)
      of access API_Vector_3D;
   pragma Convention (C, API_Vector_3D_Ptr_Array);

   type API_Colours_3D_Array is array
     (Interfaces.C.unsigned range <>) of aliased API_Colour_3D;
   pragma Convention (C, API_Colours_3D_Array);

   package Colours_3D_Array_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Colour_3D, API_Colours_3D_Array,
      API_Colour_3D'(others => <>));
   subtype Colours_3D_Array_Pointer is Colours_3D_Array_Pointers.Pointer;

   type API_Unsigned_Array is array
     (Interfaces.C.unsigned range <>) of aliased Interfaces.C.unsigned;
   pragma Convention (C, API_Unsigned_Array);

   package Unsigned_Array_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, Interfaces.C.unsigned, API_Unsigned_Array,
      16#7FFFFFFF#);
   subtype Unsigned_Array_Pointer is Unsigned_Array_Pointers.Pointer;

end API_Vectors_Matrices;
