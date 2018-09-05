
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;

with Ada.Containers.Doubly_Linked_Lists;

with GL.Types; use GL.Types;

with API_Vectors_Matrices;

package Assimp_Types is

   type API_Return is (API_Return_Out_Of_Memory, API_Return_Failure,
                       API_Return_Success, API_Enforce_Enum_Size);

   Max_Length : constant Interfaces.C.size_t := 1024;
   subtype API_String_Data_Array is char_array (0 .. Max_Length - 1);

   type Colors_Array is array (1 .. 8) of access API_Vectors_Matrices.API_Colour_4D;
   pragma Convention (C, Colors_Array);

   type Texture_Coords_Array is array (1 .. 8) of
     access API_Vectors_Matrices.API_Vector_2D;
   pragma Convention (C, Texture_Coords_Array);

   type Unsigned_Array is array (UInt range <>) of access Interfaces.C.unsigned;
   pragma Convention (C, Unsigned_Array);

   type Vector3_Array is array (UInt range <>) of access API_Vectors_Matrices.API_Vector_3D;
   pragma Convention (C, Vector3_Array);

    --  This declaration has been checked OK for Key data. DON'T CHANGE
   type API_String is record
      Length  : Interfaces.C.size_t := 0;
      Data    : API_String_Data_Array := (others => Interfaces.C.char'Val (0));
   end record;
   pragma Convention (C_Pass_By_Copy, API_String);

   type Raw_Byte_Data is array (UInt range <>) of aliased UByte;
   package Raw_Data_Pointers is new
     Interfaces.C.Pointers (UInt, UByte, Raw_Byte_Data, UByte'Last);
   subtype Data_Pointer is Raw_Data_Pointers.Pointer;

   package Byte_Data_Package is new Ada.Containers.Doubly_Linked_Lists (Ubyte);
   type Byte_Data_List is new Byte_Data_Package.List with null record;

private

   for API_Return use (API_Return_Out_Of_Memory => -3,
                       API_Return_Failure       => -1,
                       API_Return_Success       => 0,
                       API_Enforce_Enum_Size    => 16#7FFFFFFF#);

end Assimp_Types;
