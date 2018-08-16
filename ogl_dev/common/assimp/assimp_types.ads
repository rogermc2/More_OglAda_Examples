
with Interfaces.C;

with GL.Types; use GL.Types;

with API_Vectors_Matrices;

package Assimp_Types is

   type AI_Return is (AI_Return_Out_Of_Memory, AI_Return_Failure,
                      AI_Return_Success, AI_Enforce_Enum_Size);
   Max_Length : constant Interfaces.C.size_t := 1023;
   subtype API_String_Data_Array is Interfaces.C.char_array (0 .. Max_Length);

   type Colors_Array is array (1 .. 8) of access API_Vectors_Matrices.API_Colour_4D;
   pragma Convention (C, Colors_Array);
   type Texture_Coords_Array is array (1 .. 8) of
     access API_Vectors_Matrices.API_Vector_2D;
   pragma Convention (C, Texture_Coords_Array);
   type Unsigned_Array is array (UInt range <>) of access Interfaces.C.unsigned;
   pragma Convention (C, Unsigned_Array);
   type Vector3_Array is array (Int range <>) of access API_Vectors_Matrices.API_Vector_3D;
   pragma Convention (C, Vector3_Array);

   type API_String is record
      Length  : Interfaces.C.size_t := 0;
      Data    : API_String_Data_Array;
   end record;
   pragma Convention (C_Pass_By_Copy, API_String);

private

   for AI_Return use (AI_Return_Out_Of_Memory => -3,
                      AI_Return_Failure       => -1,
                      AI_Return_Success       => 0,
                      AI_Enforce_Enum_Size    => 16#7FFFFFFF#);

end Assimp_Types;
