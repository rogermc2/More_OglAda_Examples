
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;

with Ada.Containers.Indefinite_Ordered_Maps;

with GL.Types; use GL.Types;

package Assimp_Texture is

   type AI_Texel is private;
   type AI_Texel_Map is private;
   type AI_Texture_Map is private;
   subtype ACH_Format_Hint_Array is char_array (1 .. 4);

   type API_Texel is record
      B  : Interfaces.C.unsigned_char := 0;
      G  : Interfaces.C.unsigned_char := 0;
      R  : Interfaces.C.unsigned_char := 0;
      A  : Interfaces.C.unsigned_char := 0;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Texel);

   type API_Texel_Array is
     array (Interfaces.C.unsigned range <>) of aliased API_Texel;
   pragma Convention (C, API_Texel_Array);

   package Texel_Array_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Texel, API_Texel_Array, API_Texel'(others => <>));

   type API_Texture is record
      Width           : Interfaces.C.unsigned := 0;
      Height          : Interfaces.C.unsigned := 0;
      Ach_Format_Hint : ACH_Format_Hint_Array;
      PC_Data_Ptr     : Texel_Array_Pointers.Pointer;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Texture);

   type API_Texture_Array is array
     (Interfaces.C.unsigned range <>) of aliased API_Texture;
   pragma Convention (C, API_Texture_Array);

   package Texture_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Texture, API_Texture_Array, API_Texture'(others => <>));

   function To_AI_Texture_Map (Num_Textures : Interfaces.C.unsigned := 0;
                               C_Array : API_Texture_Array)
                               return AI_Texture_Map;
private

   type AI_Texel is record
      B  : GL.Types.UInt := 0;
      G  : GL.Types.UInt := 0;
      R  : GL.Types.UInt := 0;
      A  : GL.Types.UInt := 0;
   end record;

   package AI_Texel_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, AI_Texel);
   type AI_Texel_Map is new AI_Texel_Package.Map with null Record;

   type AI_Texture is record
      Width       : GL.Types.UInt := 0;
      Height      : GL.Types.UInt := 0;
      Format_Hint : String (1 .. 4);
      PC_Data     : AI_Texel_Map;
   end record;

   package AI_Texture_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, AI_Texture);
   type AI_Texture_Map is new AI_Texture_Package.Map with null Record;

end Assimp_Texture;
