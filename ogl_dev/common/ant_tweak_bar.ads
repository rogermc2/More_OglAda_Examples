
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package Ant_Tweak_Bar is

   type TW_Bar is private;

   type TW_Type is (Type_Undef, Type_Bool_CPP, Type_Bool_8, Type_Bool_16,
                    Type_Bool_32, Type_Char, Type_Int_8, Type_UInt_8,
                    Type_Int_16, Type_UInt_16, Type_Int_32, Type_UInt_32,
                    Type_Float, Type_Double, Type_Colour_32, Type_Colour_3F,
                    Type_Colour_4F, Type_CD_String, Type_STD_String,
                    Type_Quat_4F, Type_Quat_4D, Type_Dir_3F, Type_Dir_3D);
   pragma Convention (C, TW_Type);

private

   type Tw_Enum_Val is record
      Value : int;
      Label : Strings.chars_ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Tw_Enum_Val);

   type Tw_Struct_Member is record
      Name        : Strings.chars_ptr;
      Struct_Type : TW_Type;
      Offset      : size_t := 0;
      Def_String  : Strings.chars_ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Tw_Struct_Member);

   type TW_Bar is record
      Enum_Val      : Tw_Enum_Val;
      Struct_Member : Tw_Struct_Member;
   end record;
   pragma Convention (C_Pass_By_Copy, TW_Bar);

   for TW_Type use (Type_Undef => 0, Type_Bool_CPP => 1,
                    Type_Bool_8 => 2, Type_Bool_16 => 3,
                    Type_Bool_32 => 4, Type_Char => 5,
                    Type_Int_8 => 6, Type_UInt_8 => 7,
                    Type_Int_16 => 8, Type_UInt_16 => 9,
                    Type_Int_32 => 10, Type_UInt_32 => 11,
                    Type_Float => 12, Type_Double => 13,
                    Type_Colour_32 => 14, Type_Colour_3F => 15,
                    Type_Colour_4F => 16, Type_CD_String => 17,
                    Type_STD_String => 18,
                    Type_Quat_4F => 19, Type_Quat_4D => 20,
                    Type_Dir_3F => 21, Type_Dir_3D => 22);
end Ant_Tweak_Bar;
