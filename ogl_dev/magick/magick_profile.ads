
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Method_Attribute;

package Magick_Profile is

   type Rendering_Intent is (Undefined_Intent,
                             Saturation_Intent,
                             Perceptual_Intent,
                             Absolute_Intent,
                             Relative_Intent);
   pragma Convention (C, Rendering_Intent);

   type Profile_Data is array (unsigned range <>) of unsigned;

   type Profile_Info is record
      Name      : chars_ptr;
      Length    : unsigned;
      Info      : access Profile_Data;
      Signature : size_t :=
        size_t (Method_Attribute.Magick_Core_Signature);
   end record;
   pragma Convention (C_Pass_By_Copy, Profile_Info);

end Magick_Profile;
