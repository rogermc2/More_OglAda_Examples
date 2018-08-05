
with Interfaces.C; use Interfaces.C;

with Colour_Space;
with Magick_Type;
with Method_Attribute;

package Quantize is

   type Dither_Method is (Undefined_Dither_Method, No_Dithe_rMethod,
                          Riemersma_Dither_Method,
                          Floyd_Steinberg_Dither_Method);
   pragma Convention (C, Dither_Method);

   type Quantize_Info is record
      Num_Colours   : size_t := 0;
      Tree_Depth    : size_t := 0;
      Col_Space     : Colour_Space.Colourspace_Type;
      Dither        : Dither_Method;
      Measure_Error : Magick_Type.Magick_Boolean_Type;
      Signature     : size_t :=
                      size_t (Method_Attribute.Magick_Core_Signature);
   end record;
   pragma Convention (C_Pass_By_Copy, Quantize_Info);

end Quantize;
