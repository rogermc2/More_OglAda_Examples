
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Magick_Exception;
with Magick_Pixel;

package Colour is

   type Compliance_Type is (Undefined_Compliance, CSS_Compliance,
                            X11_Compliance, XPM_Compliance,
                            MVG_Compliance, All_Compliance);

   type Error_Info is record
      Mean_Error_Per_Pixel     : double := 0.0;
      Normalized_Mean_Error    : double := 0.0;
      Normalized_Maximum_Error : double := 0.0;
   end record;
   pragma Convention (C_Pass_By_Copy, Error_Info);

   function SVG_Compliance return Compliance_Type renames CSS_Compliance;
   function No_Compliance return Compliance_Type renames Undefined_Compliance;

   --  QueryColorCompliance() returns the red, green, blue, and alpha intensities
   --  for a given color name and standards compliance.
   procedure Query_Color_Compliance (Colour_Name : chars_ptr;
                                     Comply : Compliance_Type;
                                     Info : access Magick_Pixel.Pixel_Info;
                                     Except : access Magick_Exception.Exception_Info);
   pragma Import (C, Query_Color_Compliance, "QueryColorCompliance");

private
  for Compliance_Type use (Undefined_Compliance => 16#0000#,
                           CSS_Compliance       => 16#0001#,
                           X11_Compliance       => 16#0002#,
                           XPM_Compliance       => 16#0004#,
                           MVG_Compliance       => 16#0008#,
                           All_Compliance       => 16#7fffffff#);
end Colour;
