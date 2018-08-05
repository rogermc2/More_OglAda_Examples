
with Interfaces.C.Extensions;

with Core_Image;
with Draw;
with Quantize;

package Magick_Options is

   --  From Magick++/lib/Magick++/Options.h
   package Class_Options is
      type Options is limited record
         Info_Image    : access Core_Image.AI_Image_Info;  -- ../Magick++/lib/Magick++2/Options.h:335
         Info_Quantize : access Quantize.Quantize_Info;  -- ../Magick++/lib/Magick++2/Options.h:336
         Info_Draw     : access Draw.Draw_Info;  -- ../Magick++/lib/Magick++2/Options.h:337
         Quiet         : aliased Interfaces.C.Extensions.bool;  -- ../Magick++/lib/Magick++2/Options.h:338
      end record;
      pragma Import (CPP, Options);

      function New_Options return Options;  -- ../Magick++/lib/Magick++2/Options.h:33
      pragma Cpp_Constructor (New_Options, "_ZN6Magick7OptionsC1Ev");

      procedure Delete_Options (this : access Options);  -- ../Magick++/lib/Magick++2/Options.h:39
      pragma Import (CPP, Delete_Options, "_ZN6Magick7OptionsD1Ev");
   end Class_Options;

end Magick_Options;
