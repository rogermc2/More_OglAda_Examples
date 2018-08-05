
with Core_Image;
limited with Magick_Image.API;
with Magick_Options;
with Magick_Type;
with Thread;

package Image_Reference is

   package Class_Image_Ref is
      use Magick_Options.Class_Options;

      type Image_Ref is record
         Image      : access Core_Image.AI_Image;  -- ../Magick++/lib/Magick++2/imageRef.h:75
         Mutex_Lock : aliased Thread.Class_Mutex_Lock.Mutex_Lock;  -- ../Magick++/lib/Magick++2/imageRef.h:76
         Opts       : access Options;  -- ../Magick++/lib/Magick++2/imageRef.h:77
         Ref_Count  : aliased Magick_Type.ssize_t;  -- ../Magick++/lib/Magick++2/imageRef.h:78
      end record;
      pragma Import (CPP, Image_Ref);

      procedure Delete_Image_Ref (this : access Image_Ref);  -- ../Magick++/lib/Magick++2/imageRef.h:37
      pragma Import (CPP, Delete_Image_Ref, "_ZN6Magick8ImageRefD1Ev");

      function New_Image_Ref return Image_Ref;  -- ../Magick++/lib/Magick++2/imageRef.h:31
      pragma Cpp_Constructor (New_Image_Ref, "_ZN6Magick8ImageRefC1Ev");

      function New_Image_Ref (theImage : access Magick_Image.API.Class_Image.MPP_Image)
                              return Image_Ref;  -- ../Magick++/lib/Magick++2/imageRef.h:34
      pragma Cpp_Constructor (New_Image_Ref, "_ZN6Magick8ImageRefC1EPN10MagickCore6_ImageE");

      function New_Image_Ref (Image : access Magick_Image.API.Class_Image.MPP_Image;
                              Opts  : access Options)
                              return Image_Ref;  -- ../Magick++/lib/Magick++2/imageRef.h:68
      pragma Cpp_Constructor (New_Image_Ref, "_ZN6Magick8ImageRefC1EPN10MagickCore6_ImageEPKNS_7OptionsE");

      procedure Options (Ref : access Image_Ref;
                         Opts : access Options);  -- ../Magick++/lib/Magick++2/imageRef.h:52
      pragma Import (CPP, Options, "_ZN6Magick8ImageRef7optionsEPNS_7OptionsE");

   end Class_Image_Ref;

   type Image_Ref_Ptr is access all Class_Image_Ref.Image_Ref;

end Image_Reference;
