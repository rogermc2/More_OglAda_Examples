
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

with Core_Image;
with Image_Ref;
with Magick_Exception;
with Magick_Type;

package Magick_Image.API is

   --  Moved from Blob.API to avoid cirularities
   function File_To_Image (theImage : access Core_Image.AI_Image;
                           File_Name : Interfaces.C.char_array;
                           Except : access Magick_Exception.AI_Exception_Info)
                           return Magick_Type.Magick_Boolean_Type;
   pragma Import (C, File_To_Image, "FileToImage");

   --  Moved from Blob.API to avoid cirularities
   function Image_To_Blob (Info : access Core_Image.AI_Image_Info;
                           theImage : access Core_Image.AI_Image; Size : access size_t;
                           Except : access Magick_Exception.AI_Exception_Info)
                           return Magick_Type.Magick_Boolean_Type;
   pragma Import (C, Image_To_Blob, "ImageToBlob");


--     procedure read (this : access Image'Class;
--                     imageSpec_u : access constant cpp_4_9_2_bits_stringfwd_h.Class_string.string);  -- ../Magick++/lib/Magick++/ImagePP.h:1236
--        pragma Import (CPP, read, "_ZN6Magick5Image4readERKSs");



   procedure Read (Ref : access constant AIP_Image'Class;
                   File_Name : Interfaces.C.Strings.chars_ptr);
   pragma Import (C_Plus_Plus, Read, "ZN6Magick5Image8readfileERKNSt7");

   procedure Read_Mask (Ref : in out Image_Ref.Image_Ref_Ptr;
                        Mask : Core_Image.AI_Image);
   pragma Import (C_Plus_Plus, Read_Mask, "readMask");

end Magick_Image.API;
