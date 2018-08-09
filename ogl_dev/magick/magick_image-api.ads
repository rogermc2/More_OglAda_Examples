
with System;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

--  with Image_Reference;

package Magick_Image.API is

--     package Class_Image is

--        type MPP_Image is tagged limited record
      type MPP_Image is record
         Ref : System.Address;
--           Ref : access Image_Reference.Class_Image_Ref.Image_Ref;
      end record;
    pragma Convention (C_Pass_By_Copy, MPP_Image);
--        pragma Import (Cpp, MPP_Image);

--        procedure Delete_Image (this : access Image);  -- ../Magick++/lib/Magick++/imagePP.h:95
--        pragma Import (CPP, Delete_Image, "_ZN6Magick5ImageD1Ev");

   function Blob_Length return size_t;
   pragma Import (Cpp, Blob_Length, "blobLength");  --  image_io.data

   function Get_Blob_Data return Interfaces.C.Strings.chars_ptr;
   pragma Import (Cpp, Get_Blob_Data, "blobData");  --  image_io.data
--     function Get_Blob_Data (File_Name : Interfaces.C.Strings.chars_ptr;
--                         Data_Type : Interfaces.C.Strings.chars_ptr) return int;

   function Load_Blob (File_Name : Interfaces.C.Strings.chars_ptr;
                       Data_Type : Interfaces.C.Strings.chars_ptr) return int;
   pragma Import (Cpp, Load_Blob, "loadBlob");  --  image_io.writeBlob

--        function New_Image return MPP_Image;
--        pragma Cpp_Constructor (New_Image, "_ZN6Magick5ImageC1Ev");

      --  According to the GNAT Reference Manual, T
      --  there is no need to indicate the C++ mangled names associated
      --  with each subprogram because it is assumed that all the calls to
      --  these primitives will be dispatching calls.
      --  The CPP wrappers are inimag-io.h
      procedure Read (theImage : in out MPP_Image;
                      File_Name : Interfaces.C.Strings.chars_ptr);  -- ../Magick++/lib/Magick++/ImagePP.h:1236
      pragma Import (Cpp, Read, "readFile");
--        pragma Import (Cpp, Read, "_ZN6Magick5Image4readERKNSt3__112basic_stringIcNS1_11char_traitsIcEENS1_9allocatorIcEEEE");

      procedure Write (anImage : in out MPP_Image;
                       File_Name : Interfaces.C.Strings.chars_ptr);
      pragma Import (Cpp, Write, "writeFile");
--        pragma Import (Cpp, Write, "_ZN6Magick5Image5writeERKNSt3__112basic_stringIcNS1_11char_traitsIcEENS1_9allocatorIcEEEE");

      procedure Write_Blob (Data_Blob : Interfaces.C.char_array;
                            RGBA : Interfaces.C.Strings.chars_ptr;
                            Data_Length: size_t);
      pragma Import (Cpp, Write_Blob, "loadBlob");  --  image_io.writeBlob
--        pragma Import (Cpp, Write_Blob, "_ZN6Magick5Image5writeEPNS_4BlobERKNSt3__112basic_stringIcNS3_11char_traitsIcEENS3_9allocatorIcEEEE");

--     end Class_Image;

end Magick_Image.API;
