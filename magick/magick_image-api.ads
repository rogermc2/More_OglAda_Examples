
with System;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package Magick_Image.API is

      type MPP_Image is record
         Ref : System.Address;
--           Ref : access Image_Reference.Class_Image_Ref.Image_Ref;
      end record;
    pragma Convention (C_Pass_By_Copy, MPP_Image);

   function Blob_Length return size_t;
   pragma Import (Cpp, Blob_Length, "blobLength");  --  image_io.data

   function Get_Blob_Data return System.Address;
   pragma Import (Cpp, Get_Blob_Data, "blobData");  --  image_io.data

   function Get_Image_Data return System.Address;
   pragma Import (Cpp, Get_Image_Data, "imageData");  --  image_io.data

    --  Load_Blob tested with project 19_specular_lighting
   function Load_Blob (File_Name : Interfaces.C.Strings.chars_ptr;
                       Data_Type : Interfaces.C.Strings.chars_ptr) return int;
   pragma Import (Cpp, Load_Blob, "loadBlob");  --  image_io.loadBlob

      --  According to the GNAT Reference Manual,
      --  there is no need to indicate the C++ mangled names associated
      --  with each subprogram because it is assumed that all the calls to
      --  these primitives will be dispatching calls.
      --  ******  The CPP wrappers are in imag-io.h  *****
      procedure Read (theImage : in out MPP_Image;
                      File_Name : Interfaces.C.Strings.chars_ptr);  -- ../Magick++/lib/Magick++/ImagePP.h:1236
      pragma Import (Cpp, Read, "readFile");

      procedure Write (anImage : in out MPP_Image;
                       File_Name : Interfaces.C.Strings.chars_ptr);
      pragma Import (Cpp, Write, "writeFile");

      procedure Write_Blob (Data_Blob : Interfaces.C.char_array;
                            RGBA : Interfaces.C.Strings.chars_ptr;
                            Data_Length : size_t);
      pragma Import (Cpp, Write_Blob, "loadBlob");  --  image_io.writeBlob

end Magick_Image.API;
