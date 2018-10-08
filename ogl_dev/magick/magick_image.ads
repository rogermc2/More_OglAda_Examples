
with System;

with Magick_Blob;
with Core_Image;

package Magick_Image is

    type MPP_Image is record
         Ref : System.Address;
   end record;

   Image_Exception : Exception;

   function Get_Blob_Data return Magick_Blob.Blob_Data;
   function Get_Image return Core_Image.Image;
   procedure Load_Blob (File_Name, Data_Type : String);
   procedure Read_Image (theImage  : out MPP_Image;
                         File_Name : String);

end Magick_Image;
