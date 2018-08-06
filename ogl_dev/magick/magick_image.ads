
with Magick_Blob.API;
limited with Magick_Image.API;
with Core_Image;

package Magick_Image is

   Image_Exception : Exception;

   procedure Read_File (theImage : in out Core_Image.AI_Image;
                        File_Name : String);
   procedure Write_File (theImage : in out Magick_Image.API.Class_Image.MPP_Image;
                          File_Name : String);
   procedure Write_Blob (theImage : in out Magick_Image.API.Class_Image.MPP_Image;
                         theBlob  : in out Magick_Blob.API.Class_Blob.Blob;
                         Data_Type : String);

end Magick_Image;
