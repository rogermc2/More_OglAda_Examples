
with Magick_Blob.API;
limited with Magick_Image.API;
with Core_Image;
with Image_Reference;

package Magick_Image is

   Image_Exception : Exception;

   procedure Read_File (theImage : in out Core_Image.AI_Image;
                        File_Name : String);
--     procedure Write_File (theImage : in out Magick_Image.API.Class_Image.MPP_Image;
   procedure Write_File (theImage : Core_Image.AI_Image;
                          File_Name : String);
--     procedure Write_Blob (Image_Ptr : in out Image_Reference.Class_Image_Ref.Image_Ref;
   procedure Write_Blob (theImage : Core_Image.AI_Image;
                         theBlob  : out Magick_Blob.Blob_Data;
                         Data_Type : String);

end Magick_Image;
