
with Interfaces.C.Strings;

limited with Std_IO;

with Core_Blob;
with Stream;

package Core_Blob.API is

   -- File_To_Image and Image_To_Blob moved to Magick_Image.API to avoid circularities

   function Clone_Blob_Info (Info : access Blob_Info) return Core_Blob.Blob_Info_Ptr;
   pragma Import (C, Clone_Blob_Info, "CloneBlobInfo");

   function Get_Blob_File_Handle (arg1 : access int) return access Std_IO.FILE;
   pragma Import (C, Get_Blob_File_Handle, "GetBlobFileHandle");

   function Get_Blob_Stream_Handler (arg1 : access int) return Stream.Stream_Handler;
   pragma Import (C, Get_Blob_Stream_Handler, "GetBlobStreamHandler");

   function File_To_Blob (arg1 : Interfaces.C.Strings.chars_ptr;
                          arg2 : size_t; arg3 : access size_t;
                          arg4 : access int) return Magick_Boolean_Type;
   pragma Import (C, File_To_Blob, "FileToBlob");

   function Get_Blob_Stream_Data (arg1 : access int) return Magick_Boolean_Type;
   pragma Import (C, Get_Blob_Stream_Data, "GetBlobStreamData");

   function Images_To_Blob (arg1, arg2 : access int; arg3 : access size_t;
                            arg4 : access int) return Magick_Boolean_Type;
   pragma Import (C, Images_To_Blob, "ImagesToBlob");

end Core_Blob.API;
