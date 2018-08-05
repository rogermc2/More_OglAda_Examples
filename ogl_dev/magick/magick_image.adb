
with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

--  with GL.Types;

with Magick_Image.API;

package body Magick_Image is

--     type Raw_Data is array (Interfaces.C.size_t range <>) of GL.Types.UByte;

   procedure Read_File (theImage : out Magick_Image.API.Class_Image.MPP_Image;
                        File_Name : String) is

--        CPP_Image    : Magick_Image.API.Class_Image.MPP_Image;
   begin
      theImage.Read (Interfaces.C.Strings.New_String (File_Name));
   exception
      when others =>
         New_Line;
         Put_Line ("An exception occurred in Magick_Image.Read_File.");
         raise;
   end Read_File;

   --  -------------------------------------------------------------------------

    procedure Write_File (theImage : in out Magick_Image.API.Class_Image.MPP_Image;
                          File_Name : String) is
      use Interfaces.C;

--        CPP_Image    : Magick_Image.API.Class_Image.MPP_Image;
   begin
         theImage.Write (Interfaces.C.Strings.New_String (File_Name));
   exception
      when others =>
         New_Line;
         Put_Line ("An exception occurred in Magick_Image.Write_File image.");
         raise;
   end Write_File;

   --  -------------------------------------------------------------------------

   procedure Write_Blob (theImage : in out Magick_Image.API.Class_Image.MPP_Image;
                         theBlob  : in out Magick_Blob.API.Class_Blob.Blob;
                         Data_Type : String) is
      use Interfaces.C;
   begin
         theImage.Write_Blob (theBlob, Interfaces.C.Strings.New_String (Data_Type));
   exception
      when others =>
         New_Line;
         Put_Line ("An exception occurred in Magick_Image.Write_Blob.");
         raise;
   end Write_Blob;

   --  -------------------------------------------------------------------------

end Magick_Image;
