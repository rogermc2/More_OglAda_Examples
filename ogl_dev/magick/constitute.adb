
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces.C;
with Interfaces.C.Strings;

with ImageMagick.API;
with Magick_Exception;
with Magick_Type;

package body Constitute is

   procedure Read_Image (theImage : in out Core_Image.AI_Image;
                         Info : access Core_Image.AI_Image_Info) is
      Image_Ptr : access Core_Image.AI_Image := Null;
      Except    : aliased Magick_Exception.AI_Exception_Info;
   begin
      Put_Line ("Constitute.Read_Image reading image. " & Interfaces.C.To_Ada (Info.File_Name));
      Image_Ptr := ImageMagick.API.Read_Image (Info, Except'Access);
      Put_Line ("Constitute.Read_Image image read.");
      if Image_Ptr /= Null then
         theImage := Image_Ptr.all;
      else
         raise Constitute_Exception with "Constitute.Read_Image error: " &
           Interfaces.C.To_Ada (Interfaces.C.Strings.Value
                                (Magick_Exception.Get_Description (Except)));
      end if;

    exception
        when others =>
            Put_Line ("An exception occurred in Constitute.Read_Image 1.");
            raise;

   end Read_Image;

   --  -------------------------------------------------------------------------

   procedure Read_Image (theImage : in out Core_Image.AI_Image; File_Name : String) is
      theFile   : aliased constant Interfaces.C.char_array := Interfaces.C.To_C (File_Name);
      Image_Ptr : access Core_Image.AI_Image := Null;
   begin
      Image_Ptr := ImageMagick.API.Read_Image (theFile);
      if Image_Ptr /= Null then
         theImage := Image_Ptr.all;
      else
         raise Constitute_Exception with "Constitute.Read_Image error. ";
      end if;
    exception
        when others =>
            Put_Line ("An exception occurred in Constitute.Read_Image 2.");
            raise;
   end Read_Image;

   --  -------------------------------------------------------------------------

    procedure Write_Image (theImage : Core_Image.AI_Image;
                           Info : in out Core_Image.AI_Image_Info) is
      use Magick_Type;
      Info_Alias   : aliased Core_Image.AI_Image_Info := Info;
      Image_Alias  : aliased Core_Image.AI_Image := theImage;
      Except       : aliased Magick_Exception.AI_Exception_Info;
   begin
      if ImageMagick.API.Write_Image (Info_Alias'Access, Image_Alias'Access,
                                      Except'Access) = Magic_True then
         Info := Info_Alias;
      else
         raise Constitute_Exception with "Constitute.Read_Image error: " &
           Interfaces.C.To_Ada (Interfaces.C.Strings.Value
                                (Magick_Exception.Get_Description (Except)));
      end if;

    exception
        when others =>
            Put_Line ("An exception occurred in Constitute.Write_Image.");
            raise;
   end Write_Image;

end Constitute;
