
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces.C;
with Interfaces.C.Strings;

with ImageMagick.API;
with Magick_Exception;
with Magick_Type;

package body Constitute is

   procedure Read_Image (theImage : in out Core_Image.API_Image;
                         Info : access Core_Image.API_Image_Info) is
      Image_Ptr : access Core_Image.API_Image := Null;
      Except    : aliased Magick_Exception.AI_Exception_Info;
   begin
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

   procedure Read_Image (theImage  : in out Core_Image.API_Image;
                         File_Name : String) is
      use Interfaces.C;
      theFile   : constant Interfaces.C.char_array :=
                    Interfaces.C.To_C (File_Name);
      Info      : aliased Core_Image.API_Image_Info;
      Except    : aliased Magick_Exception.AI_Exception_Info;
      Image_Ptr : access Core_Image.API_Image := Null;
   begin
      for index in 1 .. File_Name'Length loop
         Info.File_Name (size_t (index - 1)) := char (File_Name (index));
      end loop;
      Put_Line ("Constitute.Read_Image 2 reading file. " & File_Name);
      Put_Line ("Constitute.Read_Image 2 Info.File_Name: " &
                  To_Ada (Info.File_Name));
      Put_Line ("Constitute.Read_Image 2 Info.Signature: " &
                  size_t'Image (Info.Signature));
      Image_Ptr := ImageMagick.API.Read_Image (Info'Access, Except'Access);
      if Image_Ptr /= Null then
         theImage := Image_Ptr.all;
      else
         raise Constitute_Exception with "Constitute.Read_Image 2 error. ";
      end if;
    exception
        when others =>
            Put_Line ("An exception occurred in Constitute.Read_Image 2.");
            raise;
   end Read_Image;

   --  -------------------------------------------------------------------------

    procedure Write_Image (theImage : Core_Image.API_Image;
                           Info : in out Core_Image.API_Image_Info) is
      use Magick_Type;
      Info_Alias   : aliased Core_Image.API_Image_Info := Info;
      Image_Alias  : aliased Core_Image.API_Image := theImage;
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
