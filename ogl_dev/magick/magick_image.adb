
with System;

with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with Magick_Image.API;

package body Magick_Image is

   procedure Load_Blob (File_Name, Data_Type : String) is
      use Interfaces.C;
      use Interfaces.C.Strings;
   begin
      if Magick_Image.API.Load_Blob (New_String (File_Name),
                                     New_String (Data_Type)) /= 0 then
         raise Image_Exception with
           "Magick_Image.Load_Blob failed to load " & File_Name;
      end if;
   end Load_Blob;

   --  -------------------------------------------------------------------------

--     procedure Read_File (theImage : out Magick_Image.API.Class_Image.MPP_Image;
   procedure Read_File (theImage : in out Core_Image.AI_Image;
                        File_Name : String) is

      CPP_Image  : Magick_Image.API.MPP_Image;
   begin
      Magick_Image.API.Read (CPP_Image, Interfaces.C.Strings.New_String (File_Name));
--        theImage := CPP_Image.Ref.;
   exception
      when others =>
         New_Line;
         Put_Line ("An exception occurred in Magick_Image.Read_File.");
         raise;
   end Read_File;

   --  -------------------------------------------------------------------------

    procedure Write_File (theImage : Core_Image.AI_Image;
                          File_Name : String) is
      use Interfaces.C;

      Local_Image : Core_Image.AI_Image := theImage;
--        CPP_Image   : Magick_Image.API.MPP_Image;
   begin
      null;
--          CPP_Image.Ref.Image.all := Local_Image;
--          CPP_Image.Write (Interfaces.C.Strings.New_String (File_Name));
   exception
      when others =>
         New_Line;
         Put_Line ("An exception occurred in Magick_Image.Write_File image.");
         raise;
   end Write_File;

   --  -------------------------------------------------------------------------

--     procedure Write_Blob (theImage : Core_Image.AI_Image;
--                           theBlob  : out Magick_Blob.Blob_Data;
--                           Data_Type : String) is
--        use Interfaces.C;
--        use GL.Types;
--        use Magick_Blob.Blob_Package;
--        Blob_Cursor   : Cursor := theBlob.First;
--  --        Data        : Magick_Blob.Data_Array (1 .. UInt (theBlob.Length));
--        Data          : Interfaces.C.char_array (1 ..  Interfaces.C.size_t(theBlob.Length));
--  --    --    Local_Image : Core_Image.AI_Image := theImage;
--        CPP_Image     : Magick_Image.API.MPP_Image;
--        CPP_Data_Type : Strings.chars_ptr := Strings.New_String (Data_Type);
--  --        CPP_Blob    : Magick_Blob.API.Class_Blob.Blob;
--        Blob_Index  : size_t := 0;
--        procedure Add_Element (Elem : Cursor) is
--        begin
--            Blob_Index := Blob_Index + 1;
--            Data (Blob_Index) := char (Element (Elem));
--        end Add_Element;
--     begin
--          CPP_Image.Ref.Image.all := theImage;
--          theBlob.Iterate (Add_Element'Access);
--          --  CPP_Blob.Blob_Ref.Data is a pointer (system address) to the Blob data
--  --          CPP_Blob.Blob_Ref.Data := Data'Address;
--           Put_Line ("Magick_Image.Write_Blob.");
--        Magick_Image.API.Write_Blob (Data, CPP_Data_Type, size_t (Blob_Index));
--
--  --          CPP_Image.Write_Blob (CPP_Blob, Interfaces.C.Strings.New_String (Data_Type));
--     exception
--        when others =>
--           New_Line;
--           Put_Line ("An exception occurred in Magick_Image.Write_Blob.");
--           raise;
--     end Write_Blob;

   --  -------------------------------------------------------------------------

end Magick_Image;
