
with System;

with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with Magick_Exception;
with Magick_Image.API;

package body Magick_Image is

   --  Blob_Data is Blob_Package.List;
   function Get_Blob_Data return Magick_Blob.Blob_Data is
      use Interfaces.C;
      use Interfaces.C.Strings;
      use Magick_Blob.Blob_Package;
      Data_Address  : constant System.Address := Magick_Image.API.Get_Blob_Data;
      Data_Length   : constant size_t := Magick_Image.API.Blob_Length;
      theData       : Magick_Blob.Blob_Data;
      Index         : size_t := 0;
   begin
      declare
         Data_Array : array (1 .. Data_Length) of aliased GL.Types.UByte
           with Import, Convention => C, Address => Data_Address;
      begin
         while Index < Data_Length loop
            Index := Index + 1;
            theData.Append (Data_Array (Index));
         end loop;
      end;
      return theData;
   exception
      when others =>
         New_Line;
         Put_Line ("An exception occurred in Magick_Image.Get_Blob_Data.");
         Put_Line ("Get_Blob_Data Index: " & size_t'Image (Index));
         raise;
   end Get_Blob_Data;

   --  -------------------------------------------------------------------------

   function Get_Image return Core_Image.Image is
      use Interfaces.C;
      use Interfaces.C.Strings;
      Data_Address : constant System.Address := Magick_Image.API.Get_Image_Data;
      C_Image      : Core_Image.API_Image
        with Import, Convention => C, Address => Data_Address;
   begin
      return Core_Image.To_Image (C_Image);
   exception
      when others =>
         New_Line;
         Put_Line ("An exception occurred in Magick_Image.Get_Image.");
         raise;
   end Get_Image;

   --  -------------------------------------------------------------------------

   --      function Get_Blob_Length return GL.Types.UInt is
   --      begin
   --          return GL.Types.UInt (Magick_Image.API.Blob_Length);
   --      end Get_Blob_Length;

   --  -------------------------------------------------------------------------

   procedure Load_Blob (File_Name, Data_Type : String) is
      use Interfaces.C;
      use Interfaces.C.Strings;
      Result : constant Boolean := Magick_Image.API.Load_Blob (New_String (File_Name),
                                                               New_String (Data_Type)) = 1;
   begin
      if not Result then
         raise Image_Exception with
           "Magick_Image.Load_Blob failed to load image: " & File_Name;
      end if;
   end Load_Blob;

   --  -------------------------------------------------------------------------

   --  Based on Image.cpp void Magick::Image::read(const std::string &imageSpec_)
   procedure Read_Image (New_Image : in out Core_Image.Image;
--                           Image_Record_Ptr  : out MPP_Image;
                         File_Name : String) is
      use System;
      use Interfaces.C;
      use Interfaces.C.Strings;
      use Core_Image;
      --        Image_Ref : Magick_Image.API.MPP_Image;
      Image_Ptr : access Core_Image.Image;
      Info      : aliased Core_Image.Image_Info;
      Excep     : aliased Magick_Exception.Exception_Info;
    begin
--        Magick_Image.API.Read (Image_Ref, New_String (File_Name));
      Image_Ptr := Magick_Image.API.Read_Image (Info'Access, Excep'Access);
      if Image_Ptr /= Null then
         Put_Line ("Magick_Image.Read_Image image read.");
         New_Image := Image_Ptr.all;
--           theImage := Image_Ref.Ref.all;
--           Image_Record_Ptr.Ref := Image_Ref.Ref;
      else
         raise Image_Exception with
           "Magick_Image.Read_Image failed to read: " & File_Name;
      end if;

   exception
      when others =>
         New_Line;
         Put_Line ("An exception occurred in Magick_Image.Read_Image.");
         raise;
   end Read_Image;

--     --  -------------------------------------------------------------------------

end Magick_Image;
