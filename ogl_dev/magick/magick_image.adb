
with System;

with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

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
        C_Image      : Core_Image.AI_Image
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

end Magick_Image;
