
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
        Data_Length   : size_t := Magick_Image.API.Blob_Length;
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

    function Get_Blob_Length return GL.Types.UInt is
    begin
        return GL.Types.UInt (Magick_Image.API.Blob_Length);
    end Get_Blob_Length;

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

    --     procedure Read_File (theImage : out Magick_Image.API.Class_Image.MPP_Image;
    procedure Read_File (theImage : in out Core_Image.AI_Image;
                         File_Name : String) is
        Image_Address : System.Address;
        CPP_Image  : Magick_Image.API.MPP_Image;
    begin
        Magick_Image.API.Read (CPP_Image, Interfaces.C.Strings.New_String (File_Name));
        Image_Address := CPP_Image.Ref;
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
