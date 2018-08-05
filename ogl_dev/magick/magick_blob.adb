
with System;

with  Magick_Blob.API;

--  Magic_Blob corresponds to Magic++.Blob
package body Magick_Blob is
    use Magick_Blob.API.Class_Blob;
    type Data_Array is  array (UInt range <>) of UByte;

    --  -----------------------------------------------------------------------------------

    function Length (aBlob : Blob) return UInt is
    begin
        return UInt (aBlob.Length);
    end Length;

    --  -----------------------------------------------------------------------------------

    function Data_Addr (aBlob : Blob) return System.Address is
    begin
        return aBlob.Data;
    end Data_Addr;

    --  -----------------------------------------------------------------------------------

    function Get_Data (aBlob : Magick_Blob.API.Class_Blob.Blob) return Blob_Data is
        Blob_Length : constant UInt := Length (aBlob);
        Data        : Data_Array (1 .. Blob_Length);
        for Data'Address use Data_Addr (aBlob);
        theData     : Blob_Data;
    begin
        for index in UInt range 1 .. Blob_Length loop
            theData.Append (Data (index));
        end loop;
        return theData;
    end Get_Data;

end Magick_Blob;
