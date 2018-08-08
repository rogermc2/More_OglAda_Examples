
with System;

with Interfaces.C;
with Interfaces.C.Strings;

--  with Blob_Reference;

package Magick_Blob.API is

    -- File_To_Image and Image_To_Blob moved to Magick_Image.API to avoid circularities
    --
    --     package Class_Blob is
    --
    type Blob is record
        Blob_Ref : System.Address;  -- /usr/local/Cellar/imagemagick/7.0.7-31/include/ImageMagick-7/Magick++/Blob.h:75
    end record;
    pragma Convention (C_Pass_By_Copy, Blob);

--     function New_Blob return Blob;  -- /usr/local/Cellar/imagemagick/7.0.7-31/include/ImageMagick-7/Magick++/Blob.h:31
--      pragma Import (C, New_Blob, "newBlob");
    --        pragma Cpp_Constructor (New_Blob, "");

    procedure Delete_Blob (this : access Blob);  -- /usr/local/Cellar/imagemagick/7.0.7-31/include/ImageMagick-7/Magick++/Blob.h:40
    pragma Import (C, Delete_Blob, "deleteBlob");
    --        pragma Import (CPP, Delete_Blob, "_ZN6Magick4BlobD1Ev");

    function Data (this : access Blob) return System.Address;  -- /usr/local/Cellar/imagemagick/7.0.7-31/include/ImageMagick-7/Magick++/Blob.h:55
    pragma Import (C, Data, "data");
      --      pragma Import (CPP, Data, "_ZNK6Magick4Blob4dataEv");

    function Length (this : access Blob) return Interfaces.C.size_t;  -- /usr/local/Cellar/imagemagick/7.0.7-31/include/ImageMagick-7/Magick++/Blob.h:58
    pragma Import (C, Length, "length");
      --  pragma Import (CPP, Length, "_ZNK6Magick4Blob6lengthEv");
      --     end Class_Blob;

end Magick_Blob.API;
