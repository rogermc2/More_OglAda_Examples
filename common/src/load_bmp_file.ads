
with GL.Objects.Textures;
with GL.Types;

package Load_BMP_File is
    Image_Error : Exception;

    type BMP_Header is private;

    Header_Error : Exception;

--      procedure Load_BMP (File_Name : String;
--                          theTexture : out GL.Objects.Textures.Texture);
    procedure Load_BMP_To_Texture (File_Name  : String; Wrap : Boolean;
                                   theTexture : out GL.Objects.Textures.Texture);
    procedure Print_BMP_Header (Header : BMP_Header);

private
    type BMP_Header is record
        Magic                 : String (1 .. 2);
        File_Size             : GL.Types.UInt;
        Data_Offset           : GL.Types.UInt;
        Width                 : GL.Types.UInt;
        Height                : GL.Types.UInt;
        Num_Colour_Planes     : GL.Types.UInt;
        Bits_Per_Pixel        : GL.Types.UInt;  --  Colour depth
        Compression           : GL.Types.UInt;
        Pixel_Data_Size       : GL.Types.UInt;
        Horizontal_Resolution : GL.Types.UInt;
        Vertical_Resolution   : GL.Types.UInt;
        Num_Palette_Colours   : GL.Types.UInt;
        Important_Colours     : GL.Types.UInt;
        Byte_Size             : GL.Types.UInt := 24; --  bytes
    end record;
end Load_BMP_File;
