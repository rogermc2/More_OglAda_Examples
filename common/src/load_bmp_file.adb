
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures.Targets;
with GL.Pixels;

package body Load_BMP_File is

    type Double_Byte is array (1 .. 2) of GL.Types.Byte;

    -- -------------------------------------------------------------------------

    procedure Load_BMP_To_Texture (File_Name  : String;
                                   theTexture : out GL.Objects.Textures.Texture) is
        use Ada.Streams.Stream_IO;
        use GL.Types;
        File_ID        : Ada.Streams.Stream_IO.File_Type;
        Header         : BMP_Header;
        Byte_Stream    : Stream_Access;
        Data_Size      : UInt;
        DB             : Double_Byte;
        Pixel_Format   : GL.Pixels.Internal_Format;
        Data_Format    : GL.Pixels.Data_Format := GL.Pixels.RGB;
    begin
        Open (File_ID, In_File, File_Name);
        Byte_Stream := Stream (File_ID);
        String'Read (Byte_Stream, Header.Magic);
        if Header.Magic /= "BM" then
            Put_Line ("Load_BMP; File is not a BMP file");
            raise Header_Error;
        end if;

        Set_Index (File_ID, Index (File_ID) + 8);
        UInt'Read (Byte_Stream, Header.Data_Offset);

        Set_Index (File_ID, Index (File_ID) + 4);
        UInt'Read (Byte_Stream, Header.Width);
        UInt'Read (Byte_Stream, Header.Height);
        Data_Size := 3 * Header.Width * Header.Height + 1;
--          Put_Line ("Load_BMP_File.Load_BMP Data_Size: " & UInt'Image (Data_Size));

        Double_Byte'Read (Byte_Stream, DB);
        Header.Num_Colour_Planes := 16 * UInt (DB (2)) + UInt (DB (1));
        Double_Byte'Read (Byte_Stream, DB);
        Header.Bits_Per_Pixel := 16 *  UInt (DB (2)) + UInt (DB (1));
        UInt'Read (Byte_Stream, Header.Compression);
        UInt'Read (Byte_Stream, Header.Pixel_Data_Size);

        UInt'Read (Byte_Stream, Header.Horizontal_Resolution);
        UInt'Read (Byte_Stream, Header.Vertical_Resolution);

        UInt'Read (Byte_Stream, Header.Num_Palette_Colours);
        UInt'Read (Byte_Stream, Header.Important_Colours);

        case Header.Bits_Per_Pixel is
            when 8 => Pixel_Format := GL.Pixels.Luminance;
            when 24 => Pixel_Format := GL.Pixels.RGB;
            when 32 => Pixel_Format := GL.Pixels.RGBA;
                Data_Format := GL.Pixels.RGBA;
            when others => raise Image_Error with
                  "TexturesManger Load_Image; " & File_Name & "format not supported.";
        end case;

        Set_Index (File_ID, Ada.Streams.Stream_IO.Count (Header.Data_Offset));
        declare
            Colour_Content : array (1 .. Data_Size) of Byte := (others => 0);
--              Data_Index     : UInt := 1;
--              Tmp            : Byte;
        begin
            for Byte_Index in Colour_Content'Range  loop
                Byte'Read (Byte_Stream, Colour_Content (Byte_Index));
            end loop;
        --  Convert BGR to RGB
--          while Data_Index < Data_Size loop
--              Tmp := Colour_Content (Data_Index);
--              Colour_Content (Data_Index) :=  Colour_Content (Data_Index + 2);
--              Colour_Content (Data_Index + 2) := Tmp;
--              Data_Index := Data_Index + 3;
--          end loop;

            theTexture.Initialize_Id;
            GL.Objects.Textures.Targets.Texture_2D.Bind (theTexture);
            GL.Pixels.Set_Pack_Alignment (GL.Pixels.Bytes);
            GL.Objects.Textures.Targets.Texture_2D.Load_From_Data
              (Level => 0, Internal_Format => Pixel_Format,
               Width => Int (Header.Width), Height => Int (Header.Height),
               Source_Format => Data_Format, Source_Type => GL.Pixels.Unsigned_Byte,
               Source  => GL.Objects.Textures.Image_Source (Colour_Content'Address));

            GL.Objects.Textures.Targets.Texture_2D.Generate_Mipmap;
    end;  -- declare block

exception
    when others =>
        Put_Line ("An exception occurred in Load_BMP_File.Load_BMP.");
        raise;
end Load_BMP_To_Texture;

    -- -------------------------------------------------------------------------

procedure Print_BMP_Header (Header : BMP_Header) is
        use GL.Types;
begin
        Put_Line ("BMP parameters:");
        Put_Line ("Load_BMP_File.Load_BMP width, Height: " &
                   UInt'Image (Header.Width) & "  " & UInt'Image (Header.Height));
        Put_Line ("Load_BMP_File.Load_BMP Num_Colour_Planes: " &
                    UInt'Image (Header.Num_Colour_Planes));
        Put_Line ("Load_BMP_File.Load_BMP Bits_Per_Pixel: " &
                    UInt'Image (Header.Bits_Per_Pixel));
        Put_Line ("Load_BMP_File.Load_BMP Compression: " &
                   UInt'Image (Header.Compression));
        Put_Line ("Load_BMP_File.Load_BMP Pixel_Data_Size: " &
                   UInt'Image (Header.Pixel_Data_Size));
        Put_Line ("Load_BMP_File.Load_BMP Horizontal_Resolution: " &
                   UInt'Image (Header.Horizontal_Resolution));
        Put_Line ("Load_BMP_File.Load_BMP Vertical_Resolution: " &
                   UInt'Image (Header.Vertical_Resolution));
        Put_Line ("Load_BMP_File.Load_BMP Num_Palette_Colours: " &
                   UInt'Image (Header.Num_Palette_Colours));
        Put_Line ("Load_BMP_File.Load_BMP Important_Colours: " &
                   UInt'Image (Header.Important_Colours));
        New_Line;
end Print_BMP_Header;

    -- -------------------------------------------------------------------------

end Load_BMP_File;
