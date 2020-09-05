
with GL.Types; use GL.Types;

package Font_Metadata_Manager is

    type Glyph_Array is private;

    procedure Load_Metadata (Path : String; Glyphs : out Glyph_Array);
    function Width (Glyphs : Glyph_Array; Index : Integer) return Single;
    function Y_Offset (Glyphs : Glyph_Array; Index : Integer) return Single;

private
    type Glyph_Data is record
        Width    : Single := 0.0;
        Y_Offset : Single := 0.0;
    end record;
    type Glyph_Array is array (1 .. 256) of Glyph_Data;

end Font_Metadata_Manager;
