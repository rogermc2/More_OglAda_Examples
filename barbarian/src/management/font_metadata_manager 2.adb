
--  with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Game_Utils;

package body Font_Metadata_Manager is

    type Metadata is record
       Ascii_Code : Integer := -1;
       X_Min      : Single := 0.0;
       Width      : Single := 0.0;
       Y_Min      : Single := 0.0;
       Height     : Single := 0.0;
       Y_Offset   : Single := 0.0;
    end record;

    -- -------------------------------------------------------------------------

    procedure Load_Metadata (Path : String; Glyphs : out Glyph_Array) is
        use Ada.Streams;
        Input_File   : Stream_IO.File_Type;
        Input_Stream : Stream_IO.Stream_Access;
        aLine        : Unbounded_String;
        Meta_Record  : Metadata;
    begin
        Game_Utils.Game_Log ("loading font meta-data from file " &
                            Path);
        Stream_IO.Open (Input_File, Stream_IO.In_File, Path);
        Input_Stream := Stream_IO.Stream (Input_File);
        Unbounded_String'Read (Input_Stream, aLine);
        while not Stream_IO.End_Of_File (Input_File) loop
            Metadata'Read (Input_Stream, Meta_Record);
            Glyphs (Meta_Record.Ascii_Code) :=
              (Meta_Record.Width, 1.0 - Meta_Record.Height - Meta_Record.Y_Offset);
       end loop;
        Stream_IO.Close (Input_File);

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Font_Metadata_Manager.Load_Metadata!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Load_Metadata;

    --  ----------------------------------------------------------------------------

    function Width (Glyphs : Glyph_Array; Index : Integer) return Single is
    begin
        return Glyphs (Index).Width;
    end Width;

    --  ----------------------------------------------------------------------------

    function Y_Offset (Glyphs : Glyph_Array; Index : Integer) return Single is
    begin
        return Glyphs (Index).Y_Offset;
    end Y_Offset;

    --  ----------------------------------------------------------------------------

end Font_Metadata_Manager;
