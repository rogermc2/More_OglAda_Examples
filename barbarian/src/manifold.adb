
with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Game_Utils;
with Settings;

package body Manifold is

    package Tiles_Package is new Ada.Containers.Doubly_Linked_Lists
      (Character);
    type Tiles_List is new Tiles_Package.List with null record;

    Batches_Across    : Integer := 0;
    Batches_Down      : Integer := 0;
    Batch_Split_Count : Integer := 0;
    Max_Cols          : Integer := 0;
    Max_Rows          : Integer := 0;
    Total_Tiles       : Integer := 0;
    Tile_Heights      : Tiles_List;
    Tile_Facings      : Tiles_List;
    Tile_Textures     : Tiles_List;
    Tile_Types        : Tiles_List;
    Diff_Palette_Name : Unbounded_String := To_Unbounded_String ("");
    Spec_Palette_Name : Unbounded_String := To_Unbounded_String ("");

--  ----------------------------------------------------------------------------

    function Batch_Split_Size return Integer is
    begin
        return Batch_Split_Count;
    end Batch_Split_Size;

--  ----------------------------------------------------------------------------

    function Init_Manifold return Boolean is
    begin
        return False;
    end Init_Manifold;

--  ----------------------------------------------------------------------------

    procedure Load_Part (Input_Stream : Stream_IO.Stream_Access;
                         Tile_List : in out Tiles_List) is
        aLine     : Unbounded_String;
        prev_Char : Character;
        tex_Char  : Character;
    begin
        for row in 1 .. Max_Rows loop
            Unbounded_String'Read (Input_Stream, aLine);
            declare
                aString : constant String := To_String (aLine);
            begin
                prev_Char := ASCII.NUL;
                for col in 1 .. Max_Cols loop
                    tex_Char := aString (col);
                    if prev_Char = '\' and then
                      (tex_Char = 'n' or tex_Char = ASCII.NUL) then
                      Tile_List.Delete_Last;
                    else
                        Tile_List.Append (tex_Char);
                    end if;
                end loop;
                prev_Char := tex_Char;
            end;
        end loop;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Manifold.Load_Part!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Load_Part;

--  ----------------------------------------------------------------------------

    procedure Load_Modified_Part (Input_Stream : Stream_IO.Stream_Access;
                                  Tile_List : in out Tiles_List) is
        aLine      : Unbounded_String;
        prev_Char  : Character;
        tex_Char   : Character;
        Char_Index : Integer;
    begin
        for row in 1 .. Max_Rows loop
            Unbounded_String'Read (Input_Stream, aLine);
            declare
                aString : constant String := To_String (aLine);
            begin
                prev_Char := ASCII.NUL;
                for col in 1 .. Max_Cols loop
                    tex_Char := aString (col);
                    if prev_Char = '\' and then
                      (tex_Char = 'n' or tex_Char = ASCII.NUL) then
                      Tile_List.Delete_Last;
                    else
                      if tex_Char >= '0' and tex_Char <= '9' then
                          Char_Index := Character'Pos (tex_Char);
                          Char_Index := Char_Index - Character'Pos ('0');
                          tex_Char := Character'Val (Char_Index);
                      else
                          tex_Char := Character'Val (Char_Index);
                          Char_Index := 10 + Char_Index - Character'Pos ('a');
                          tex_Char := Character'Val (Char_Index);
                      end if;
                      Tile_List.Append (tex_Char);
                    end if;
                end loop;
                prev_Char := tex_Char;
            end;
        end loop;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Manifold.Load_Modified_Part!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Load_Modified_Part;

--  ----------------------------------------------------------------------------

    procedure Load_Tiles (Input_Stream : Stream_IO.Stream_Access) is
        use Stream_IO;
        use Settings;
        String8       : String (1 .. 8);
        String3       : String (1 .. 3);
        aChar         : Character;
        Tex_Header    : Unbounded_String;
    begin
        Game_Utils.Game_Log ("loading tiles and generating manifold from FP...");
        String'Read (Input_Stream, String8);
        String'Read (Input_Stream, String8);  --  "facings "
        if Ada.Characters.Handling.To_Lower (String8) /= "facings " then
            raise Manifold_Parsing_Exception with
              "Invalid format, ""facings"" expected.";
        end if;

        Integer'Read (Input_Stream, Max_Cols);
        Character'Read (Input_Stream, aChar);
        Integer'Read (Input_Stream, Max_Rows);
        Total_Tiles := Max_Rows * Max_Cols;
        Batches_Across :=
          Integer (Float'Ceiling (Float (Max_Cols) / Float (Tile_Batch_Width)));
        Batches_Down :=
          Integer (Float'Ceiling (Float (Max_Rows) / Float (Tile_Batch_Width)));
        Batch_Split_Count := Batches_Across * Batches_Down;

        Load_Part (Input_Stream, Tile_Facings);
        Unbounded_String'Read (Input_Stream, Tex_Header);
        Load_Modified_Part (Input_Stream, Tile_Textures);
        Load_Part (Input_Stream, Tile_Types);
        Load_Modified_Part (Input_Stream, Tile_Heights);

        String'Read (Input_Stream, String3);  --  "dm "
        if Ada.Characters.Handling.To_Lower (String3) /= "dm " then
            raise Manifold_Parsing_Exception with
              "Invalid format, ""dm"" expected.";
        end if;
        Unbounded_String'Read (Input_Stream, Diff_Palette_Name);

        String'Read (Input_Stream, String3);  --  "sm "
        if Ada.Characters.Handling.To_Lower (String3) /= "sm " then
            raise Manifold_Parsing_Exception with
              "Invalid format, ""sm"" expected.";
        end if;
        Unbounded_String'Read (Input_Stream, Spec_Palette_Name);

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Manifold.Load_Tiles!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Load_Tiles;

--  ----------------------------------------------------------------------------

    function Number_Of_Tiles return Integer is
    begin
        return Total_Tiles;
    end Number_Of_Tiles;

--  ----------------------------------------------------------------------------

    procedure Reset_Manifold_Vars is
    begin
        Batches_Across := 0;
        Batches_Down := 0;
        Batch_Split_Count := 0;
        Max_Cols := 0;
        Max_Rows := 0;
        Total_Tiles  := 0;
        Tile_Heights.Clear;
        Tile_Facings.Clear;
        Tile_Textures.Clear;
        Tile_Types.Clear;
        Diff_Palette_Name := To_Unbounded_String ("");
        Spec_Palette_Name := To_Unbounded_String ("");
    end Reset_Manifold_Vars;

--  ----------------------------------------------------------------------------

end Manifold;
