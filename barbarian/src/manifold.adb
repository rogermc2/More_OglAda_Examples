
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

    package Batches_Package is new Ada.Containers.Vectors
      (Positive, Batch_Meta);
    type Batches_List is new Batches_Package.Vector with null record;

    Batches           : Batches_List;

    Batches_Across    : Int := 0;
    Batches_Down      : Int := 0;
    Batch_Split_Count : Integer := 0;
    Total_Tiles       : Int := 0;
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

    function Get_Batch_Index (Column, Row : Int) return Int is
        Result : Int := -1;
    begin
--          if Column >= 0 and Column < Max_Cols and Row >= 0 and Row < Max_Rows then
            Result := (Column + Batches_Across * Row) /
              Int (Settings.Tile_Batch_Width);
--          end if;
        return Result;
    end Get_Batch_Index;

--  ----------------------------------------------------------------------------

    function Get_Light_Index (Column, Row : Int; Light_Number : Integer)
                              return Int is
        Batch_Index   : constant Positive :=
                          Positive (Get_Batch_Index (Column, Row));
        Batch         : Batch_Meta;
        Light_Indices : Tile_Nodes_List;
        Result        : Int := -1;
    begin
        if not Batches.Is_Empty then
           Batch := Batches.Element (Batch_Index);
           Light_Indices := Batch.Static_Light_Indices;
           if Light_Number > Integer (Light_Indices.Length) then
                raise Manifold_Exception with
                  "Manifold.Get_Light_Index; Light number " &
                  Integer'Image (Light_Number) & " requested at ( " &
                  Int'Image (Column) & "," & Int'Image (Row) &
                  ") in batch " &  Integer'Image (Batch_Index) &
		" does not exist.";
           end if;
           Result := Int (Light_Indices.Element (Light_Number));
        end if;
        return Result;
    end Get_Light_Index;

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
                    tex_Char := aString (Integer (col));
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
        Prev_Char  : Character;
        Tex_Char   : Character;
        Char_Index : Integer;
    begin
        for row in 1 .. Max_Rows loop
            Unbounded_String'Read (Input_Stream, aLine);
            declare
                aString : constant String := To_String (aLine);
            begin
                Prev_Char := ASCII.NUL;
                for col in 1 .. Max_Cols loop
                    Tex_Char := aString (Integer (col));
                    if Prev_Char = '\' and then
                      (Tex_Char = 'n' or Tex_Char = ASCII.NUL) then
                      Tile_List.Delete_Last;
                    else
                      if Tex_Char >= '0' and Tex_Char <= '9' then
                          Char_Index := Character'Pos (Tex_Char);
                          Char_Index := Char_Index - Character'Pos ('0');
                          Tex_Char := Character'Val (Char_Index);
                      else
                          Tex_Char := Character'Val (Char_Index);
                          Char_Index := 10 + Char_Index - Character'Pos ('a');
                          Tex_Char := Character'Val (Char_Index);
                      end if;
                      Tile_List.Append (Tex_Char);
                    end if;
                end loop;
                Prev_Char := Tex_Char;
            end;
        end loop;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Manifold.Load_Modified_Part!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Load_Modified_Part;

--  ----------------------------------------------------------------------------

    function Is_Tile_Valid (Col, Row : Int) return Boolean is
    begin
        return Col >= 0 and Col < Max_Cols and  Row >= 0 and Row < Max_Rows;
    end Is_Tile_Valid;

--  ----------------------------------------------------------------------------

    procedure Load_Tiles (Input_Stream : Stream_IO.Stream_Access) is
        use Stream_IO;
        use Settings;
        type Tiles_Header is record
            Name     : String (1 .. 8) := "        ";
            Max_Cols : Int := 0;
            X        : Character := ' ';
            Max_Rows : Int := 0;
        end record;
        String3       : String (1 .. 3);
        Header      : Tiles_Header;
        Tex_Header    : Unbounded_String;
    begin
        Game_Utils.Game_Log ("loading tiles and generating manifold from FP...");
        Tiles_Header'Read (Input_Stream, Header);
        if Header.Name /= "facings " then
            raise Manifold_Parsing_Exception with
              "Invalid format, ""facings"" expected: " & Header.Name;
        end if;

        Max_Cols := Header.Max_Cols;
        Max_Rows := Header.Max_Rows;
        Total_Tiles := Max_Rows * Max_Cols;
        Batches_Across :=
          Int (Float'Ceiling (Float (Max_Cols) / Float (Tile_Batch_Width)));
        Batches_Down :=
          Int (Float'Ceiling (Float (Max_Rows) / Float (Tile_Batch_Width)));
        Batch_Split_Count := Integer (Batches_Across * Batches_Down);

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
        return Integer (Total_Tiles);
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
