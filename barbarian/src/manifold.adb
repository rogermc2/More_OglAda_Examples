
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Objects.Textures;

with Game_Utils;
with Settings;
with Texture_Manager;

package body Manifold is

    package Int_Package is new Ada.Containers.Doubly_Linked_Lists
      (Int);
    type Int_List is new Int_Package.List with null record;

    package Tiles_Package is new Ada.Containers.Doubly_Linked_Lists
      (Character);
    type Tiles_List is new Tiles_Package.List with null record;

    package Batches_Package is new Ada.Containers.Vectors
      (Positive, Batch_Meta);
    type Batches_List is new Batches_Package.Vector with null record;

    --      Max_Tile_Cols : constant Int := 64;
    Max_Cols      : Int := 0;
    Max_Rows      : Int := 0;
    Batches           : Batches_List;
    Batches_Across    : Int := 0;
    Batches_Down      : Int := 0;
    Batch_Split_Count : Integer := 0;
    Total_Tiles       : Int := 0;
    Tile_Heights      : Int_List;
    Tile_Facings      : Tiles_List;
    Tile_Textures     : Int_List;
    Tile_Types        : Tiles_List;
    Diff_Palette_Name : Unbounded_String := To_Unbounded_String ("");
    Spec_Palette_Name : Unbounded_String := To_Unbounded_String ("");
    Tile_Tex          : GL.Objects.Textures.Texture;
    Tile_Spec_Tex     : GL.Objects.Textures.Texture;
    Ramp_Diff_Tex     : GL.Objects.Textures.Texture;
    Ramp_Spec_Tex     : GL.Objects.Textures.Texture;

    procedure Parse_Facings_By_Row (File : File_Type; Max_Rows, Max_Cols : Int;
                                    Tile_List : in out Tiles_List);

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

    function Is_Tile_Valid (Col, Row : Int) return Boolean is
    begin
        return Col >= 0 and Col < Max_Cols and  Row >= 0 and Row < Max_Rows;
    end Is_Tile_Valid;

    --  ----------------------------------------------------------------------------

    procedure Load_Palette_File_Names (File : File_Type) is

        function  Get_Palette_File_Name (ID : String) return Unbounded_String is
            aLine : constant String := Get_Line (File);
            Label : constant String (1 .. 3) := aLine (1 .. 3);
        begin
            if Label /= ID then
                Game_Utils.Game_Log
                  ("Manifold.Get_Palette_File_Name invalid format, expected "
                   & "line commencing " & ID & " but obtained " & aLine);
                raise Manifold_Parsing_Exception with
                  "Manifold.Get_Palette_File_Name, invalid format, " & ID &
                  " expected starting " & aLine;
            end if;
            return To_Unbounded_String ("src/" & aLine (4 .. aLine'Last));
        end Get_Palette_File_Name;

    begin
        Game_Utils.Game_Log
          ("Manifold.Load_Palette_File_Names loading palette file names");
        Diff_Palette_Name := Get_Palette_File_Name ("dm ");
        Spec_Palette_Name := Get_Palette_File_Name ("sm ");
        Put_Line ("Manifold.Load_Palette_File_Names palette file names loaded");

        Game_Utils.Game_Log
          ("Manifold.Load_Palette_File_Names palette file names loaded");

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Manifold.Load_Palette_File_Names!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Load_Palette_File_Names;

    --  ------------------------------------------------------------------------
    pragma Warnings (off);
    procedure Load_Char_Rows (File : File_Type; Load_Type : String;
                              Tile_List : in out Tiles_List) is
        use Ada.Strings;
        Header     : constant String := Get_Line (File);
        Cols       : Int := 0;
        Rows       : Int := 0;
        Pos1       : constant Natural := Fixed.Index (Header, " ") + 1;
        Pos2       : Natural;
        Prev_Char  : Character;
    begin
        if Fixed.Index (Header (1 .. Load_Type'Length), Load_Type) = 0 then
            Game_Utils.Game_Log ("Error: Invalid format, " & Load_Type &
                                   " expected: " & Header (1 .. Pos1));
            raise Manifold_Parsing_Exception with
              "Invalid format, " & Load_Type & " expected: " & Header (1 .. Pos1);
        end if;

        Pos2 := Fixed.Index (Header (Pos1 + 1 .. Header'Last), "x");
        Cols := Int'Value (Header (Pos1 .. Pos2 - 1));
        Rows := Int'Value (Header (Pos2 + 1 .. Header'Last));

        Game_Utils.Game_Log ("Loading " & Load_Type & " rows," & Int'Image (Rows)
                             & " rows, "  & Int'Image (Cols) & " columns");
        for row in 1 .. Rows loop
            declare
                aString : constant String := Get_Line (File);
                aChar   : Character;
            begin
                --                  Game_Utils.Game_Log ("Row " & Int'Image (row) & ": aString " & aString);
                if aString'Length < Max_Cols then
                    raise Manifold_Parsing_Exception with
                      "Manifold.Load_Char_Rows: textures line has not enough columns.";
                end if;
                Prev_Char := ASCII.NUL;
                for col in 1 .. Cols loop
                    aChar := aString (Integer (col));
                    if Prev_Char = '\' and then
                      (aChar = 'n' or aChar = ASCII.NUL) then
                        Tile_List.Delete_Last;
                    else
--                          Tile_List.Append (aChar);
                        Null;
                    end if;
                end loop;
                Prev_Char := aChar;
            end;  --  declare block
        end loop;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Manifold.Load_Char_Rows!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Load_Char_Rows;

    --  ----------------------------------------------------------------------------

    procedure Load_Int_Rows (File : File_Type; Load_Type : String;
                             Texture_List : in out Int_List) is
        use Ada.Strings;
        Header     : constant String := Get_Line (File);
        Code_0     : constant Int := Character'Pos ('0');
        Code_a     : constant Int := Character'Pos ('a');
        Cols       : Int := 0;
        Rows       : Int := 0;
        Pos1       : constant Natural := Fixed.Index (Header, " ") + 1;
        Pos2       : Natural;
        Prev_Char  : Character;
    begin
        if Fixed.Index (Header (1 .. Load_Type'Length), Load_Type) = 0 then
            Game_Utils.Game_Log ("Error: Invalid format, " & Load_Type &
                                   " expected: " & Header (1 .. Pos1));
            raise Manifold_Parsing_Exception with
              "Invalid format, " & Load_Type & " expected: " & Header (1 .. Pos1);
        end if;

        Pos2 := Fixed.Index (Header (Pos1 + 1 .. Header'Last), "x");
        Cols := Int'Value (Header (Pos1 .. Pos2 - 1));
        Rows := Int'Value (Header (Pos2 + 1 .. Header'Last));

        Game_Utils.Game_Log ("Loading " & Load_Type & " rows," & Int'Image (Rows)
                             & " rows, "  & Int'Image (Cols) & " columns");
        for row in 1 .. Rows loop
            declare
                aString    : constant String := Get_Line (File);
                Tex_Char   : Character;
                Tex_Int    : Int;
            begin
                --                  Game_Utils.Game_Log ("Row " & Int'Image (row) & ": aString " & aString);
                if aString'Length < Max_Cols then
                    raise Manifold_Parsing_Exception with
                      "Manifold.Load_Int_Rows: textures line has not enough columns.";
                end if;
                Prev_Char := ASCII.NUL;
                for col in 1 .. Cols loop
                    Tex_Char := aString (Integer (col));
                    if Prev_Char = '\' and then
                      (Tex_Char = 'n' or Tex_Char = ASCII.NUL) then
                        Texture_List.Delete_Last;
                    else
                        if Tex_Char >= '0' and Tex_Char <= '9' then
                            Tex_Int := Character'Pos (Tex_Char) - Code_0;
                        else
                            Tex_Int := 10 + Character'Pos (Tex_Char) - Code_a;
                        end if;
--                          Texture_List.Append (Tex_Int);
                    end if;
                end loop;
                Prev_Char := Tex_Char;
            end;  --  declare block
        end loop;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Manifold.Load_Int_Rows!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Load_Int_Rows;

    --  ----------------------------------------------------------------------------

    function Load_Textures return Boolean is
        use Texture_Manager;
        OK : Boolean := False;
    begin
        if Load_Image_To_Texture
          (To_String (Diff_Palette_Name), Tile_Tex, True, True) then
            if Load_Image_To_Texture (To_String (Spec_Palette_Name),
                                      Tile_Spec_Tex, True, True) then
                if Load_Image_To_Texture ("src/textures/stepsTileSet1_diff.png",
                                          Ramp_Diff_Tex, True, True) then
                    OK := Load_Image_To_Texture
                      ("src/textures/stepsTileSet1_spec.png",
                       Ramp_Spec_Tex, True, True);
                end if;
            end if;
        end if;
        return OK;
    end Load_Textures;

    --  ------------------------------------------------------------------------

    function Load_Tiles (File : File_Type) return Boolean is
        use Ada.Strings;
        use Settings;
        Max_Cols : Int := 0;
        Max_Rows : Int := 0;
        aLine    : constant String := Get_Line (File);
        Pos1     : Natural;
        Pos2     : Natural;
    begin
        Game_Utils.Game_Log ("Loading tiles and generating manifold from FP...");
        Put_Line ("Manifold.Load_Tiles loading tiles ");
        Pos1 := Fixed.Index (aLine, " ") + 1;
        if Fixed.Index (aLine, "facings ") = 0 then
            raise Manifold_Parsing_Exception with
              "Invalid format, ""facings"" expected: " & aLine (1 .. Pos1);
        end if;

        Pos2 := Fixed.Index (aLine (Pos1 + 1 .. aLine'Last), "x");

        Max_Cols := Int'Value (aLine (Pos1 .. Pos2 - 1));
        Max_Rows := Int'Value (aLine (Pos2 + 1 .. aLine'Last));
        Total_Tiles := Max_Rows * Max_Cols;
        Game_Utils.Game_Log (" Maximum columns " & Int'Image (Max_Cols) &
                               ", maximum rows " & Int'Image (Max_Rows) &
                               ", total tiles " & Int'Image (Total_Tiles));
        Batches_Across :=
          Int (Float'Ceiling (Float (Max_Cols) / Float (Tile_Batch_Width)));
        Batches_Down :=
          Int (Float'Ceiling (Float (Max_Rows) / Float (Tile_Batch_Width)));
        Batch_Split_Count := Integer (Batches_Across * Batches_Down);
        Game_Utils.Game_Log
          (Integer'Image (Batch_Split_Count) &
             " batches of width " & Integer'Image (Settings.Tile_Batch_Width) &
             "; batches across " & Int'Image (Batches_Across) &
             " down " & Int'Image (Batches_Down));

        Parse_Facings_By_Row (File, Max_Rows, Max_Cols, Tile_Facings);

        Load_Int_Rows (File, "textures", Tile_Textures);
        Load_Char_Rows (File, "types", Tile_Types);
        Load_Int_Rows (File, "heights", Tile_Heights);

        Load_Palette_File_Names (File);
        Put_Line ("Manifold.Load_Tiles file names loaded.");
        Game_Utils.Game_Log ("Palette file names: " & To_String (Diff_Palette_Name)
                             & ", " & To_String (Spec_Palette_Name));
        return Load_Textures;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Manifold.Load_Tiles!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
            return False;
    end Load_Tiles;

    --  ----------------------------------------------------------------------------

    function Number_Of_Tiles return Integer is
    begin
        return Integer (Total_Tiles);
    end Number_Of_Tiles;

    --  ----------------------------------------------------------------------------

    procedure Parse_Facings_By_Row (File : File_Type; Max_Rows, Max_Cols : Int;
                                    Tile_List : in out Tiles_List) is
        prev_Char : Character;
    begin
        Game_Utils.Game_Log ("Parsing facings by row");
        for row in 1 .. Max_Rows loop
            declare
                aString : constant String := Get_Line (File);
                tex_Char  : Character;
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
            Put_Line ("An exception occurred in Manifold.Parse_Facings_By_Row!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Parse_Facings_By_Row;

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
