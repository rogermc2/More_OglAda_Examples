
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Objects.Textures;
with GL.Types; use GL.Types;

with Maths;

with Batch_Manager;
with Game_Utils;
with GL_Maths;
with Settings;
with Texture_Manager;

package body Tiles_Manager is

    type Static_Light_Data is record
        Row      : Integer := 0;
        Column   : Integer := 0;
        Position : GL.Types.Singles.Vector3 := Maths.Vec3_0;
        Diffuse  : GL.Types.Singles.Vector3 := Maths.Vec3_0;
        Specular : GL.Types.Singles.Vector3 := Maths.Vec3_0;
        Distance : GL.Types.Single := 0.0;
    end record;

    package Static_Light_Package is new Ada.Containers.Vectors
      (Positive, Static_Light_Data);
    type Static_Light_List is new Static_Light_Package.Vector with null record;

    Tile_Tex              : GL.Objects.Textures.Texture;
    Tile_Spec_Tex         : GL.Objects.Textures.Texture;
    Tile_Heights          : GL_Maths.Integers_List;
    Tile_Facings          : Tile_List;
    Tile_Textures         : GL_Maths.Integers_List;
    Tile_Types            : Tile_List;
    Ramp_Diff_Tex         : GL.Objects.Textures.Texture;
    Ramp_Spec_Tex         : GL.Objects.Textures.Texture;
    Static_Lights         : Static_Light_List;
    Diff_Palette_Name     : Unbounded_String := To_Unbounded_String ("");
    Spec_Palette_Name     : Unbounded_String := To_Unbounded_String ("");
    Total_Tiles           : Integer := 0;

    Tiles                 : Tile_List;

    procedure Add_Static_Light (Col, Row, Tile_Height_Offset : Integer;
                                Offset, Diffuse, Specular : Singles.Vector3;
                                Light_Range : Single);
    function Get_Tile_Level (Col, Row : Integer) return Integer;
    procedure Parse_Facings_By_Row (File : File_Type;
                                    Max_Rows, Max_Cols : Integer;
                                    Tiles : in out Tile_List);

    --  ------------------------------------------------------------------------

    procedure Add_Dummy_Manifold_Lights is
        use Maths;
    begin
        Add_Static_Light (1, 1, 0, Vec3_0, Vec3_0, Vec3_0, 0.0);
        Add_Static_Light (1, 1, 0, Vec3_0, Vec3_0, Vec3_0, 0.0);
    end Add_Dummy_Manifold_Lights;

    --  ----------------------------------------------------------------------------

    procedure Add_Static_Light (Col, Row, Tile_Height_Offset : Integer;
                                Offset, Diffuse, Specular : Singles.Vector3;
                                Light_Range : Single) is
        use Batch_Manager;
        X     : constant Single := Single (2 * Col) + Offset (GL.X);
        Y     : constant Single :=
                  Single (2 * Get_Tile_Level (Col, Row) + Tile_Height_Offset) +
                  Offset (GL.Y);
        Z      : constant Single := Single (2 * (Row - 1)) + Offset (GL.Z);
        Total_Batches :  constant Integer := Batches_Across * Batches_Down;
        --          Sorted        : Boolean := False;
        New_Light     : Static_Light_Data;
        aBatch        : Batch_Manager.Batch_Meta;
    begin
        New_Light.Row := Row;
        New_Light.Column := Col;
        New_Light.Position := (X, Y, Z);
        New_Light.Diffuse := Diffuse;
        New_Light.Specular := Specular;
        New_Light.Distance := Light_Range;
        Static_Lights.Append (New_Light);

        for index in 1 .. Total_Batches loop
            aBatch := Batches.Element (index);
            aBatch.Static_Light_Indices.Append (Static_Lights.Last_Index);
            Batches.Replace_Element (index, aBatch);
        end loop;

    end Add_Static_Light;

    --  ----------------------------------------------------------------------------

      procedure Add_Tile_Index (Batch : in out Batch_Manager.Batch_Meta;
                              Tile_Index : Positive) is
    begin
        Batch.Tile_Indices.Append (Tile_Index);
    end Add_Tile_Index;

    --  ----------------------------------------------------------------------------

    procedure Add_Tiles_To_Batches is
        use Batch_Manager;
        use Batches_Package;
        Batch_Across  : Natural;
        Batch_Down    : Natural;
        Batch         : Batch_Manager.Batch_Meta;
        Batch_Index   : Positive;
        Tile_Index    : Positive;
    begin
--          Game_Utils.Game_Log ("Manifold.Add_Tiles_To_Batches Total_Tiles, Max_Rows, Max_Cols " &
--                                 Integer'Image (Total_Tiles) & ", " &
--                                 Integer'Image (Max_Rows) & ", " &
--                                 Integer'Image (Max_Cols));
--
--          Game_Utils.Game_Log ("Manifold.Add_Tiles_To_Batches Settings.Tile_Batch_Width " &
--                                 Integer'Image (Settings.Tile_Batch_Width));
        --          for index in 1 .. Total_Tiles loop
        for Row in 1 .. Max_Rows loop
            --              Row := index / Max_Cols + 1;
--              Game_Utils.Game_Log ("Manifold.Add_Tiles_To_Batches Row" &
--                                     Integer'Image (Row));
            Batch_Down  := (Row - 1) / Settings.Tile_Batch_Width;
            for Col in 1 .. Max_Cols loop
                --              Col := index - (Row - 1) * Max_Cols;
--                  Game_Utils.Game_Log ("Manifold.Add_Tiles_To_Batches Col" &
--                                         Integer'Image (Col));
                Tile_Index := (Row - 1) * Max_Cols + Col;
                Batch_Across := (Col - 1) / Settings.Tile_Batch_Width;
                Batch_Index := Batches_Across * Batch_Down + Batch_Across + 1;
--                  Game_Utils.Game_Log ("Manifold.Add_Tiles_To_Batches Batch_Index" &
--                                         Integer'Image (Batch_Index));
                if Has_Element (Batches.To_Cursor (Batch_Index)) then
                    Batch := Batches.Element (Batch_Index);
                    Add_Tile_Index (Batch, Tile_Index);
                    Batch.Tile_Count := Batch.Tile_Count + 1;
                    Batches.Replace_Element (Batch_Index, Batch);
                else
                    Batch.Tile_Count := 1;
                    Batches.Append (Batch);
                end if;
            end loop;
        end loop;
        Game_Utils.Game_Log ("Add_Tiles_To_Batches Batch, Batches.Length " &
                              Ada.Containers.Count_Type'Image (Batches.Length));
        Game_Utils.Game_Log ("Add_Tiles_To_Batches Batch_Split_Count, total tiles " &
                            Integer'Image (Batch_Split_Count) & ", " &
                            Integer'Image (Total_Tiles));
        for index in 1 .. Batch_Split_Count loop
            Regenerate_Batch (Tiles, index);
        end loop;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Tiles_Manager.Add_Tiles_To_Batches!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Add_Tiles_To_Batches;

    --  ----------------------------------------------------------------------------

    function Get_Tile_Level (Col, Row : Integer) return Integer is
        use Batch_Manager;
    begin
        if Col < 1 or Col > Max_Cols or Row < 1 or Row > Max_Rows then
            raise Tiles_Manager_Exception with
              " Tiles_Manager.Get_Tile_Level, invalid row or column: " &
              Integer'Image (Row) & ", " & Integer'Image (Col);
        end if;
        return Tile_Heights.Element ((Row - 1) * Max_Cols + Col);
    end Get_Tile_Level;

    --  ----------------------------------------------------------------------------

    procedure Load_Char_Rows (File : File_Type; Load_Type : String;
                              Tiles : in out Tile_List) is
        use Ada.Strings;
        use Tile_Data_Package;
        Header     : constant String := Get_Line (File);
        Cols       : Integer := 0;
        Rows       : Integer := 0;
        Pos1       : constant Natural := Fixed.Index (Header, " ") + 1;
        Pos2       : Natural;
        Prev_Char  : Character;
        aTile      : Tile_Data;
        Tile_Index : Positive;
    begin
        if Fixed.Index (Header (1 .. Load_Type'Length), Load_Type) = 0 then
            Game_Utils.Game_Log ("Error: Invalid format, " & Load_Type &
                                   " expected: " & Header (1 .. Pos1));
            raise Tiles_Manager_Exception with
              "Invalid format, " & Load_Type & " expected: " & Header (1 .. Pos1);
        end if;

        Pos2 := Fixed.Index (Header (Pos1 + 1 .. Header'Last), "x");
        Cols := Integer'Value (Header (Pos1 .. Pos2 - 1));
        Rows := Integer'Value (Header (Pos2 + 1 .. Header'Last));

        Game_Utils.Game_Log ("Loading " & Load_Type & " rows," & Integer'Image (Rows)
                             & " rows, "  & Integer'Image (Cols) & " columns");
        for row in 1 .. Rows loop
            declare
                aString : constant String := Get_Line (File);
                aChar   : Character;
            begin
                --                  Game_Utils.Game_Log ("Row " & Int'Image (row) & ": aString " & aString);
                if aString'Length < Batch_Manager.Max_Cols then
                    raise Tiles_Manager_Exception with
                      "Tiles_Manager.Load_Char_Rows: " & Load_Type &
                      " line has not enough columns.";
                end if;
                Prev_Char := ASCII.NUL;
                for col in 1 .. Cols loop
                    Tile_Index := (row - 1) * Batch_Manager.Max_Cols + col;
                    if Has_Element (Tiles.To_Cursor (Tile_Index)) then
                        aTile := Tiles.Element (Tile_Index);
                    end if;

                    aChar := aString (Integer (col));
                    if Prev_Char = '\' and then
                      (aChar = 'n' or aChar = ASCII.NUL) then
                        Tiles.Delete_Last;
                    else
                        aTile.Tile_Type := aChar;
                    end if;

                    if Has_Element (Tiles.To_Cursor (Tile_Index)) then
                        Tiles.Replace_Element (Tile_Index, aTile);
                    else
                        Tiles.Append (aTile);
                    end if;
                end loop;
                Prev_Char := aChar;
            end;  --  declare block
            Tiles.Append (aTile);
        end loop;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Manifold.Load_Char_Rows!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Load_Char_Rows;

    --  ----------------------------------------------------------------------------

    procedure Load_Int_Rows (File : File_Type; Load_Type : String;
                             Tiles : in out Tile_List) is
        use Ada.Strings;
        use Tile_Data_Package;
        Header     : constant String := Get_Line (File);
        Code_0     : constant Integer := Character'Pos ('0');
        Code_a     : constant Integer := Character'Pos ('a');
        Cols       : Integer := 0;
        Rows       : Integer := 0;
        Pos1       : constant Natural := Fixed.Index (Header, " ") + 1;
        Pos2       : Natural;
        Prev_Char  : Character;
        aTile      : Tile_Data;
        Tile_Index : Positive;
    begin
        if Fixed.Index (Header (1 .. Load_Type'Length), Load_Type) = 0 then
            Game_Utils.Game_Log ("Error: Invalid format, " & Load_Type &
                                   " expected: " & Header (1 .. Pos1));
            raise Tiles_Manager_Exception with
              "Invalid format, " & Load_Type & " expected: " & Header (1 .. Pos1);
        end if;

        Pos2 := Fixed.Index (Header (Pos1 + 1 .. Header'Last), "x");
        Cols := Integer'Value (Header (Pos1 .. Pos2 - 1));
        Rows := Integer'Value (Header (Pos2 + 1 .. Header'Last));

        Game_Utils.Game_Log ("Loading " & Load_Type & " rows," &
                               Integer'Image (Rows) & " rows, "  &
                               Integer'Image (Cols) & " columns");
        for row in 1 .. Rows loop
            declare
                aString    : constant String := Get_Line (File);
                Tex_Char   : Character;
                Tex_Int    : Integer;
            begin
                --                  Game_Utils.Game_Log ("Row " & Int'Image (row) & ": aString " & aString);
                if aString'Length < Batch_Manager.Max_Cols then
                    raise Tiles_Manager_Exception with
                      " Tiles_Manager.Load_Int_Rows: " & Load_Type &
                      " line has not enough columns.";
                end if;
                Prev_Char := ASCII.NUL;
                for col in 1 .. Cols loop
                    Tile_Index := (row - 1) * Batch_Manager.Max_Cols + col;
                    if Has_Element (Tiles.To_Cursor (Tile_Index)) then
                        aTile := Tiles.Element (Tile_Index);
                    end if;

                    Tex_Char := aString (Integer (col));
                    if Prev_Char = '\' and then
                      (Tex_Char = 'n' or Tex_Char = ASCII.NUL) then
                        Tiles.Delete_Last;
                    else
                        if Tex_Char >= '0' and Tex_Char <= '9' then
                            Tex_Int := Character'Pos (Tex_Char) - Code_0;
                        else
                            Tex_Int := 10 + Character'Pos (Tex_Char) - Code_a;
                        end if;

                        if Load_Type = "textures" then
                            aTile.Texture := Tex_Int;
                        elsif Load_Type = "heights" then
                            aTile.Height := Tex_Int;
                        end if;
                        if Has_Element (Tiles.To_Cursor (Tile_Index)) then
                            Tiles.Replace_Element (Tile_Index, aTile);
                        else
                            Tiles.Append (aTile);
                        end if;
                    end if;
                end loop;
                Prev_Char := Tex_Char;
            end;  --  declare block
        end loop;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Tiles_Manager.Load_Int_Rows!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Load_Int_Rows;

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
                raise Tiles_Manager_Exception with
                  "Tiles_Manager.Get_Palette_File_Name, invalid format, " & ID &
                  " expected starting " & aLine;
            end if;
            return To_Unbounded_String ("src/" & aLine (4 .. aLine'Last));
        end Get_Palette_File_Name;

    begin
        Game_Utils.Game_Log
          ("Tiles_Manager.Load_Palette_File_Names loading palette file names");
        Diff_Palette_Name := Get_Palette_File_Name ("dm ");
        Spec_Palette_Name := Get_Palette_File_Name ("sm ");
        Put_Line ("Tiles_Manager.Load_Palette_File_Names palette file names loaded");

        Game_Utils.Game_Log
          ("Tiles_Manager.Load_Palette_File_Names palette file names loaded");

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Tiles_Manager.Load_Palette_File_Names!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Load_Palette_File_Names;

    --  ------------------------------------------------------------------------

    procedure Load_Textures is
        use Texture_Manager;
    begin
        Load_Image_To_Texture
          (To_String (Diff_Palette_Name), Tile_Tex, True, True);
         Load_Image_To_Texture (To_String (Spec_Palette_Name),
                                      Tile_Spec_Tex, True, True);
         Load_Image_To_Texture ("src/textures/stepsTileSet1_diff.png",
                                          Ramp_Diff_Tex, True, True);
         Load_Image_To_Texture ("src/textures/stepsTileSet1_spec.png",
                                Ramp_Spec_Tex, True, True);
    end Load_Textures;

    --  ------------------------------------------------------------------------

  procedure Load_Tiles (File : File_Type) is
        use Ada.Strings;
        use Batch_Manager;
        use Settings;
        aLine    : constant String := Get_Line (File);
        Pos1     : Natural;
        Pos2     : Natural;
    begin
        Game_Utils.Game_Log ("Loading tiles and generating manifold from FP...");
        Put_Line ("Tiles_Manager.Load_Tiles loading tiles ");
        Pos1 := Fixed.Index (aLine, " ") + 1;
        if Fixed.Index (aLine, "facings ") = 0 then
            raise Tiles_Manager_Exception with
              "Invalid format, ""facings"" expected: " & aLine (1 .. Pos1);
        end if;

        Pos2 := Fixed.Index (aLine (Pos1 + 1 .. aLine'Last), "x");

        Max_Cols := Integer'Value (aLine (Pos1 .. Pos2 - 1));
        Max_Rows := Integer'Value (aLine (Pos2 + 1 .. aLine'Last));
        Total_Tiles := Max_Rows * Max_Cols;
        Batches_Across :=
          Integer (Float'Ceiling (Float (Max_Cols) / Float (Tile_Batch_Width)));
        Batches_Down :=
          Integer (Float'Ceiling (Float (Max_Rows) / Float (Tile_Batch_Width)));
        Batch_Split_Count := Integer (Batches_Across * Batches_Down);

        Parse_Facings_By_Row (File, Max_Rows, Max_Cols, Tile_Facings);

        Load_Int_Rows (File, "textures", Tiles);
        Load_Char_Rows (File, "types", Tiles);
        Load_Int_Rows (File, "heights", Tiles);

        Load_Palette_File_Names (File);
        Game_Utils.Game_Log ("Palette file names: " &
                               To_String (Diff_Palette_Name)
                             & ", " & To_String (Spec_Palette_Name));
        Load_Textures;
        Add_Tiles_To_Batches;
        Add_Dummy_Manifold_Lights;

        Game_Utils.Game_Log ("Total points " & Integer'Image (Total_Points));
        Game_Utils.Game_Log ("Manifold generated.");
        Game_Utils.Game_Log ("Tiles loaded.");

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Tiles_Manager.Load_Tiles!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Load_Tiles;

    --  ----------------------------------------------------------------------------

    function Number_Of_Tiles return Integer is
    begin
        return Total_Tiles;
    end Number_Of_Tiles;

    --  ----------------------------------------------------------------------------

    procedure Parse_Facings_By_Row (File : File_Type;
                                    Max_Rows, Max_Cols : Integer;
                                    Tiles : in out Tile_List) is
        use Tile_Data_Package;
        Prev_Char  : Character;
        aTile      : Tile_Data;
        Tile_Index : Positive;
    begin
        Game_Utils.Game_Log ("Parsing facings by row");
        for row in 1 .. Max_Rows loop
            declare
                aString   : constant String := Get_Line (File);
                Text_Char : Character;
            begin
                Prev_Char := ASCII.NUL;
                for col in 1 .. Max_Cols loop
                    Tile_Index := (row - 1) * Max_Cols + col;
                    Text_Char := aString (Integer (col));
                    if Prev_Char = '\' and then
                      (Text_Char = 'n' or Text_Char = ASCII.NUL) then
                        Tiles.Delete_Last;
                    else
                        aTile.Facing := Text_Char;
                    end if;

                    if Has_Element (Tiles.To_Cursor (Tile_Index)) then
                        Tiles.Replace_Element (Tile_Index, aTile);
                    else
                        Tiles.Append (aTile);
                    end if;
                end loop;
                Prev_Char := Text_Char;
            end;
        end loop;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Tiles_Manager.Parse_Facings_By_Row!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Parse_Facings_By_Row;

    --  ----------------------------------------------------------------------------

    procedure Reset_Vars is
    begin
        Total_Tiles  := 0;
        Tile_Heights.Clear;
        Tile_Facings.Clear;
        Tile_Textures.Clear;
        Tile_Types.Clear;
        Diff_Palette_Name := To_Unbounded_String ("");
        Spec_Palette_Name := To_Unbounded_String ("");
    end Reset_Vars;

    --  ----------------------------------------------------------------------------

end Tiles_Manager;
