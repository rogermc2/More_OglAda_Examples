
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Objects.Textures;
with GL.Objects.Programs;

with Batch_Manager;
with Game_Utils;
with GL_Maths;
with Manifold_Shader_Manager;
with Mesh_Loader;
with Settings;
with Texture_Manager;
with Water_Shader_Manager;

package body Manifold is

    package Tiles_Package is new Ada.Containers.Vectors
      (Positive, Character);
    type Tiles_List is new Tiles_Package.Vector with null record;

    Manifold_Program      : GL.Objects.Programs.Program;
    Water_Program         : GL.Objects.Programs.Program;
    --      Max_Tile_Cols    : constant Int := 64;
    Batches               : Batch_Manager.Batches_List;
    Total_Tiles           : Integer := 0;
    Tile_Heights          : GL_Maths.Integers_List;
    Tile_Facings          : Tiles_List;
    Tile_Textures         : GL_Maths.Integers_List;
    Tile_Types            : Tiles_List;
    Diff_Palette_Name     : Unbounded_String := To_Unbounded_String ("");
    Spec_Palette_Name     : Unbounded_String := To_Unbounded_String ("");
    Tile_Tex              : GL.Objects.Textures.Texture;
    Tile_Spec_Tex         : GL.Objects.Textures.Texture;
    Ramp_Diff_Tex         : GL.Objects.Textures.Texture;
    Ramp_Spec_Tex         : GL.Objects.Textures.Texture;
    Ramp_Mesh_Points      : GL_Maths.Vector3_List;
    Ramp_Mesh_Normals     : GL_Maths.Vector3_List;
    Ramp_Mesh_Smooth_Normals : GL_Maths.Vector3_List;
    Ramp_Mesh_Texcoords      : GL_Maths.Vector2_List;
    Ramp_Mesh_Point_Count    : Integer := 0;
    Water_Mesh_Points        : GL_Maths.Vector3_List;
    Water_Mesh_Normals       : GL_Maths.Vector3_List;
    Water_Mesh_Texcoords     : GL_Maths.Vector2_List;
    Water_Mesh_Point_Count   : Integer := 0;
    Static_Lights            : Static_Light_List;

    procedure Add_Static_Light (Col, Row, Tile_Height_Offset : Integer;
                               Offset, Diffuse, Specular : Singles.Vector3;
                               Light_Range : Single);
    function Get_Tile_Level (Col, Row : Integer) return Integer;
    procedure Parse_Facings_By_Row (File : File_Type;
                                    Max_Rows, Max_Cols : Integer;
                                    Tile_List : in out Tiles_List);

    --  ----------------------------------------------------------------------------

    procedure Add_Dummy_Manifold_Lights is
        use Maths;
    begin
        Add_Static_Light (0, 0, 0, Vec3_0, Vec3_0, Vec3_0, 0.0);
        Add_Static_Light (0, 0, 0, Vec3_0, Vec3_0, Vec3_0, 0.0);
    end Add_Dummy_Manifold_Lights;

    --  ----------------------------------------------------------------------------

    procedure Add_Static_Light (Col, Row, Tile_Height_Offset : Integer;
                               Offset, Diffuse, Specular : Singles.Vector3;
                               Light_Range : Single) is
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

    function Add_Tiles_To_Batches return Boolean is
        use Batch_Manager;
        Row           : Integer;
	Col           : Integer;
	Batch_Across  : Integer;
        Batch_Down    : Integer;
        Batch         : Batch_Manager.Batch_Meta;
        Batch_Index   : Positive;
    begin
        for index in 1 .. Total_Tiles loop
            Row := index / Max_Cols;
            Col := index - Row * Max_Cols;
            Batch_Across := Col / Settings.Tile_Batch_Width;
            Batch_Down  := Row / Settings.Tile_Batch_Width;
            Batch_Index := Positive (Batches_Across * Batch_Down + Batch_Across);
            Batch := Batches.Element (Batch_Index);
            Batch.Tile_Count := Batch.Tile_Count + 1;
            Batches.Replace_Element (Batch_Index, Batch);
        end loop;
        return False;
    end Add_Tiles_To_Batches;

    --  ----------------------------------------------------------------------------

    function Batch_Split_Size return Integer is
    begin
        return Batch_Split_Count;
    end Batch_Split_Size;

    --  ----------------------------------------------------------------------------

    procedure Free_Manifold_Mesh_Data is
    begin
        null;
    end Free_Manifold_Mesh_Data;

    --  ----------------------------------------------------------------------------

    function Get_Light_Index (Column, Row, Light_Number : Integer)
                              return Integer is
        use Batch_Manager;
        Batch_Index   : constant Positive :=
                          Positive (Get_Batch_Index (Column, Row));
        Batch         : Batch_Manager.Batch_Meta;
        Light_Indices : GL_Maths.Integers_List;
        Result        : Integer := -1;
    begin
        if not Batches.Is_Empty then
            Batch := Batches.Element (Batch_Index);
            Light_Indices := Batch.Static_Light_Indices;
            if Light_Number > Integer (Light_Indices.Length) then
                raise Manifold_Exception with
                  "Manifold.Get_Light_Index; Light number " &
                  Integer'Image (Light_Number) & " requested at ( " &
                  Integer'Image (Column) & "," & Integer'Image (Row) &
                  ") in batch " &  Integer'Image (Batch_Index) &
                  " does not exist.";
            end if;
            Result := Light_Indices.Element (Light_Number);
        end if;
        return Result;
    end Get_Light_Index;

    --  ------------------------------------------------------------------------

    function Get_Tile_Level (Col, Row : Integer) return Integer is
        use Batch_Manager;
    begin
        if Col < 1 or Col > Max_Cols or Row < 1 or Row > Max_Rows then
            raise Manifold_Exception with
            "Manifold.Get_Tile_Level, invalid row or column: " &
            Integer'Image (Row) & ", " & Integer'Image (Col);
        end if;
        return Tile_Heights.Element ((Row - 1) * Max_Cols + Col);
    end Get_Tile_Level;

    --  ----------------------------------------------------------------------------

    procedure Init is
        Points       : GL_Maths.Vector3_List;
        Texcoords    : GL_Maths.Vector2_List;
        Points_Count : Integer := 0;

    begin
        Game_Utils.Game_Log ("Initializing manifold.");
        Manifold_Shader_Manager.Init (Manifold_Program);
        Game_Utils.Game_Log ("Manifold_Program initialized.");
        Manifold_Shader_Manager.Set_Ambient_Light_Colour ((0.0125, 0.0125, 0.0125));
        Manifold_Shader_Manager.Set_Diff_Map (0);
        Manifold_Shader_Manager.Set_Spec_Map (1);
        Manifold_Shader_Manager.Set_Cube_Texture (3);

        Water_Shader_Manager.Init (Water_Program);
        Game_Utils.Game_Log ("Water_Program initialized.");
        Water_Shader_Manager.Set_K_Diff ((0.03, 0.50, 0.20, 0.75));
        Water_Shader_Manager.Set_K_Spec ((0.5, 0.5, 0.5, 1.0));
        Water_Shader_Manager.Set_Ambient_Light_Colour ((0.0125, 0.0125, 0.0125));
        Water_Shader_Manager.Set_Cube_Texture (3);

        Game_Utils.Game_Log ("Manifold shaders initialized.");
        Free_Manifold_Mesh_Data;
        if not Mesh_Loader.Load_Mesh_Data_Only
          ("src/meshes/ramp_may_2014.apg", Ramp_Mesh_Points,
            Ramp_Mesh_Texcoords, Ramp_Mesh_Normals, Ramp_Mesh_Point_Count) then
            raise Manifold_Exception with
              "Manifold.Init_Manifold error loading ramp mesh data from file "
              & "src/meshes/ramp_may_2014.apg";
        end if;
        Game_Utils.Game_Log ("ramp_may_2014.apg loaded.");

        if not Mesh_Loader.Load_Mesh_Data_Only ("src/meshes/ramp_smooth.apg",
                                                Points, Texcoords,
                                                Ramp_Mesh_Smooth_Normals,
                                                Points_Count) then
            raise Manifold_Exception with
              "Manifold.Init_Manifold error loading ramp mesh data from file "
              & "src/meshes/ramp_smooth.apg";
        end if;
        Game_Utils.Game_Log ("ramp_smooth.apg loaded.");

        if not Mesh_Loader.Load_Mesh_Data_Only
          ("src/meshes/water.apg", Water_Mesh_Points, Water_Mesh_Texcoords,
            Water_Mesh_Normals, Water_Mesh_Point_Count) then
            raise Manifold_Exception with
              "Manifold.Init_Manifold error loading ramp mesh data from file "
              & "src/meshes/water.apg";
        end if;
        Game_Utils.Game_Log ("water.apg loaded.");
        Game_Utils.Game_Log ("Manifold initialized.");

    end Init;

    --  ----------------------------------------------------------------------------

    function Is_Tile_Valid (Col, Row : Integer) return Boolean is
        use Batch_Manager;
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
    --      pragma Warnings (off);
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
                if aString'Length < Batch_Manager.Max_Cols then
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
                        Tile_List.Append (aChar);
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
                             Texture_List : in out GL_Maths.Integers_List) is
        use Ada.Strings;
        Header     : constant String := Get_Line (File);
        Code_0     : constant Integer := Character'Pos ('0');
        Code_a     : constant Integer := Character'Pos ('a');
        Cols       : Integer := 0;
        Rows       : Integer := 0;
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
                        Texture_List.Append (Tex_Int);
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
        if OK then
            OK := Add_Tiles_To_Batches;
        end if;
        return OK;
    end Load_Textures;

    --  ------------------------------------------------------------------------

    function Load_Tiles (File : File_Type) return Boolean is
        use Ada.Strings;
        use Batch_Manager;
        use Settings;
        aLine    : constant String := Get_Line (File);
        Pos1     : Natural;
        Pos2     : Natural;
        Result   : Boolean := False;
    begin
        Game_Utils.Game_Log ("Loading tiles and generating manifold from FP...");
        Put_Line ("Manifold.Load_Tiles loading tiles ");
        Pos1 := Fixed.Index (aLine, " ") + 1;
        if Fixed.Index (aLine, "facings ") = 0 then
            raise Manifold_Parsing_Exception with
              "Invalid format, ""facings"" expected: " & aLine (1 .. Pos1);
        end if;

        Pos2 := Fixed.Index (aLine (Pos1 + 1 .. aLine'Last), "x");

        Max_Cols := Integer'Value (aLine (Pos1 .. Pos2 - 1));
        Max_Rows := Integer'Value (aLine (Pos2 + 1 .. aLine'Last));
        Total_Tiles := Max_Rows * Max_Cols;
        Game_Utils.Game_Log (" Maximum columns " & Integer'Image (Max_Cols) &
                               ", maximum rows " & Integer'Image (Max_Rows) &
                               ", total tiles " & Integer'Image (Total_Tiles));
        Batches_Across :=
          Integer (Float'Ceiling (Float (Max_Cols) / Float (Tile_Batch_Width)));
        Batches_Down :=
          Integer (Float'Ceiling (Float (Max_Rows) / Float (Tile_Batch_Width)));
        Batch_Split_Count := Integer (Batches_Across * Batches_Down);
        Game_Utils.Game_Log
          (Integer'Image (Batch_Split_Count) &
             " batches of width " & Integer'Image (Settings.Tile_Batch_Width) &
             "; batches across " & Integer'Image (Batches_Across) &
             " down " & Integer'Image (Batches_Down));

        Parse_Facings_By_Row (File, Max_Rows, Max_Cols, Tile_Facings);

        Load_Int_Rows (File, "textures", Tile_Textures);
        Load_Char_Rows (File, "types", Tile_Types);
        Load_Int_Rows (File, "heights", Tile_Heights);

        Load_Palette_File_Names (File);
        Put_Line ("Manifold.Load_Tiles file names loaded.");
        Game_Utils.Game_Log ("Palette file names: " &
                               To_String (Diff_Palette_Name)
                             & ", " & To_String (Spec_Palette_Name));
        if Load_Textures then
            Result := Add_Tiles_To_Batches;
        end if;
        Add_Dummy_Manifold_Lights;
        return Result;

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Manifold.Load_Tiles!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
            return False;
    end Load_Tiles;

    --  ----------------------------------------------------------------------------

    function Number_Of_Tiles return Integer is
    begin
        return Total_Tiles;
    end Number_Of_Tiles;

    --  ----------------------------------------------------------------------------

    procedure Parse_Facings_By_Row (File : File_Type;
                                    Max_Rows, Max_Cols : Integer;
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
        Batch_Manager.Max_Cols := 0;
        Batch_Manager.Max_Rows := 0;
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
