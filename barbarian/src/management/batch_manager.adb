
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Pixels;

with Utilities;

with Game_Utils;
with GL_Maths;
with GL_Utils;
with Mesh_Loader;
with Settings;
with Shader_Attributes;

package body Batch_Manager is

    type Tile_Side is (North_Side, East_Side, South_Side, West_Side);

    Batches_Data           : Batches_List;
    Static_Lights_List     : Static_Light_Vector;
    Atlas_Factor           : constant Single := 0.25;
    Sets_In_Atlas_Row      : constant Positive := 4;
    ST_Offset              : constant Single := 8.0 / 2048.0;
    Ramp_Mesh_Points         : GL_Maths.Vec3_List;
    Ramp_Mesh_Normals        : GL_Maths.Vec3_List;
    Ramp_Mesh_Smooth_Normals : GL_Maths.Vec3_List;
    Ramp_Mesh_Texcoords      : GL_Maths.Vec2_List;
    Water_Mesh_Points        : GL_Maths.Vec3_List;
    Water_Mesh_Normals       : GL_Maths.Vec3_List;
    Water_Mesh_Texcoords     : GL_Maths.Vec2_List;
    Ramp_Mesh_Point_Count    : Integer := 0;  --  Set by load_mesh_data_only
    Water_Mesh_Point_Count   : Integer := 0;  --  Set by load_mesh_data_only
    Total_Points             : Integer := 0;

    function Check_For_OOO (Batch_Index : Positive) return Boolean;
    procedure Set_AABB_Dimensions (aBatch : in out Batch_Meta);
    procedure Update_AABB_Dimensions (aBatch : in out Batch_Meta;
                                      Point_List : GL_Maths.Vec3_List);
    procedure Set_Tex_Coords (aBatch : in out Batch_Meta;
                              aTile : Tiles_Manager.Tile_Data;
                              Side : Tile_Side; Level : Natural);

    --  -------------------------------------------------------------------------

    procedure Add_Batch (Data : Batch_Meta) is
    begin
        Batches_Data.Append (Data);
    end Add_Batch;

    --  -------------------------------------------------------------------------

    procedure Add_East_Points (aBatch : in out Batch_Meta;  Height : Integer;
                               Tile_Row, Tile_Col : Positive) is
        use Tiles_Manager;
        aTile    : constant Tile_Data := Get_Tile (Tile_Row, Tile_Col);
        N_Height : Integer;
        Diff     : Integer;
        X        : Single;
        Y        : Single;
        Z        : Single;
    begin
        N_Height := aTile.Height;
        if aTile.Tile_Type = '~' then
            N_Height := N_Height - 1;
        end if;
        Diff := Height - N_Height;
        --  remove bit behind stairs from construction list
        if aTile.Tile_Type = '/' and then aTile.Facing = 'E' then
            Diff := Diff - 1;
        end if;

        for level in -Diff .. -1 loop
            X := Single (2 * Col - 1);
            Y := Single (2 * (Height + level + 2));
            Z := Single (2 * Tile_Row + 1);
            aBatch.Points.Append ((X, Y, Z));

            Z := Single (2 * Tile_Row - 1);
            aBatch.Points.Append ((X, Y, Z));
            aBatch.Points.Append ((X, Y, Z));

            Z := Single (2 * Tile_Row + 1);
            aBatch.Points.Append ((X, Y, Z));

            Y := Single (2 * (Height + level + 2));
            aBatch.Points.Append ((X, Y, Z));

            for index in 1 .. 6 loop
                aBatch.Normals.Append ((0.0, 0.0, -1.0));
            end loop;

            if level >= 0 then
                Set_Tex_Coords (aBatch, aTile, East_Side, level);
            else
                raise Batch_Manager_Exception with
                  "Add_East_Points, invalid level value: " &
                  Integer'Image (level);
            end if;
        end loop;

    end Add_East_Points;

    --  -------------------------------------------------------------------------

    procedure Add_North_Points (aBatch   : in out Batch_Meta; Height : Integer;
                                Tile_Row, Tile_Col : Positive) is
        use Tiles_Manager;
        aTile    : constant Tile_Data := Get_Tile (Tile_Row, Tile_Col);
        N_Height : Integer;
        Diff     : Integer;
        X        : Single;
        Y        : Single;
        Z        : Single;
    begin
        N_Height := aTile.Height;
        if aTile.Tile_Type = '~' then
            N_Height := N_Height - 1;
        end if;
        Diff := Height - N_Height;

        if aTile.Tile_Type = '/' and then aTile.Facing = 'N' then
            Diff := Diff - 1;
        end if;

        for level in -Diff .. -1 loop
            X := Single (2 * Col + 1);
            Y := Single (2 * (Height + level + 1));
            Z := Single (2 * Tile_Row + 1);
            aBatch.Points.Append ((X, Y, Z));  --  1
            X := Single (2 * Col - 1);
            aBatch.Points.Append ((X, Y, Z));  --  2
            Y := Single (2 * (Height + level));
            aBatch.Points.Append ((X, Y, Z));   --  3

            aBatch.Points.Append ((X, Y, Z));   --  4
            X := Single (2 * Col + 1);
            aBatch.Points.Append ((X, Y, Z));   --  5
            Y := Single (2 * (Height + level + 1));
            aBatch.Points.Append ((X, Y, Z));    --  6
            aBatch.Point_Count := aBatch.Point_Count + 6;

            for index in 1 .. 6 loop
                aBatch.Normals.Append ((0.0, 0.0, 1.0));
            end loop;

            if level >= 0 then
                Set_Tex_Coords (aBatch, aTile, North_Side, Abs (level));
            else
                raise Batch_Manager_Exception with
                  "Add_North_Points, invalid level value: " &
                  Integer'Image (level);
            end if;
        end loop;

    end Add_North_Points;

    --  -------------------------------------------------------------------------

    procedure Add_Sides_Count (Tiles       : Tiles_Manager.Tile_2D_List;
                               theBatch    : in out Batch_Meta;
                               Tile_Height, Row_Index, Col_Index : Positive) is
        use Tiles_Manager;
        use Tile_Row_Package;
        use Tile_Column_Package;
        N_Row       : Tile_Column_List := Tiles (Row_Index);
        N_Tile      : Tile_Data := Get_Tile (Row_Index, Col_Index);
        N_Height    : Integer := 0;
        Diff        : Integer := 0;

        procedure Add_Point_Count (Diff : Integer) is
        begin
            if Diff > 0 then
                theBatch.Point_Count := theBatch.Point_Count + 2 * Diff;
                Total_Points := Total_Points + 2 * Diff;
            end if;
        end Add_Point_Count;

    begin
        --          Game_Utils.Game_Log ("Batch_Manager.Add_Sides_Count, Row_Index, Col_Index: " &
        --                    Integer'Image (Row_Index) & ", " & Integer'Image (Col_Index));
        if Row_Index > 1 then
            N_Row := Tiles.Element (Row_Index - 1);
            N_Tile := N_Row.Element (1);
            N_Height := N_Tile.Height;
            if N_Tile.Tile_Type = '~' then
                N_Height := N_Height - 1;
            end if;
            Diff := Tile_Height - N_Height;

            --  Remove bit behind stairs from construction list
            if N_Tile.Tile_Type = '/' and then
              N_Tile.Facing = 'S' then
                Diff := Diff - 1;
            end if;
            Add_Point_Count (Diff);
        end if;

        if Row_Index < Tiles.Last_Index then
            --          Game_Utils.Game_Log ("Batch_Manager.Add_Sides_Count, Row_Index: " &
            --                    Integer'Image (Row_Index) & " of " & Integer'Image (Tiles.Last_Index));
            --              N_Row := Tiles.Element (Row_Index + 1);
            --          Game_Utils.Game_Log ("Batch_Manager.Add_Sides_Count, N_Row.Last_Index: " &
            --                    Integer'Image (Integer(N_Row.Last_Index)));
            N_Tile := N_Row.Element (1);
            N_Height := N_Tile.Height;
            if N_Tile.Tile_Type = '~' then
                N_Height := N_Height - 1;
            end if;
            Diff := Tile_Height - N_Height;

            if N_Tile.Tile_Type = '/' and then
              N_Tile.Facing = 'N' then
                Diff := Diff - 1;
            end if;
            Add_Point_Count (Diff);
        end if;

        if Col_Index > 1 then
            N_Tile := N_Row.Element (Col_Index - 1);
            N_Height := N_Tile.Height;
            if N_Tile.Tile_Type = '~' then
                N_Height := N_Height - 1;
            end if;
            Diff := Tile_Height - N_Height;

            if N_Tile.Tile_Type = '/' and then
              N_Tile.Facing = 'E' then
                Diff := Diff - 1;
            end if;
            Add_Point_Count (Diff);
        end if;

        if Col_Index < N_Row.Last_Index then
            N_Tile := N_Row.Element (Col_Index + 1);
            N_Height := N_Tile.Height;
            if N_Tile.Tile_Type = '~' then
                N_Height := N_Height - 1;
            end if;
            Diff := Tile_Height - N_Height;

            if N_Tile.Tile_Type = '/' and then
              N_Tile.Facing = 'W' then
                Diff := Diff - 1;
            end if;
            Add_Point_Count (Diff);
        end if;

    end Add_Sides_Count;

    --  -------------------------------------------------------------------------

    procedure Add_South_Points (aBatch : in out Batch_Meta; Height : Integer;
                                Tile_Row, Tile_Col : Positive) is
        use Tiles_Manager;
        aTile    : constant Tile_Data := Get_Tile (Tile_Row, Tile_Col);
        N_Height : Integer;
        Diff     : Integer;
        X        : Single;
        Y        : Single;
        Z        : Single;
    begin
        N_Height := aTile.Height;
        if aTile.Tile_Type = '~' then
            N_Height := N_Height - 1;
        end if;
        Diff := Height - N_Height;
        --  remove bit behind stairs from construction list
        if aTile.Tile_Type = '/' and then aTile.Facing = 'S' then
            Diff := Diff - 1;
        end if;

        for level in -Diff .. -1 loop
            X := Single (2 * Col - 1);
            Y := Single (2 * (Height + level + 2));
            Z := Single (2 * Tile_Row - 1);
            aBatch.Points.Append ((X, Y, Z));

            X := Single (2 * Col + 1);
            aBatch.Points.Append ((X, Y, Z));

            Y := Single (2 * (Height + level));
            aBatch.Points.Append ((X, Y, Z));
            aBatch.Points.Append ((X, Y, Z));

            X := Single (2 * Col - 1);
            aBatch.Points.Append ((X, Y, Z));

            Y := Single (2 * (Height + level + 2));
            aBatch.Points.Append ((X, Y, Z));
            for index in 1 .. 6 loop
                aBatch.Normals.Append ((0.0, 0.0, -1.0));
            end loop;

            if level >= 0 then
                Set_Tex_Coords (aBatch, aTile, South_Side, level);
            else
                raise Batch_Manager_Exception with
                  "Add_South_Points, invalid level value: " &
                  Integer'Image (level);
            end if;
        end loop;

    end Add_South_Points;

    --  -------------------------------------------------------------------------

    procedure Add_Static_Light (Row, Col : Int;
                                Tile_Height_Offset : Integer;
                                Offset_Pos, Diffuse, Specular  : Singles.Vector3;
                                Light_Range                    : Single) is
        use Batches_Package;
        S_Row         : constant Single := Single (Row);
        S_Col         : constant Single := Single (Col);
        X             : Single := 2.0 * S_Col + Offset_Pos (GL.X);
        Y             : Single :=
                          2.0 * (Tiles_Manager.Get_Tile_Height
                                 (S_Row, S_Col, False, False) + Single (Tile_Height_Offset))
                          + Offset_Pos (GL.Y);
        Z             : Single := 2.0 * S_Row + Offset_Pos (GL.Z);
        Total_Batches : Integer := Batches_Across * Batches_Down;
        --          Batch_Index   : Positive := Batches.First_Index;
        New_Light     : Static_Light_Data;

        procedure Process_Batch (Curs : Batches_Package.Cursor) is
            Sorted  : Boolean := False;
            aBatch  : Batch_Meta := Element (Curs);
        begin
            Sorted := False;
            aBatch.Static_Light_Indices.Append (Static_Lights_List.Last_Index);
            Update_Batch (To_Index (Curs), aBatch);
            while not Sorted loop
                Sorted := Check_For_OOO (To_Index (Curs));
            end loop;
        end Process_Batch;
    begin
        if not Tiles_Manager.Is_Tile_Valid (Row, Col) then
            raise Batch_Manager_Exception with
              "Batch_Manager.Add_Static_Light invalid tile";
        end if;

        New_Light.Position := Offset_Pos;
        New_Light.Diffuse := Diffuse;
        New_Light.Specular := Specular;
        New_Light.Light_Range := Light_Range;
        New_Light.Row := Row;
        New_Light.Column := Col;
        Static_Lights_List.Append (New_Light);

--          Game_Utils.Game_Log ("Batch_Manager.Add_Static_Light iterating");
        Batches.Iterate (Process_Batch'Access);
    end Add_Static_Light;

    --  ------------------------------------------------------------------------

    procedure Add_West_Points (aBatch : in out Batch_Meta; Height : Integer;
                               Tile_Row, Tile_Col : Positive) is
        use Tiles_Manager;
        aTile    : constant Tile_Data := Get_Tile (Tile_Row, Tile_Col);
        N_Height : Integer;
        Diff     : Integer;
        X        : Single;
        Y        : Single;
        Z        : Single;
    begin
        N_Height := aTile.Height;
        if aTile.Tile_Type = '~' then
            N_Height := N_Height - 1;
        end if;
        Diff := Height - N_Height;
        --  remove bit behind stairs from construction list
        if aTile.Tile_Type = '/' and then aTile.Facing = 'W' then
            Diff := Diff - 1;
        end if;

        for level in -Diff .. -1 loop
            X := Single (2 * Col + 1);
            Y := Single (2 * (Height + level + 2));
            Z := Single (2 * Tile_Row - 1);
            aBatch.Points.Append ((X, Y, Z));

            Z := Single (2 * Tile_Row + 1);
            aBatch.Points.Append ((X, Y, Z));
            aBatch.Points.Append ((X, Y, Z));

            Z := Single (2 * Tile_Row - 1);
            aBatch.Points.Append ((X, Y, Z));

            Y := Single (2 * (Height + level + 2));
            aBatch.Points.Append ((X, Y, Z));

            for index in 1 .. 6 loop
                aBatch.Normals.Append ((0.0, 0.0, -1.0));
            end loop;

            if level >= 0 then
                Set_Tex_Coords (aBatch, aTile, West_Side, Abs (level));
            else
                raise Batch_Manager_Exception with
                  "Add_West_Points, invalid level value: " &
                  Integer'Image (level);
            end if;
        end loop;

    end Add_West_Points;

    --  -------------------------------------------------------------------------

    function Batches return Batches_List is
    begin
        return Batches_Data;
    end Batches;

    --  ----------------------------------------------------------------------------
    --  Out-of-order check. swap on first out-of-order and returns false.
    --  Multiple calls required to sort entire list
    function Check_For_OOO (Batch_Index : Positive) return Boolean is
        use Maths.Single_Math_Functions;
        use Tiles_Manager.Tile_Row_Package;
        use GL_Maths.Indices_Package;
        Half_Batch_Width     : constant Int :=
                                 Int (Settings.Tile_Batch_Width / 2);
        This_Batch           : Batch_Meta := Batches.Element (Batch_Index);
        Batches_Dn           : constant Int :=
                                 Int (Batch_Index) / Int (Batches_Across);
        Batches_Ac           : constant Int := Int (Batch_Index) -
                                 Batches_Dn * Int (Batches_Across);
        Batch_Centre_Row     : constant Int :=
                                 Batches_Dn * Int (Settings.Tile_Batch_Width) +
                                 Half_Batch_Width;
        Batch_Centre_Col     : constant Int :=
                                 Batches_Ac * Int (Settings.Tile_Batch_Width) +
                                 Half_Batch_Width;
        Batch_Light_Indices  : GL_Maths.Indices_List :=
                                 This_Batch.Static_Light_Indices;
        Current_Light_Index  : Positive := Batch_Light_Indices.First_Index;
        Current_Light_Cursor : GL_Maths.Indices_Package.Cursor :=
                                 Batch_Light_Indices.To_Cursor (Current_Light_Index);
        Current_Light        : Static_Light_Data :=
                                 Static_Lights_List.Element (Current_Light_Index);
        Prev_Light_Index     : Positive;
        Next_Light_Index     : Positive;
        Next_Light           : Static_Light_Data;
        Prev_Light_Cursor    : GL_Maths.Indices_Package.Cursor;
        Next_Light_Cursor    : GL_Maths.Indices_Package.Cursor;
        Curr_Row             : Int;
        Curr_Col             : Int;
        Next_Row             : Int;
        Next_Col             : Int;
        Curr_Row_Dist        : Single;
        Curr_Col_Dist        : Single;
        Next_Row_Dist        : Single;
        Next_Col_Dist        : Single;
        Curr_Dist            : Single;
        Next_Dist            : Single;
    begin
        if Current_Light_Index < Static_Lights.Last_Index then

            while Has_Element (Next_Light_Cursor) loop
                Current_Light := Static_Lights_List.Element (Current_Light_Index);
                Next (Next_Light_Cursor);
                Next_Light_Index := Element (Next_Light_Cursor);
                Next_Light := Static_Lights_List.Element (Next_Light_Index);

                Curr_Row := Current_Light.Row;
                Curr_Col := Current_Light.Column;
                Next_Row := Next_Light.Row;
                Next_Col := Next_Light.Row;
                Curr_Row_Dist := Single (Batch_Centre_Row - Curr_Row);
                Curr_Col_Dist := Single (Batch_Centre_Col - Curr_Col);
                Next_Row_Dist := Single (Batch_Centre_Row - Next_Row);
                Next_Col_Dist := Single (Batch_Centre_Col - Next_Col);
                Curr_Dist := Sqrt (Curr_Row_Dist ** 2 + Curr_Col_Dist ** 2);
                Next_Dist := Sqrt (Next_Row_Dist ** 2 + Next_Col_Dist ** 2);

                if Next_Dist < Curr_Dist then
                    Batch_Light_Indices.Swap (Next_Light_Cursor, Current_Light_Cursor);
                    if Prev_Light_Cursor /= Batch_Light_Indices.First then
                        Prev_Light_Cursor := Next_Light_Cursor;
                        --              else
                        --                 Batch_Light_Indices.First_Element := Next_Light_Cursor;
                    end if;
                end if;
                Prev_Light_Index := Current_Light_Index;
                Current_Light_Index := Next_Light_Index;
                Current_Light_Cursor := Next_Light_Cursor;
                Next_Light_Index := Element (Next_Light_Cursor);
            end loop;

            This_Batch.Static_Light_Indices := Batch_Light_Indices;
            Batches_Data.Replace_Element (Batch_Index, This_Batch);
        end if;
        return True;

    exception
        when anError : Constraint_Error =>
            Put ("Batch_Manager.Check_For_OOO constraint error: ");
            Put_Line (Exception_Information (anError));
            raise;
        when anError :  others =>
            Put_Line ("An exception occurred in Batch_Manager.Check_For_OOO.");
            Put_Line (Exception_Information (anError));
            raise;
    end Check_For_OOO;

    --  ----------------------------------------------------------------------------

    procedure Clear is
    begin
        Batches_Data.Clear;
        Ramp_Mesh_Points.Clear;
        Ramp_Mesh_Normals.Clear;
        Ramp_Mesh_Smooth_Normals.Clear;
        Ramp_Mesh_Texcoords.Clear;
        Water_Mesh_Points.Clear;
        Water_Mesh_Normals.Clear;
        Water_Mesh_Texcoords.Clear;
        --          Ramp_Mesh_Point_Count := 0;  --  Not used?
        Water_Mesh_Point_Count := 0;
        Total_Points := 0;
    end Clear;

    --  ----------------------------------------------------------------------------

    procedure Free_Batch_Data (Batch_Index : Positive) is
        theBatch : Batch_Meta := Batches.Element (Batch_Index);
    begin
        theBatch.Points.Clear;
        theBatch.Ramp_Points.Clear;
        theBatch.Water_Points.Clear;
        Batches_Data.Replace_Element  (Batch_Index, theBatch);
    end Free_Batch_Data;

    --  -------------------------------------------------------------------------

    procedure Generate_Points (aBatch : in out Batch_Meta;
                               Tiles  : Tiles_Manager.Tile_2D_List) is
        use Tiles_Manager;
        use Tile_Row_Package;
        use GL_Maths;
        Tile_Row  : Tile_Column_List := Tiles.First_Element;
        aTile     : Tile_Data;
        --          Row       : Positive;
        --          Column    : Positive;
        Height    : Integer;
        X         : Single;
        Y         : Single;
        Z         : Single;
        Atlas_Row : Integer;
        Atlas_Col : Integer;

        procedure Add_Tex_Coords (S_Offset, T_Offset : Single)  is
            S  : constant Single := (Single (Atlas_Col) + S_Offset) * Atlas_Factor;
            T  : constant Single := (Single (Atlas_Row) + T_Offset) * Atlas_Factor;
        begin
            aBatch.Tex_Coords.Append ((S - ST_Offset, T - ST_Offset));
        end Add_Tex_Coords;

    begin
        if not Is_Empty (Tiles) then
            for Row_Index in Tiles.First_Index .. Tiles.Last_Index loop
                Tile_Row := Tiles (Row_Index);
                for Col_Index in Tile_Row.First_Index .. Tile_Row.Last_Index loop
                    --                  Game_Utils.Game_Log ("Batch_Manager.Generate_Points Tile_Index"
                    --                                       & Integer'Image (Tile_Index));
                    aTile := Tile_Row.Element (Col_Index);
                    Height := aTile.Height;
                    X := Single (2 * Col_Index);
                    Y := Single (2 * Height);
                    Z := Single (2 * Row_Index);

                    if aTile.Tile_Type = '~' then
                        Height := Height - 1;
                    elsif aTile.Tile_Type /= '/' then  -- Flat tile
                        --  floor FR, FL, BL, BL, BR, FR
                        aBatch.Points.Append ((X + 1.0, Y, Z - 1.0));  -- FR
                        aBatch.Points.Append ((X - 1.0, Y, Z - 1.0));  -- FL
                        aBatch.Points.Append ((X - 1.0, Y, Z + 1.0));  -- BL
                        aBatch.Points.Append ((X - 1.0, Y, Z + 1.0));  -- BL
                        aBatch.Points.Append ((X + 1.0, Y, Z + 1.0));  -- BR
                        aBatch.Points.Append ((X + 1.0, Y, Z - 1.0));  -- FR
                        aBatch.Point_Count := aBatch.Point_Count + 6;

                        for index in 1 .. 6 loop
                            aBatch.Normals.Append ((0.0, 1.0, 0.0));
                            aBatch.Normal_Count := aBatch.Normal_Count + 1;
                        end loop;

                        --                          Atlas_Row := Tile_Index / Sets_In_Atlas_Row;
                        --                          Atlas_Col := (Tile_Index - Atlas_Row + 1) * Sets_In_Atlas_Row;
                        Atlas_Row := Row_Index;
                        Atlas_Col := Col_Index;
                        Add_Tex_Coords (0.5, 1.0);
                        Add_Tex_Coords (0.0, 1.0);
                        Add_Tex_Coords (0.0, 0.5);
                        Add_Tex_Coords (0.0, 0.5);
                        Add_Tex_Coords (0.5, 0.5);
                        Add_Tex_Coords (0.5, 1.0);
                        --     aBatch.Tex_Coord_Count := aBatch.Tex_Coord_Count;
                    end if;

                    --  check for higher neighbour to north (walls belong to the lower tile)
                    if Row_Index < Tiles.Last_Index then
                        Add_North_Points (aBatch, Height, Row_Index, Col_Index);
                    end if;
                    if Row_Index > 1 then
                        Add_South_Points (aBatch, Height, Row_Index, Col_Index);
                    end if;
                    if Col_Index < Tile_Row.Last_Index then
                        Add_West_Points (aBatch, Height, Row_Index, Col_Index);
                    end if;
                    if Col_Index > 1 then
                        Add_East_Points (aBatch, Height, Row_Index, Col_Index);
                    end if;
                end loop;
            end loop;

            aBatch.Points_VAO.Initialize_Id;
            GL_Utils.Bind_VAO (aBatch.Points_VAO);
            aBatch.Points_VBO := GL_Utils.Create_3D_VBO
              (GL_Maths.To_Vector3_Array (aBatch.Points));
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (Shader_Attributes.Attrib_VP, 3, Single_Type, False, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VP);

            Update_AABB_Dimensions  (aBatch, aBatch.Points);
            aBatch.Point_Count := Integer (aBatch.Points.Length);
            --              aBatch.Points.Clear;

            aBatch.Normals_VBO := GL_Utils.Create_3D_VBO
              (GL_Maths.To_Vector3_Array (aBatch.Normals));
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (Shader_Attributes.Attrib_VN, 3, Single_Type, False, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VN);
            aBatch.Normals.Clear;

            aBatch.Tex_Coords_VBO := GL_Utils.Create_2D_VBO
              (GL_Maths.To_Vector2_Array (aBatch.Tex_Coords));
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (Shader_Attributes.Attrib_VT, 2, Single_Type, False, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VT);
            aBatch.Tex_Coords.Clear;
        end if;

    end Generate_Points;

    --  ----------------------------------------------------------------------------

    procedure Generate_Ramps (aBatch : in out Batch_Meta;
                              Tiles  : Tiles_Manager.Tile_2D_List) is
        use Singles;
        use Maths;
        use Tiles_Manager;
        use Tiles_Manager.Tile_Row_Package;
        use Tiles_Manager.Tile_Column_Package;
        use GL_Maths;
        use Vec2_Package;
        use Vec3_Package;
        aTile          : Tile_Data;
        aRow           : Tile_Column_List;
        Facing         : Character;
        Height         : Integer;
        Deg            : Degree;
        Model_Matrix   : Matrix4 := Identity4;
        Rot_Matrix     : Matrix4 := Identity4;
        Curs_N         : Vec3_Cursor;
        Curs_P         : Vec3_Cursor;
        Curs_T         : Vec2_Package.Cursor;
        Curs_S         : Vec3_Cursor;
        aNormal        : Vector3;
        aPoint         : Vector3;
        aSmooth_Normal : Vector3;
        VPF            : Singles.Vector4;
        VNF            : Singles.Vector4;
        Smooth_VNF     : Singles.Vector4;

    begin
        if not Is_Empty (Tiles) then
            for Row_Index in Tiles.First_Index .. Tiles.Last_Index loop
                aRow := Tiles.Element (Row_Index);
                for Col_Index in aRow.First_Index .. aRow.Last_Index loop
                    aTile := aRow.Element (Col_Index);
                    Height := aTile.Height;
                    Facing := aTile.Facing;

                    case Facing is
                    when 'N' => Deg := Degree (0);
                    when 'W' => Deg := Degree (90);
                    when 'S' => Deg := Degree (180);
                    when 'E' => Deg := Degree (270);
                    when others =>
                        raise Batch_Manager_Exception with
                          "Batch_Manager.Generate_Ramps, invalid Facing value";
                    end case;

                    if aTile.Tile_Type = '/' then
                        --                      Put_Line ("Batch_Manager.Generate_Ramps, Tile_Type " &
                        --                              aTile.Tile_Type);
                        --  Put each vertex point into world space
                        Rot_Matrix := Rotate_Y_Degree (Rot_Matrix, Deg);
                        Model_Matrix := Translation_Matrix
                          ((Single (2 * Col_Index),
                           Single (2 * Height), Single (2 * Row_Index)));
                        Curs_P := Ramp_Mesh_Points.First;
                        Curs_N := Ramp_Mesh_Normals.First;
                        Curs_T := Ramp_Mesh_Texcoords.First;
                        Curs_S := Ramp_Mesh_Smooth_Normals.First;
                        while Has_Element (Curs_P) loop
                            aPoint := Element (Curs_P);
                            aNormal := Element (Curs_N);
                            aSmooth_Normal := Element (Curs_S);
                            VPF := Model_Matrix * Singles.To_Vector4 (aPoint);
                            VNF := Model_Matrix * Singles.To_Vector4 (aNormal);
                            Smooth_VNF := Model_Matrix * Singles.To_Vector4 (aSmooth_Normal);

                            aBatch.Ramp_Points.Append (To_Vector3 (VPF));
                            aBatch.Ramp_Normals.Append (To_Vector3 (VNF));
                            aBatch.Ramp_Tex_Coords.Append (Element (Curs_T));
                            aBatch.Ramp_Smooth_Normals.Append (To_Vector3 (Smooth_VNF));

                            Next (Curs_N);
                            Next (Curs_P);
                            Next (Curs_T);
                            Next (Curs_S);
                        end loop;
                        aBatch.Ramp_Point_Count :=
                          aBatch.Ramp_Point_Count + Ramp_Mesh_Point_Count;
                    end if;
                end loop;
            end loop;

            --              Utilities.Print_GL_Array3
            --                ("aBatch.Ramp_Points", GL_Maths.To_Vector3_Array (aBatch.Ramp_Points));
            aBatch.Ramp_VBO := GL_Utils.Create_3D_VBO
              (GL_Maths.To_Vector3_Array (aBatch.Ramp_Points));
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (Shader_Attributes.Attrib_VP, 3, Single_Type, False, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VP);

            Update_AABB_Dimensions  (aBatch, aBatch.Ramp_Points);
            aBatch.Ramp_Point_Count := Integer (aBatch.Ramp_Points.Length);
            aBatch.Ramp_Points.Clear;

            aBatch.Ramp_VAO.Initialize_Id;
            aBatch.Ramp_Normals_VBO := GL_Utils.Create_3D_VBO
              (GL_Maths.To_Vector3_Array (aBatch.Ramp_Normals));
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (Shader_Attributes.Attrib_VN, 3, Single_Type, False, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VN);
            aBatch.Ramp_Normals.Clear;

            aBatch.Ramp_Texcoords_VBO := GL_Utils.Create_2D_VBO
              (GL_Maths.To_Vector2_Array (aBatch.Ramp_Tex_Coords));
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (Shader_Attributes.Attrib_Vt, 2, Single_Type, False, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VT);
            aBatch.Ramp_Tex_Coords.Clear;

            aBatch.Ramp_Smooth_Normals_VBO := GL_Utils.Create_3D_VBO
              (GL_Maths.To_Vector3_Array (aBatch.Ramp_Smooth_Normals));
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (Shader_Attributes.Attrib_VN, 3, Single_Type, False, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VN);
            aBatch.Ramp_Smooth_Normals.Clear;
        end if;

    end Generate_Ramps;

    --  ----------------------------------------------------------------------------

    procedure Generate_Water (aBatch : in out Batch_Meta;
                              Tiles  : Tiles_Manager.Tile_2D_List) is
        use Singles;
        use Maths;
        use Tiles_Manager;
        use GL_Maths;
        use Vec3_Package;
        aTile          : Tile_Data;
        aRow           : Tile_Column_List;
        Facing         : Character;
        Height         : Integer;
        Deg            : Degree;
        Model_Matrix   : Matrix4 := Identity4;
        Rot_Matrix     : Matrix4 := Identity4;
        Curs_P         : Cursor;
        aWater_Point   : Vector3;
        VPF            : Singles.Vector4;
    begin
        for Row_Index in Tiles.First_Index .. Tiles.Last_Index loop
            aRow := Tiles.Element (Row_Index);
            for Col_Index in aRow.First_Index .. aRow.Last_Index loop
                aTile := aRow.Element (Col_Index);
                Height := aTile.Height;
                Facing := aTile.Facing;

                case Facing is
                    when 'N' => Deg := Degree (0);
                    when 'W' => Deg := Degree (90);
                    when 'S' => Deg := Degree (180);
                    when 'E' => Deg := Degree (270);
                    when others =>
                        raise Batch_Manager_Exception with
                          "Generate_Ramps, invalid Facing value";
                end case;

                if aTile.Tile_Type = '~' then
                    --  Put each vertex point into world space
                    Rot_Matrix := Rotate_Y_Degree (Rot_Matrix, Deg);
                    Model_Matrix := Translation_Matrix
                      ((Single (2 * Col_Index), Single (2 * Height),
                       Single (2 * Row_Index)));
                    Curs_P := Water_Mesh_Points.First;
                    while Has_Element (Curs_P) loop
                        aWater_Point := Element (Curs_P);
                        VPF := Model_Matrix * Singles.To_Vector4 (aWater_Point);
                        aBatch.Water_Points.Append (To_Vector3 (VPF));
                        Next (Curs_P);
                    end loop;  --  end for each Water_Point
                    aBatch.Water_Point_Count :=
                      aBatch.Water_Point_Count + Water_Mesh_Point_Count;
                end if;
            end loop;  --  end for each row
        end loop;  --  end for each Tile

        --              Put_Line ("Batch_Manager.Generate_Water, Water_Mesh_Points size: " &
        --                         Integer'Image (Integer (Water_Mesh_Points.Length)));
        --              Put_Line ("Batch_Manager.Generate_Water, aBatch.Water_Points size: " &
        --                         Integer'Image (Integer (aBatch.Water_Points.Length)));

        aBatch.Water_VAO.Initialize_Id;
        GL_Utils.Bind_VAO (aBatch.Water_VAO);
        aBatch.Water_VBO := GL_Utils.Create_3D_VBO
          (GL_Maths.To_Vector3_Array (aBatch.Water_Points));
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (Shader_Attributes.Attrib_VP, 3, Single_Type, False, 0, 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VP);

        aBatch.Water_Point_Count := Integer (aBatch.Water_Points.Length);
        aBatch.Water_Points.Clear;

    end Generate_Water;

    --  -------------------------------------------------------------------------

    function Get_Batch_Index (Column, Row : Positive) return Integer is
        Result : Integer := -1;
    begin
        if Column >= 0 and Column < Positive (Max_Cols) and
          Row >= 0 and Row < Positive (Max_Rows) then
            Result := (Integer (Column) + Batches_Across * Integer (Row)) /
              Settings.Tile_Batch_Width;
        end if;
        return Result;
    end Get_Batch_Index;

    --  -------------------------------------------------------------------------

    procedure Init is
        Points       : GL_Maths.Vec3_List;
        Texcoords    : GL_Maths.Vec2_List;
        Points_Count : Integer := 0;
    begin
        Clear;
        if not Mesh_Loader.Load_Mesh_Data_Only
          ("src/meshes/ramp_may_2014.apg", Ramp_Mesh_Points,
           Ramp_Mesh_Texcoords, Ramp_Mesh_Normals) then
            raise Batch_Manager_Exception with
              "Batch_Manager.Init error loading ramp mesh data from file "
              & "src/meshes/ramp_may_2014.apg";
        end if;
        Ramp_Mesh_Point_Count := Integer (Ramp_Mesh_Points.Length);

        Game_Utils.Game_Log ("Batch_Manager.Init loaded src/meshes/ramp_may_2014.apg"
                             & " Ramp_Mesh_Points.Length " & Integer'Image (Integer (Ramp_Mesh_Points.Length))
                             & " Ramp_Mesh_Texcoords.Length " & Integer'Image (Integer (Ramp_Mesh_Texcoords.Length))
                             & " Ramp_Mesh_Normals.Length " & Integer'Image (Integer (Ramp_Mesh_Normals.Length)));

        if not Mesh_Loader.Load_Mesh_Data_Only ("src/meshes/ramp_smooth.apg",
                                                Points, Texcoords,
                                                Ramp_Mesh_Smooth_Normals) then
            raise Batch_Manager_Exception with
              "Batch_Manager.Init error loading ramp mesh data from file "
              & "src/meshes/ramp_smooth.apg";
        end if;
        Game_Utils.Game_Log ("Batch_Manager.Init ramp_smooth.apg loaded."
                             & " Points.Length " & Integer'Image (Integer (Points.Length))
                             & " Texcoords.Length " & Integer'Image (Integer (Texcoords.Length))
                             & " Ramp_Mesh_Smooth_Normals.Length " & Integer'Image (Integer (Ramp_Mesh_Smooth_Normals.Length)));

        if not Mesh_Loader.Load_Mesh_Data_Only ("src/meshes/ramp_smooth.apg",
                                                Points, Texcoords,
                                                Ramp_Mesh_Smooth_Normals) then
            raise Batch_Manager_Exception with
              "Batch_Manager.Init error loading ramp mesh data from file "
              & "src/meshes/ramp_smooth.apg";
        end if;
        Game_Utils.Game_Log ("Batch_Manager.Init ramp_smooth.apg loaded.");

        if not Mesh_Loader.Load_Mesh_Data_Only
          ("src/meshes/water.apg", Water_Mesh_Points, Water_Mesh_Texcoords,
           Water_Mesh_Normals) then
            raise Batch_Manager_Exception with
              "Batch_Manager.Init error loading ramp mesh data from file "
              & "src/meshes/water.apg";
        end if;
        Water_Mesh_Point_Count := Integer (Water_Mesh_Points.Length);
        Game_Utils.Game_Log ("Batch_Manager.Init water.apg loaded.");

    end Init;

    --  -------------------------------------------------------------------------

    function Num_Points (Batch_Index : Positive) return Integer is
        aBatch : constant Batch_Meta := Batches.Element (Batch_Index);
    begin
        return aBatch.Point_Count;
    end Num_Points;

    --  -------------------------------------------------------------------------

    function Num_Ramp_Points (Batch_Index : Positive) return Integer is
        aBatch : constant Batch_Meta := Batches.Element (Batch_Index);
    begin
        return aBatch.Ramp_Point_Count;
    end Num_Ramp_Points;

    --  -------------------------------------------------------------------------

    function Num_Water_Points  (Batch_Index : Positive) return Integer is
        aBatch : constant Batch_Meta := Batches.Element (Batch_Index);
    begin
        return aBatch.Water_Point_Count;
    end Num_Water_Points;

    --  -------------------------------------------------------------------------

    procedure Regenerate_Batch (Tiles       : Tiles_Manager.Tile_2D_List;
                                Batch_Index : Positive) is
        use Tiles_Manager;
        use Tiles_Manager.Tile_Indices_Package;
        use Batches_Package;
        use Tile_Row_Package;
        use Tile_Column_Package;
        theBatch     : Batch_Meta := Batches.Element (Batch_Index);
        Tile_Indices : constant Tiles_Manager.Tile_Indices_List :=
                         theBatch.Tiles;
        Tile_Index   : Ints.Vector2 := Tile_Indices.First_Element;
        Row_Index    : Positive := Positive (Tile_Index (GL.X));
        Col_Index    : Positive := Positive (Tile_Index (GL.Y));
        Indices_Curs : Tile_Indices_Cursor := Tile_Indices.First;
        Tile_Row    : Tile_Column_List;
        aTile       : Tile_Data;
        Row         : Int := 0;
        Column      : Int := 0;
        Height      : Integer := 0;
        N_Tile      : Tile_Data;
        N_Index     : Integer := 0;
        N_Height    : Integer := 0;
        Diff        : Integer := 0;

        procedure Add_Point_Count (Diff : Integer) is
        begin
            if Diff > 0 then
                theBatch.Point_Count := theBatch.Point_Count + 2 * Diff;
                Total_Points := Total_Points + 2 * Diff;
            end if;
        end Add_Point_Count;

    begin
        Free_Batch_Data (Batch_Index);
        if Tile_Row_Package.Is_Empty (Tiles) then
            Game_Utils.Game_Log ("Regenerate_Batch, theBatch.Tiles is empty.");
            raise Batch_Manager_Exception with
              "Batch_Manager.Regenerate_Batch, theBatch.Tiles is empty.";
        end if;
        --              Put_Line ("Batch_Manager.Regenerate_Batch Max_Cols " &
        --                          Int'Image (Max_Cols));
        --         Put_Line ("Batch_Manager.Regenerate_Batch  Tile_Indices and Batches Lengths " &
        --              Integer'Image (Integer (Tile_Indices.Length)) &
        --              ", " & Integer'Image (Integer (Batches.Length)));
        if not Tile_Indices.Is_Empty then
            while Has_Element (Indices_Curs) loop
                Tile_Index := Element (Indices_Curs);
                Row_Index := Positive (Tile_Index (GL.X));
                Col_Index := Positive (Tile_Index (GL.Y));
                aTile := Get_Tile (Row_Index, Col_Index);
                Height := aTile.Height;
                if aTile.Tile_Type = '~' then
                    Height := Height - 1;
                    theBatch.Water_Point_Count :=
                      theBatch.Water_Point_Count + Water_Mesh_Point_Count;
                    Total_Points := Total_Points + Water_Mesh_Point_Count;
                elsif aTile.Tile_Type = '/' then
                    theBatch.Ramp_Point_Count :=
                      theBatch.Ramp_Point_Count + Ramp_Mesh_Point_Count;
                    Total_Points := Total_Points + Ramp_Mesh_Point_Count;
                else  --  add floor count
                    theBatch.Point_Count := theBatch.Point_Count + 6;
                    Total_Points := Total_Points + 6;
                end if;

                Add_Sides_Count (Tiles, theBatch, Height, Row_Index, Col_Index);
                Next (Indices_Curs);
            end loop;  -- over tile indices
        end if;  --  not Tiles not empty

        Generate_Points (theBatch, Tiles);
        Generate_Ramps (theBatch, Tiles);
        Generate_Water (theBatch, Tiles);
        --          Put_Line ("Batch_Manager.Regenerate_Batch Water generated");

        Batches_Data.Replace_Element (Batch_Index, theBatch);

    end Regenerate_Batch;

    --  -------------------------------------------------------------------------

    procedure Set_AABB_Dimensions (aBatch : in out Batch_Meta) is
    begin
        aBatch.AABB_Mins := (100000.0, 100000.0, 100000.0);
        aBatch.AABB_Maxs := (-100000.0, -100000.0, -100000.0);
        Update_AABB_Dimensions  (aBatch, aBatch.Points);

    end Set_AABB_Dimensions;

    --  -------------------------------------------------------------------------

    procedure Set_Tex_Coords (aBatch : in out Batch_Meta;
                              aTile : Tiles_Manager.Tile_Data;
                              Side : Tile_Side; Level : Natural) is
        Offset_Factor     : Singles.Vector2_Array (1 .. 6);
        Texture_Index     : constant Positive := aTile.Texture_Index;
        Half_Atlas_Factor : constant Single := 0.5 * Atlas_Factor;
        Atlas_Row         : constant Positive := Texture_Index / Sets_In_Atlas_Row;
        Atlas_Col         : constant Positive := Texture_Index - Atlas_Row * Sets_In_Atlas_Row;
        S                 : Single := Half_Atlas_Factor;
        T                 : Single := 0.0;
        S_Offset          : Single := 0.0;
        Side_State        : Natural := 0;
        Reverse_S         : Boolean := False;

        procedure New_Tex_Cords is
            SF       : Single := 0.0;
            TF       : Single := 0.0;
            STF      : Singles.Vector2 := (0.0, 0.0);
            T_Offset : Single := 0.0;
            SI       : Integer := 0;
            TI       : Integer := 0;
        begin
            for index in Int range 1 .. 6 loop
                SF := Offset_Factor (index) (GL.X) * Half_Atlas_Factor;
                Tf := Offset_Factor (index) (GL.Y) * Half_Atlas_Factor;
                S_Offset := S + Sf + Single (Atlas_Col) * Atlas_Factor;
                T_Offset := T + Tf + Single (Atlas_Row) * Atlas_Factor;
                Si := aBatch.Tex_Coord_Count;
                Ti := aBatch.Tex_Coord_Count;
                Sf := -Offset_Factor (index) (GL.X);
                Tf := -Offset_Factor (index) (GL.Y);
                STF := (S_Offset + Sf * ST_Offset,
                        T_Offset + Tf * ST_Offset);
                aBatch.Tex_Coords.Replace_Element (Positive (Si), STF);
                aBatch.Tex_Coord_Count := aBatch.Tex_Coord_Count + 1;
            end loop;
        end New_Tex_Cords;

    begin
        case Side is
        when North_Side => null;
        when others => Side_State := Side_State + 1;
        end case;

        case aTile.Facing is
        when 'N' => null;
        when 'E' => Side_State := Side_State + 1;
        when 'S' => Side_State := Side_State + 2;
        when 'W' => Side_State := Side_State + 3;
        when others => null;
        end case;

        Side_State := Side_State mod 4;
        --  Top bits
        if Level = 0 then
            T := Half_Atlas_Factor;
        elsif Side_State = 1 or Side_State = 3 then
            S := 0.0;
        end if;
        Reverse_S := Side_State = 1 or Side_State = 2;

        if not Reverse_S then
            Offset_Factor := ((1.0, 1.0),
                              (0.0, 1.0),
                              (0.0, 0.0),
                              (0.0, 0.0),
                              (1.0, 0.0),
                              (1.0, 1.0));
        else  --  Reverse_S
            Offset_Factor := ((0.0, 1.0),
                              (1.0, 1.0),
                              (1.0, 0.0),
                              (1.0, 0.0),
                              (0.0, 0.0),
                              (0.0, 1.0));
        end if;
        New_Tex_Cords;

    end Set_Tex_Coords;

    --  --------------------------------------------------------------------------

    function Static_Lights return Static_Light_Vector is
    begin
        return Static_Lights_List;
    end Static_Lights;

    --  --------------------------------------------------------------------------

    procedure Update_AABB_Dimensions (aBatch : in out Batch_Meta;
                                      Point_List : GL_Maths.Vec3_List) is
        use GL_Maths.Vec3_Package;
        Curs   : Cursor := Point_List.First;
        aPoint : Singles.Vector3;
    begin
        while Has_Element (Curs) loop
            aPoint := Element (Curs);
            for index in Singles.Vector3'Range loop
                if aPoint (index) < aBatch.AABB_Mins (index) then
                    aBatch.AABB_Mins (index) := aPoint (index);
                elsif
                  aPoint (index) > aBatch.AABB_Maxs (GL.X) then
                    aBatch.AABB_Maxs (index) := aPoint (index);
                end if;
            end loop;
            Next (Curs);
        end loop;

    end Update_AABB_Dimensions;

    --  -------------------------------------------------------------------------

    procedure Update_Batch (Index : Positive; Data : Batch_Meta) is
    begin
        Batches_Data.Replace_Element (Index, Data);
    end Update_Batch;

    --  ----------------------------------------------------------------------------

end Batch_Manager;
