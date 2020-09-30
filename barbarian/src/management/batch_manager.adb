
with Game_Utils;
with Settings;

package body Batch_Manager is

   Atlas_Factor      : constant Single := 0.25;
   Sets_In_Atlas_Row : constant Integer := 4;
   ST_Offset         : constant Single := 8.0 / 2048.0;

   procedure North_Check (aBatch     : in out Batch_Meta;
                          Row, Col   :   Int; Height : Integer;
                          Tiles      : Tiles_Manager.Tile_List;
                          Tile_Index : positive);

   --  ----------------------------------------------------------------------------

   function Batch_Split_Size return Integer is
   begin
      return Batch_Split_Count;
   end Batch_Split_Size;

   --  ----------------------------------------------------------------------------

   procedure Free_Batch_Data (Batch_Index : Positive) is
      theBatch : Batch_Meta := Batches.Element (Batch_Index);
   begin
      theBatch.Points.Clear;
      theBatch.Point_Count := 0;
      theBatch.Ramp_Points.Clear;
      theBatch.Ramp_Point_Count := 0;
      theBatch.Water_Points.Clear;
      theBatch.Water_Point_Count := 0;
      Batches.Replace_Element  (Batch_Index, theBatch);
   end Free_Batch_Data;

   --  ----------------------------------------------------------------------------

   procedure Generate_Points (aBatch : in out Batch_Meta;
                              Tiles  : Tiles_Manager.Tile_List) is
      use Tiles_Manager;
      aTile     : Tile_Data;
      Row       : Int;
      Column    : Int;
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
         for Tile_Index in Tiles.First_Index .. Tiles.Last_Index loop
            aTile := Tiles.Element (Tile_Index);
            Row := Int (Tile_Index) / Max_Cols + 1;
            Column := Int (Tile_Index) - Row * Max_Cols;
            Height := aTile.Height;
            X := Single (2 * Column);
            Y := Single (2 * Height);
            Z := Single (2 * Row);

            if aTile.Tile_Type = '~' then
               Height := Height - 1;
            elsif aTile.Tile_Type /= '/' then
               --  floor FR, FL, BL, BL, BR, FR
               aBatch.Points.Append ((X + 1.0, Y, Z - 1.0));
               aBatch.Points.Append ((X - 1.0, Y, Z - 1.0));
               aBatch.Points.Append ((X - 1.0, Y, Z + 1.0));
               aBatch.Points.Append ((X + 1.0, Y, Z + 1.0));
               aBatch.Points.Append ((X + 1.0, Y, Z - 1.0));
               aBatch.Point_Count := aBatch.Point_Count + 5;
            end if;
            for index in 1 .. 6 loop
               aBatch.Normals.Append ((0.0, 1.0, 0.0));
            end loop;
            aBatch.Normal_Count := 6;

            Atlas_Row := Tile_Index / Sets_In_Atlas_Row + 1;
            Atlas_Col := Tile_Index - Atlas_Row * Sets_In_Atlas_Row;
            Add_Tex_Coords (0.5, 1.0);
            Add_Tex_Coords (0.0, 1.0);
            Add_Tex_Coords (0.0, 0.5);
            Add_Tex_Coords (0.0, 0.5);
            Add_Tex_Coords (0.5, 0.5);
            Add_Tex_Coords (0.5, 1.0);

            --  check for higher neighbour to north (walls belong to the lower tile)
            if Row < Max_Rows then
               North_Check (aBatch, Row, Column, Height, Tiles, Tile_Index);
            end if;
         end loop;
      end if;

   end Generate_Points;

   --  ----------------------------------------------------------------------------

   function Get_Batch_Index (Column, Row : Int) return Integer is
      Result : Integer := -1;
   begin
      if Column >= 0 and Column < Max_Cols and
        Row >= 0 and Row < Max_Rows then
         Result := (Integer (Column) + Batches_Across * Integer (Row)) /
           Settings.Tile_Batch_Width;
      end if;
      return Result;
   end Get_Batch_Index;

   --  ----------------------------------------------------------------------------

   procedure North_Check (aBatch     : in out Batch_Meta;
                          Row, Col   : Int; Height : Integer;
                          Tiles      : Tiles_Manager.Tile_List;
                          Tile_Index : positive) is
      use Tiles_Manager;
      N_Index  : constant Integer := Tile_Index + Integer (Max_Cols);
      aTile    :  Tile_Data;
      N_Height : Integer;
      Diff     : Integer;
      X        : Single;
      Y        : Single;
      Z        : Single;
   begin
--        Game_Utils.Game_Log ("Batch_Manager.North_Check, Tile_Index, N_Index "
--                                   & Integer'Image (Tile_Index) & ",  "
--                                   & Integer'Image (N_Index));
      aTile := Tiles.Element (N_Index);
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
         Z := Single (2 * Row - 1);
         aBatch.Points.Append ((X, Y, Z));
         X := Single (2 * Col - 1);
         aBatch.Points.Append ((X, Y, Z));
         Y := Single (2 * (Height + level));
         aBatch.Points.Append ((X, Y, Z));
         X := Single (2 * Col + 1);
         aBatch.Points.Append ((X, Y, Z));
         Y := Single (2 * (Height + level + 1));
         aBatch.Points.Append ((X, Y, Z));
         aBatch.Point_Count := aBatch.Point_Count + 6;
         for index in 1 .. 6 loop
            aBatch.Normals.Append ((0.0, 0.0, 1.0));
         end loop;
         aBatch.Normal_Count := aBatch.Normal_Count + 6;
         --              Set_Texcoords ();
      end loop;

   end North_Check;

   --  ----------------------------------------------------------------------------

   procedure Regenerate_Batch (Tiles       : Tiles_Manager.Tile_List;
                               Batch_Index : Positive) is
      use Tiles_Manager;
      theBatch    : Batch_Meta := Batches.Element (Batch_Index);
      aTile       : Tile_Data;
      Batch_Tiles : constant Tile_Indices := theBatch.Tiles;
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
--              Game_Utils.Game_Log ("Batch_Manager.Regenerate_Batch, Diff, theBatch.Point_Count"
--                                   & Integer'Image (Diff) & ", " &
--                                   Integer'Image (theBatch.Point_Count));
            theBatch.Point_Count := theBatch.Point_Count + 6 * Diff;
            Total_Points := Total_Points + 6 * Diff;
         end if;
      end Add_Point_Count;

   begin
      Free_Batch_Data (Batch_Index);
      theBatch.Static_Light_Count := 0;
      if Is_Empty (Tiles) then
         Game_Utils.Game_Log ("Regenerate_Batch, theBatch.Tiles is empty.");
         raise Batch_Manager_Exception with
           "Batch_Manager.Regenerate_Batch, theBatch.Tiles is empty.";
      else
         for Tile_Index in Batch_Tiles.First_Index .. Batch_Tiles.Last_Index loop
--              Game_Utils.Game_Log ("Batch_Manager.Regenerate_Batch, Tile_Index, last index "
--                                   & Integer'Image (Tile_Index)
--                                   & Integer'Image (Batch_Tiles.Last_Index));
            aTile := Tiles.Element (Tile_Index);
            Row := Int (Tile_Index) / Max_Cols + 1;
            Column :=  Int (Tile_Index) - Row * Max_Cols;
            Height := aTile.Height;
            if aTile.Tile_Type = '~' then
               Height := Height - 1;
               theBatch.Water_Point_Count := theBatch.Water_Point_Count +
                 Water_Mesh_Point_Count;
               Total_Points := Total_Points + Water_Mesh_Point_Count;
            elsif aTile.Tile_Type = '/' then
               theBatch.Ramp_Point_Count := theBatch.Ramp_Point_Count +
                 Ramp_Mesh_Point_Count;
            else
               --  Add floor count
               theBatch.Point_Count := theBatch.Point_Count + 6;
               Total_Points := Total_Points + 6;
            end if;
            --  Sides count
            if Row > 1 then
               N_Index := Tile_Index - Integer (Max_Cols) + 1;
--                 Game_Utils.Game_Log ("Batch_Manager.Regenerate_Batch sides count: Row, N_Index"
--                                     & Int'Image (Row) & ", " & Integer'Image (N_Index));
               N_Tile := Tiles.Element (N_Index);
               N_Height := N_Tile.Height;
               if N_Tile.Tile_Type = '~' then
                  N_Height := N_Height - 1;
               end if;
               Diff := Height - N_Height;
--                 Game_Utils.Game_Log ("Batch_Manager.Regenerate_Batch, Height, N_Height "
--                                      & Integer'Image (Height) & ", " &
--                                        Integer'Image (N_Height));
               --  Remove bit behind stairs from construction list

               if N_Tile.Tile_Type = '/' and then
                 N_Tile.Facing = 'S' then
                  Diff := Diff - 1;
               end if;
               Add_Point_Count (Diff);
            end if;

            if Row < Max_Rows then
               N_Index := Tile_Index + Integer (Max_Cols);
               N_Tile := Tiles.Element (N_Index);
               N_Height := N_Tile.Height;
               if N_Tile.Tile_Type = '~' then
                  N_Height := N_Height - 1;
               end if;
               Diff := Height - N_Height;
--              Game_Utils.Game_Log ("Batch_Manager.Regenerate_Batch, Row < Max_Rows, Height, N_Height "
--                                   & Integer'Image (Height) & ", " &
--                                   Integer'Image (N_Height));

               if N_Tile.Tile_Type = '/' and then
                 N_Tile.Facing = 'N' then
                  Diff := Diff - 1;
               end if;
               Add_Point_Count (Diff);
            end if;

            if Column > 1 then
               N_Index := Tile_Index - 1;
               N_Tile := Tiles.Element (N_Index);
               N_Height := N_Tile.Height;
               if N_Tile.Tile_Type = '~' then
                  N_Height := N_Height - 1;
               end if;
               Diff := Height - N_Height;
--              Game_Utils.Game_Log ("Batch_Manager.Regenerate_Batch, Column > 1, Height, N_Height "
--                                   & Integer'Image (Diff) & ", " &
--                                   Integer'Image (Height) & ", " &
--                                   Integer'Image (N_Height));

               if N_Tile.Tile_Type = '/' and then
                 N_Tile.Facing = 'E' then
                  Diff := Diff - 1;
               end if;
--              Game_Utils.Game_Log ("Batch_Manager.Regenerate_Batch, Tile_Type = '/', Height, N_Height "
--                                   & Integer'Image (Height) & ", " &
--                                   Integer'Image (N_Height));
               Add_Point_Count (Diff);
            end if;

            if Column < Max_Cols then
               N_Index := Tile_Index + 1;
               N_Tile := Tiles.Element (N_Index);
               N_Height := N_Tile.Height;
               if N_Tile.Tile_Type = '~' then
                  N_Height := N_Height - 1;
               end if;
               Diff := Height - N_Height;

               if N_Tile.Tile_Type = '/' and then
                 N_Tile.Facing = 'W' then
                  Diff := Diff - 1;
               end if;
--              Game_Utils.Game_Log ("Batch_Manager.Regenerate_Batch, Tile_Type = '/' 2, Height, N_Height "
--                                   & Integer'Image (Height) & ", " &
--                                   Integer'Image (N_Height));
               Add_Point_Count (Diff);
            end if;
         end loop;  -- over tiles
      end if;  --  not Tiles not empty

      theBatch.VAO.Initialize_Id;
      theBatch.Point_Count := 0;
      theBatch.Normal_Count := 0;
      theBatch.Tex_Coord_Count := 0;

      theBatch.Ramp_VAO.Initialize_Id;
      theBatch.Ramp_Point_Count := 0;
      theBatch.Ramp_Normal_Count := 0;
      theBatch.Ramp_Tex_Coord_Count := 0;

      theBatch.Water_VAO.Initialize_Id;
      theBatch.Water_Point_Count := 0;

      Generate_Points (theBatch, Tiles);
      Batches.Replace_Element (Batch_Index, theBatch);
--        Game_Utils.Game_Log ("Regenerate_Batch,Total_Points " &
--                               Integer'Image (Total_Points));

   end Regenerate_Batch;

   --  ----------------------------------------------------------------------------

end Batch_Manager;
