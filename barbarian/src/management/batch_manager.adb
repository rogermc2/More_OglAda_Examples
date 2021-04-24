
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Pixels;

with Utilities;

with Game_Utils;
with GL_Maths;
with GL_Utils;
with Manifold;
with Mesh_Loader;
with Settings;
with Shader_Attributes;

package body Batch_Manager is

   type Tile_Side is (North_Side, East_Side, South_Side, West_Side);

   Batches_Data               : Batches_List;
   Static_Lights_List         : Static_Light_Vector;
   Atlas_Factor               : constant Single := 0.25;
   Sets_In_Atlas_Row          : constant Positive := 4;
   ST_Offset                  : constant Single := 8.0 / 2048.0;
   Ramp_Mesh_Points           : GL_Maths.Vec3_List;
   Ramp_Mesh_Normals          : GL_Maths.Vec3_List;
   Ramp_Mesh_Texcoords        : GL_Maths.Vec2_List;
   Ramp_Mesh_Smooth_Points    : GL_Maths.Vec3_List;
   Ramp_Mesh_Smooth_Normals   : GL_Maths.Vec3_List;
   Ramp_Mesh_Smooth_Texcoords : GL_Maths.Vec2_List;
   Water_Mesh_Points          : GL_Maths.Vec3_List;
   Water_Mesh_Normals         : GL_Maths.Vec3_List;
   Water_Mesh_Texcoords       : GL_Maths.Vec2_List;

   function Check_For_OOO (Batch_Index : Natural) return Boolean;
   procedure Set_AABB_Dimensions (aBatch : in out Batch_Meta);
   procedure Update_AABB_Dimensions (aBatch     : in out Batch_Meta;
                                     Point_List : GL_Maths.Vec3_List);
   procedure Set_Tex_Coords (aBatch : in out Batch_Meta;
                             aTile  : Tiles_Manager.Tile_Data;
                             Side   : Tile_Side; Level : Natural);

   --  -------------------------------------------------------------------------

   procedure Add_Batch_To_Batch_List (Batch_Data : Batch_Meta) is
   begin
      Batches_Data.Append (Batch_Data);
   end Add_Batch_To_Batch_List;

   --  -------------------------------------------------------------------------

   procedure Add_East_Points
     (aBatch             : in out Batch_Meta;  Height : Integer;
      Tile_Index         : Natural;
      Tile_Row, Tile_Col : Tiles_Manager.Tiles_RC_Index) is
      use Tiles_Manager;
      N_Tile   : Tile_Data;
      N_Height : Integer;
      Diff     : Integer;
      X        : Single;
      Y        : Single;
      Z        : Single;
   begin
      N_Tile := Get_Tile (Tile_Index - 1);
      N_Height := N_Tile.Height;
      if N_Tile.Tile_Type = '~' then
         N_Height := N_Height - 1;
      end if;
      Diff := Height - N_Height;

      --  remove bit behind stairs from construction list
      if N_Tile.Tile_Type = '/' and then N_Tile.Facing = 'E' then
         Diff := Diff - 1;
      end if;

      for level in -Diff .. -1 loop
         X := Single (2 * Col - 1);
         Y := Single (2 * (Height + level + 1));
         Z := Single (2 * Tile_Row + 1);
         aBatch.Points.Append ((X, Y, Z));

         Z := Single (2 * Tile_Row - 1);
         aBatch.Points.Append ((X, Y, Z));

         Y := Single (2 * (Height + level));
         aBatch.Points.Append ((X, Y, Z));
         aBatch.Points.Append ((X, Y, Z));

         Z := Single (2 * Tile_Row + 1);
         aBatch.Points.Append ((X, Y, Z));

         Y := Single (2 * (Height + level + 1));
         aBatch.Points.Append ((X, Y, Z));

         for index in 1 .. 6 loop
            aBatch.Normals.Append ((-1.0, 0.0, 0.0));
         end loop;

         Set_Tex_Coords (aBatch, Get_Tile (Tile_Index), East_Side,
                         Diff - level - 1);
      end loop;

   exception
      when anError : others =>
         Put ("Batch_Manager.Add_East_Points exception: ");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
         raise;

   end Add_East_Points;

   --  -------------------------------------------------------------------------

   procedure Add_North_Points
     (aBatch             : in out Batch_Meta;  Height : Integer;
      Tile_Index         : Natural;
      Tile_Row, Tile_Col : Tiles_Manager.Tiles_RC_Index) is
      use Tiles_Manager;
      N_Tile   : Tile_Data;
      N_Height : Integer;
      Diff     : Integer;
      X        : Single;
      Y        : Single;
      Z        : Single;
   begin
      N_Tile := Get_Tile (Tile_Index + Max_Map_Cols);
      N_Height := N_Tile.Height;
      if N_Tile.Tile_Type = '~' then
         N_Height := N_Height - 1;
      end if;
      Diff := Height - N_Height;

      --  Remove bit behind stairs from construction list
      if N_Tile.Tile_Type = '/' and then N_Tile.Facing = 'N' then
         Diff := Diff - 1;
      end if;

      --  Build one of Diff wall sections at a time
      if Diff > 0 then
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

            for index in 1 .. 6 loop
               aBatch.Normals.Append ((0.0, 0.0, 1.0));
            end loop;

            Set_Tex_Coords (aBatch, Get_Tile (Tile_Index), North_Side,
                            Diff - level - 1);
         end loop;
      end if;

   exception
      when anError : others =>
         Put ("Batch_Manager.Add_North_Points exception: ");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
         raise;

   end Add_North_Points;

   --  -------------------------------------------------------------------------

   procedure Add_South_Points
     (aBatch             : in out Batch_Meta;  Height : Integer;
      Tile_Index         : Natural;
      Tile_Row, Tile_Col : Tiles_Manager.Tiles_RC_Index) is
      use Tiles_Manager;
      aTile    : constant Tile_Data := Get_Tile (Tile_Index);
      N_Tile   : Tile_Data;
      N_Height : Integer;
      Diff     : Integer;
      X        : Single;
      Y        : Single;
      Z        : Single;
   begin
      N_Tile := Get_Tile (Tile_Index - Max_Map_Cols);
      N_Height := N_Tile.Height;
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
         Y := Single (2 * (Height + level + 1));
         Z := Single (2 * Tile_Row - 1);
         aBatch.Points.Append ((X, Y, Z));   --  1

         X := Single (2 * Col + 1);
         aBatch.Points.Append ((X, Y, Z));   --  2

         Y := Single (2 * (Height + level));
         aBatch.Points.Append ((X, Y, Z));   --  3
         aBatch.Points.Append ((X, Y, Z));   --  4

         X := Single (2 * Col - 1);
         aBatch.Points.Append ((X, Y, Z));   --  5

         Y := Single (2 * (Height + level + 2));
         aBatch.Points.Append ((X, Y, Z));   --  6

         for index in 1 .. 6 loop
            aBatch.Normals.Append ((0.0, 0.0, -1.0));
         end loop;

         Set_Tex_Coords (aBatch, aTile, South_Side, diff - level - 1);
      end loop;

   exception
      when anError : others =>
         Put ("Batch_Manager.Add_South_Points exception: ");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
         raise;

   end Add_South_Points;

   --  -------------------------------------------------------------------------

   procedure Add_Static_Light (Row, Col                       : Natural;
                               Tile_Height_Offset             : Integer;
                               Offset_Pos, Diffuse, Specular  : Singles.Vector3;
                               Light_Range                    : Single) is
      use Batches_Package;
      use Tiles_Manager;
      S_Row         : constant Single := Single (Row);
      S_Col         : constant Single := Single (Col);
      X             : Single := 2.0 * S_Col + Offset_Pos (GL.X);
      Y             : Single :=
                        2.0 * (Tiles_Manager.Get_Tile_Height
                               (S_Row, S_Col, False, False) + Single (Tile_Height_Offset))
                        + Offset_Pos (GL.Y);
      Z             : Single := 2.0 * S_Row + Offset_Pos (GL.Z);
      Total_Batches : Integer := Batches_Across * Batches_Down;
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
      if not Tiles_Manager.Is_Tile_Valid ((Int (Row), Int (Col))) then
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

      Batches_Data.Iterate (Process_Batch'Access);

   end Add_Static_Light;

   --  ------------------------------------------------------------------------

   procedure Add_West_Points
     (aBatch             : in out Batch_Meta;  Height : Integer;
      Tile_Index         : Natural;
      Tile_Row, Tile_Col : Tiles_Manager.Tiles_RC_Index) is
      use Tiles_Manager;
      N_Height : Integer;
      N_Tile   : Tile_Data;
      Diff     : Integer;
      X        : Single;
      Y        : Single;
      Z        : Single;
   begin
      N_Tile := Get_Tile (Tile_Index + 1);
      N_Height := N_Tile.Height;
      if N_Tile.Tile_Type = '~' then
         N_Height := N_Height - 1;
      end if;
      Diff := Height - N_Height;

      --  remove bit behind stairs from construction list
      if N_Tile.Tile_Type = '/' and then N_Tile.Facing = 'W' then
         Diff := Diff - 1;
      end if;

      for level in -Diff .. -1 loop
         X := Single (2 * Col + 1);
         Y := Single (2 * (Height + level + 1));
         Z := Single (2 * Tile_Row - 1);
         aBatch.Points.Append ((X, Y, Z));   --  1

         Z := Single (2 * Tile_Row + 1);
         aBatch.Points.Append ((X, Y, Z));   --  2
         Y := Single (2 * (Height + level));
         aBatch.Points.Append ((X, Y, Z));   --  3
         aBatch.Points.Append ((X, Y, Z));   --  4

         Z := Single (2 * Tile_Row - 1);
         aBatch.Points.Append ((X, Y, Z));   --  5
         X := Single (2 * Col + 1);
         aBatch.Points.Append ((X, Y, Z));   --  6

         for index in 1 .. 6 loop
            aBatch.Normals.Append ((1.0, 0.0, 0.0));
         end loop;

         Set_Tex_Coords (aBatch, Get_Tile (Tile_Index), West_Side,
                         Diff - level - 1);
      end loop;

   exception
      when anError : others =>
         Put ("Batch_Manager.Add_West_Points exception: ");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
         raise;

   end Add_West_Points;

   --  -------------------------------------------------------------------------

   function Batch_List return Batches_List is
   begin
      return Batches_Data;
   end Batch_List;

   --  ------------------------------------------------------------------------

   function Batches_Empty return Boolean is
   begin
      return Batches_Data.Is_Empty;
   end Batches_Empty;

   --  ------------------------------------------------------------------------
   --  Out-of-order check. swap on first out-of-order and returns false.
   --  Multiple calls required to sort entire list
   function Check_For_OOO (Batch_Index : Natural) return Boolean is
      use Maths.Single_Math_Functions;
      use Tiles_Manager;
      use GL_Maths.Indices_Package;
      Half_Batch_Width     : constant Natural :=
                               Settings.Tile_Batch_Width / 2;
      This_Batch           : Batch_Meta := Batches_Data.Element (Batch_Index);
      Batches_Dn           : constant Natural := Batch_Index / Batches_Across;
      Batches_Ac           : constant Natural := Batch_Index -
                               Batches_Dn * Batches_Across;
      Batch_Centre_Row     : constant Natural :=
                               Batches_Dn * Settings.Tile_Batch_Width +
                                 Half_Batch_Width;
      Batch_Centre_Col     : constant Natural :=
                               Batches_Ac * Settings.Tile_Batch_Width +
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
      Curr_Row             : Natural;
      Curr_Col             : Natural;
      Next_Row             : Natural;
      Next_Col             : Natural;
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

   procedure Clear_Batch_Data is
   begin
      Batches_Data.Clear;
      Ramp_Mesh_Points.Clear;
      Ramp_Mesh_Normals.Clear;
      Ramp_Mesh_Texcoords.Clear;
      Ramp_Mesh_Smooth_Points.Clear;
      Ramp_Mesh_Smooth_Normals.Clear;
      Ramp_Mesh_Smooth_Texcoords.Clear;
      Water_Mesh_Points.Clear;
      Water_Mesh_Normals.Clear;
      Water_Mesh_Texcoords.Clear;
   end Clear_Batch_Data;

   --  ----------------------------------------------------------------------------

   procedure Free_Batch_Data (Batch_Index : Natural) is
      theBatch : Batch_Meta := Batches_Data.Element (Batch_Index);
   begin
      theBatch.Points.Clear;
      theBatch.Ramp_Points.Clear;
      theBatch.Water_Points.Clear;
      theBatch.Normals.Clear;
      theBatch.Ramp_Normals.Clear;
      theBatch.Ramp_Smooth_Normals.Clear;
      theBatch.Tex_Coords.Clear;
      theBatch.Ramp_Tex_Coords.Clear;
      theBatch.Static_Light_Indices.Clear;
      Batches_Data.Replace_Element  (Batch_Index, theBatch);
   end Free_Batch_Data;

   --  -------------------------------------------------------------------------

   --  Generate_Points for all tiles in a batch
   procedure Generate_Points (aBatch : in out Batch_Meta) is
      use Tiles_Manager;
      use Tile_Indices_Package;
      use GL_Maths;
--        subtype Tiles_Index is Natural range 0 .. Total_Tiles - 1;
      subtype Atlas_Index is Natural range 0 .. Max_Tile_Cols - 1;
      subtype Texture_Index is single range 0.0 .. 1.0;
      Row_Index         : Natural;
      Col_Index         : Natural;
      Column_List       : Tile_Column_List;
      aTile             : Tile_Data;  --  includes texture index
      N_Tile            : Tile_Data;
      Tile_Index        : Tiles_Index;
      Height            : Integer;
      X                 : Single;
      Y                 : Single;
      Z                 : Single;
      Tex_Index         : Atlas_Index;
      Atlas_Row         : Atlas_Index;
      Atlas_Col         : Atlas_Index;

      procedure Add_Tex_Coords (S_Offset, T_Offset : Single)  is
         --  Atlas_Factor = 0.25 (Atlas_Col and Row range 0 .. 63)
         --  Atlas_Factor reduces Atlas_Col and Row range 0 .. 15
         --  Object size 125 x 125 pixels
         S  : constant Texture_Index :=
                Atlas_Factor * (Single (Atlas_Col) + S_Offset);
         T  : constant Texture_Index :=
                Atlas_Factor * (Single (Atlas_Row) + T_Offset);
      begin
         --  ST_Offset = 8.0 / 2048.0 = 1 / 256
         --  For S_Offset = 0:
         --  S - ST_Offset = Atlas_Col/4 - 1 / 256
         --  S - ST_Offset range; -1 / 256  .. 63/4 -1 / 256
         aBatch.Tex_Coords.Append ((S - ST_Offset, T - ST_Offset));
      end Add_Tex_Coords;

   begin
      aBatch.Points.Clear;
      aBatch.Normals.Clear;
      aBatch.Tex_Coords.Clear;
--        Game_Utils.Game_Log
--          ("Batch_Manger.Generate_Points Tile_Indices.First_Index, Last: " &
--             Integer'Image (aBatch.Tile_Indices.First_Index) & ", " &
--             Integer'Image (aBatch.Tile_Indices.Last_Index));
--        Put_Line ("Batch_Manger.Generate_Points, Tile_Indices.Last_Index: " &
--                   Integer'Image (Integer (aBatch.Tile_Indices.Last_Index)));
--        Put_Line ("Batch_Manger.Generate_Points, Number_Of_Tiles" &
--                   Integer'Image (Tiles_Manager.Number_Of_Tiles));
      --  for all tiles in aBatch
      for index in aBatch.Tile_Indices.First_Index ..
        aBatch.Tile_Indices.Last_Index loop
         Tile_Index := aBatch.Tile_Indices.Element (index);
         aTile := Get_Tile (Tile_Index);
         Height := aTile.Height;
         Row_Index := Tile_Index / Max_Map_Cols;
         Col_Index := Tile_Index - Row_Index * Max_Map_Cols;
--           Game_Utils.Game_Log ("Batch_Manger.Generate_Points Tile_Index: " &
--                                  Integer'Image (Tile_Index));
--           Game_Utils.Game_Log
--             ("Batch_Manger.Generate_Points Row_Index, Col_Index: " &
--                Integer'Image (Row_Index) & ", " & Integer'Image (Col_Index));
         X := Single (2 * Col_Index);
         Y := Single (2 * Height);
         Z := Single (2 * Row_Index);
         if aTile.Tile_Type = '~' then
            Height := Height - 1;
         end if;

         --  Generate flat tiles
         if aTile.Tile_Type /= '/' and aTile.Tile_Type /= '~' then
            --  floor FR, FL, BL, BL, BR, FR
            aBatch.Points.Append ((X + 1.0, Y, Z - 1.0));  -- FR
            aBatch.Points.Append ((X - 1.0, Y, Z - 1.0));  -- FL
            aBatch.Points.Append ((X - 1.0, Y, Z + 1.0));  -- BL
            aBatch.Points.Append ((X - 1.0, Y, Z + 1.0));  -- BL
            aBatch.Points.Append ((X + 1.0, Y, Z + 1.0));  -- BR
            aBatch.Points.Append ((X + 1.0, Y, Z - 1.0));  -- FR

            for norm_count in 1 .. 6 loop
               aBatch.Normals.Append ((0.0, 1.0, 0.0));
            end loop;

            --  Texture_Index from map file (range 0 .. 15, one hex digit)
            Tex_Index := aTile.Texture_Index;
            --  Select tile from map file
            --  Sets_In_Atlas_Row = 4 (Tiles in Atlas_Row)
            Atlas_Row := Tex_Index / Sets_In_Atlas_Row;
            Atlas_Col := Tex_Index - Atlas_Row * Sets_In_Atlas_Row;
--              Game_Utils.Game_Log
--                ("Batch_Manger.Generate_Points Tex_Index, Atlas_Row, Atlas_Col: "
--                 & Integer'Image (Tex_Index) & ", " & Integer'Image (Atlas_Row)
--                 & ", " & Integer'Image (Atlas_Col));
            Add_Tex_Coords (0.5, 1.0);
            Add_Tex_Coords (0.0, 1.0);
            Add_Tex_Coords (0.0, 0.5);
            Add_Tex_Coords (0.0, 0.5);
            Add_Tex_Coords (0.5, 0.5);
            Add_Tex_Coords (0.5, 1.0);
--              Game_Utils.Game_Log
--                ("Batch_Manger.Generate_Points Add_Tex_Coords added to tile: " &
--                Integer'Image (Tile_Index));
         end if;

         --  check for higher neighbour to north (walls belong to the lower tile)
         --           if Row_Index < Max_Map_Rows - 1 then
         --              Add_North_Points (aBatch, Height, Tile_Index, Row_Index, Col_Index);
         --           end if;
         --           if Row_Index > 0 then
         --              Add_South_Points (aBatch, Height, Tile_Index, Row_Index, Col_Index);
         --           end if;
         --
         --           if Col_Index < Column_List.Last_Index then
         --              Add_West_Points (aBatch, Height, Tile_Index, Row_Index, Col_Index);
         --           end if;
         --           if Col_Index > 0 then
         --              Add_East_Points (aBatch, Height, Tile_Index, Row_Index, Col_Index);
         --           end if;
      end loop;  -- over tile indices
--        Game_Utils.Game_Log
--          ("Batch_Manger.Generate_Points Tiles loaded.");

      aBatch.Points_VAO.Initialize_Id;
      GL_Utils.Bind_VAO (aBatch.Points_VAO);

      aBatch.Points_VBO := GL_Utils.Create_3D_VBO
        (GL_Maths.To_Vector3_Array (aBatch.Points));
      GL.Attributes.Set_Vertex_Attrib_Pointer
        (Shader_Attributes.Attrib_VP, 3, Single_Type, False, 0, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VP);

      Set_AABB_Dimensions (aBatch);

      aBatch.Normals_VBO := GL_Utils.Create_3D_VBO
        (GL_Maths.To_Vector3_Array (aBatch.Normals));
      GL.Attributes.Set_Vertex_Attrib_Pointer
        (Shader_Attributes.Attrib_VN, 3, Single_Type, False, 0, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VN);

      aBatch.Tex_Coords_VBO := GL_Utils.Create_2D_VBO
        (GL_Maths.To_Vector2_Array (aBatch.Tex_Coords));
      GL.Attributes.Set_Vertex_Attrib_Pointer
        (Shader_Attributes.Attrib_VT, 2, Single_Type, False, 0, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VT);
--        Game_Utils.Game_Log
--          ("Batch_Manger.Generate_Points done points, normals, tex coords lengths: "
--           & Integer'Image (Integer (aBatch.Points.Length)) & ", " &
--             Integer'Image (Integer (aBatch.Normals.Length)) & ", " &
--             Integer'Image (Integer (aBatch.Tex_Coords.Length)));

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Batch_Manger.Generate_Points!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
         raise;

   end Generate_Points;

   --  ----------------------------------------------------------------------------

   procedure Generate_Ramps (aBatch : in out Batch_Meta) is
      use Singles;
      use Maths;
      use Tiles_Manager;
      use Tile_Indices_Package;
      use GL_Maths;
      use Vec2_Package;
      use Vec3_Package;

      subtype Tiles_Index is Natural range 0 .. Total_Tiles - 1;
      Indices_Curs   : Tile_Indices_Package.Cursor := aBatch.Tile_Indices.First;
      Tile_Index     : Tiles_Index;
      Row_Index      : Tiles_RC_Index;
      Col_Index      : Tiles_RC_Index;
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
      VPI            : Singles.Vector4;
      VPF            : Singles.Vector4;
      VNF            : Singles.Vector4;
      Smooth_VNI     : Singles.Vector4;
      Smooth_VNF     : Singles.Vector4;
      Has_Ramp       : Boolean := False;
      --        First          : Boolean := True;  --  Debug
      --        Count          : Integer := 0;     --  Debug
   begin
      --  Manifold.cpp, approx line 1015, p = g_batches[batch_idx].tiles;
      --  for all tiles in aBatch
      while Has_Element (Indices_Curs) loop
         Tile_Index := Element (Indices_Curs);
         aTile := Get_Tile (Tile_Index);
         --           aTile := Get_Tile (Element (Indices_Curs));
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
            Has_Ramp := True;
            --              Row_Index := Element (Indices_Curs) (GL.X);
            --              Col_Index := Element (Indices_Curs) (GL.Y);
            Row_Index := Tile_Index / Max_Map_Cols;
            Col_Index := Tile_Index - Row_Index * Max_Map_Cols;
            --  Put each vertex point into world space
            Rot_Matrix := Rotate_Y_Degree (Identity4, Deg);
            Model_Matrix := Translation_Matrix
              ((Single (2 * (Col_Index - aRow.First_Index)),
               Single (2 * Height),
               Single (2 * (Row_Index - 1)))) *
              Rot_Matrix;
            Curs_P := Ramp_Mesh_Points.First;
            Curs_N := Ramp_Mesh_Normals.First;
            Curs_T := Ramp_Mesh_Texcoords.First;
            Curs_S := Ramp_Mesh_Smooth_Normals.First;

            while Has_Element (Curs_P) loop
               aPoint := Element (Curs_P);
               aNormal := Element (Curs_N);
               aSmooth_Normal := Element (Curs_S);
               VPI := Singles.To_Vector4 (aPoint);
               VPF := Model_Matrix * VPI;
               VNF := Model_Matrix * Singles.To_Vector4 (aNormal);
               Smooth_VNI := Singles.To_Vector4 (aSmooth_Normal);
               Smooth_VNF := Model_Matrix * Smooth_VNI;

               aBatch.Ramp_Points.Append (To_Vector3 (VPF));
               aBatch.Ramp_Normals.Append (To_Vector3 (VNF));
               aBatch.Ramp_Tex_Coords.Append (Element (Curs_T));
               aBatch.Ramp_Smooth_Normals.Append (To_Vector3 (Smooth_VNF));

               Next (Curs_N);
               Next (Curs_P);
               Next (Curs_T);
               Next (Curs_S);
            end loop;
            --                    First := False;
         end if;
         Next  (Indices_Curs);
      end loop;

      if Has_Ramp then
         aBatch.Ramp_VAO.Initialize_Id;
         GL_Utils.Bind_VAO (aBatch.Ramp_VAO);

         aBatch.Ramp_Points_VBO := GL_Utils.Create_3D_VBO
           (GL_Maths.To_Vector3_Array (aBatch.Ramp_Points));
         GL.Attributes.Set_Vertex_Attrib_Pointer
           (Shader_Attributes.Attrib_VP, 3, Single_Type, False, 0, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VP);

         Update_AABB_Dimensions (aBatch, aBatch.Ramp_Points);

         aBatch.Ramp_Normals_VBO := GL_Utils.Create_3D_VBO
           (GL_Maths.To_Vector3_Array (aBatch.Ramp_Normals));
         GL.Attributes.Set_Vertex_Attrib_Pointer
           (Shader_Attributes.Attrib_VN, 3, Single_Type, False, 0, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VN);

         aBatch.Ramp_Texcoords_VBO := GL_Utils.Create_2D_VBO
           (GL_Maths.To_Vector2_Array (aBatch.Ramp_Tex_Coords));

         GL.Attributes.Set_Vertex_Attrib_Pointer
           (Shader_Attributes.Attrib_Vt, 2, Single_Type, False, 0, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VT);

         aBatch.Ramp_Smooth_Normals_VBO := GL_Utils.Create_3D_VBO
           (GL_Maths.To_Vector3_Array (aBatch.Ramp_Smooth_Normals));
         GL.Attributes.Set_Vertex_Attrib_Pointer
           (Shader_Attributes.Attrib_VN, 3, Single_Type, False, 0, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VN);
      end if;

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Batch_Manger.Generate_Ramps!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
         raise;

   end Generate_Ramps;

   --  ----------------------------------------------------------------------------

   procedure Generate_Water (aBatch : in out Batch_Meta) is
      use Singles;
      use Maths;
      use Tiles_Manager;
      use Tile_Indices_Package;
      use GL_Maths;
      use Vec3_Package;
      subtype Tiles_Index is Natural range 0 .. Total_Tiles - 1;
      Indices_Curs   : Tile_Indices_Package.Cursor := aBatch.Tile_Indices.First;
      Tile_Index     : Tiles_Index;
      Row_Index      : Tiles_RC_Index;
      Col_Index      : Tiles_RC_Index;
      aTile          : Tile_Data;
      aRow           : Tile_Column_List;
      Facing         : Character;
      Height         : Integer;
      Deg            : Degree;
      Model_Matrix   : Matrix4 := Identity4;
      Rot_Matrix     : Matrix4 := Identity4;
      Curs_P         : Vec3_Cursor;
      aWater_Point   : Vector3;
      VPF            : Singles.Vector4;
      Has_Water      : Boolean := False;
   begin
      while Has_Element (Indices_Curs) loop
         Tile_Index := Element (Indices_Curs);
         aTile := Get_Tile (Tile_Index);
         --           aTile := Get_Tile (Element (Indices_Curs));
         Height := aTile.Height;
         Facing := aTile.Facing;

         case Facing is
            when 'N' => Deg := Degree (0);
            when 'W' => Deg := Degree (90);
            when 'S' => Deg := Degree (180);
            when 'E' => Deg := Degree (270);
            when others =>
               raise Batch_Manager_Exception with
                 "Generate_Water, invalid Facing value";
         end case;

         if aTile.Tile_Type = '~' then
            Has_Water := True;
            --              Row_Index := Element (Indices_Curs) (GL.X);
            --              Col_Index := Element (Indices_Curs) (GL.Y);
            Row_Index := Tile_Index / Max_Map_Cols;
            Col_Index := Tile_Index - Row_Index * Max_Map_Cols;
            --  Put each vertex point into world space
            Rot_Matrix := Rotate_Y_Degree (Rot_Matrix, Deg);
            Model_Matrix := Translation_Matrix
              ((Single (2 * Col_Index - aRow.First_Index),
               Single (2 * Height),
               Single (2 * Row_Index - 1)));
            Curs_P := Water_Mesh_Points.First;
            while Has_Element (Curs_P) loop
               aWater_Point := Element (Curs_P);
               VPF := Model_Matrix * Singles.To_Vector4 (aWater_Point);
               aBatch.Water_Points.Append (To_Vector3 (VPF));
               Next (Curs_P);
            end loop;  --  end for each Water_Point
         end if;
         Next  (Indices_Curs);
      end loop;

      if Has_Water then
         aBatch.Water_VAO.Initialize_Id;
         GL_Utils.Bind_VAO (aBatch.Water_VAO);

         aBatch.Water_Points_VBO := GL_Utils.Create_3D_VBO
           (GL_Maths.To_Vector3_Array (aBatch.Water_Points));
         GL.Attributes.Set_Vertex_Attrib_Pointer
           (Shader_Attributes.Attrib_VP, 3, Single_Type, False, 0, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VP);

         Update_AABB_Dimensions (aBatch, aBatch.Water_Points);
      end if;

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Batch_Manger.Generate_Water!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
         raise;

   end Generate_Water;

   --  -------------------------------------------------------------------------

   function Get_Batch_Index (Column, Row : Positive) return Natural is
      use Tiles_Manager;
      Result : Integer := 0;
   begin
      if Column < Positive (Max_Map_Cols) and Row < Positive (Max_Map_Rows) then
         Result := (Integer (Column - 1) +
                      Batches_Across * Integer (Row - 1 )) /
           Settings.Tile_Batch_Width + 1;
      end if;
      return Result;
   end Get_Batch_Index;

   --  -------------------------------------------------------------------------

   procedure Init_Batch_Data is
   begin
      Clear_Batch_Data;
      if not Mesh_Loader.Load_Mesh_Data_Only
        ("src/meshes/ramp_may_2014.apg", Ramp_Mesh_Points,
         Ramp_Mesh_Texcoords, Ramp_Mesh_Normals) then
         raise Batch_Manager_Exception with
           "Batch_Manager.Init_Batch_Data error loading ramp mesh data from file "
           & "src/meshes/ramp_may_2014.apg";
      end if;

      if not Mesh_Loader.Load_Mesh_Data_Only ("src/meshes/ramp_smooth.apg",
                                              Ramp_Mesh_Smooth_Points,
                                              Ramp_Mesh_Smooth_Texcoords,
                                              Ramp_Mesh_Smooth_Normals) then
         raise Batch_Manager_Exception with
           "Batch_Manager.Init_Batch_Data error loading ramp smooth mesh data from file "
           & "src/meshes/ramp_smooth.apg";
      end if;

      if not Mesh_Loader.Load_Mesh_Data_Only
        ("src/meshes/water.apg", Water_Mesh_Points, Water_Mesh_Texcoords,
         Water_Mesh_Normals) then
         raise Batch_Manager_Exception with
           "Batch_Manager.Init_Batch_Data error loading ramp mesh data from file "
           & "src/meshes/water.apg";
      end if;

   end Init_Batch_Data;

   --  -------------------------------------------------------------------------

   procedure Regenerate_Batch (Batch_Index : Natural) is
      theBatch     : Batch_Meta;
      Tile_Indices : Tiles_Manager.Tile_Indices_List;

   begin
      Free_Batch_Data (Batch_Index);

      theBatch := Batches_Data.Element (Batch_Index);
      theBatch.Static_Light_Indices.Clear;
      Tile_Indices := theBatch.Tile_Indices;
      if Tile_Indices.Is_Empty then
         raise Batch_Manager_Exception with
           "Batch_Manager.Regenerate_Batch called with empty Tiles list";
      end if;

--        Game_Utils.Game_Log ("Batch_Manager.Regenerate_Batch Generate_Points for Batch_Index"
--                             & Integer'Image (Batch_Index));
      Generate_Points (theBatch);
      Generate_Ramps (theBatch);
--        Game_Utils.Game_Log ("Batch_Manager.Regenerate_Batch Generate_Ramps done");
      Generate_Water (theBatch);
--        Game_Utils.Game_Log ("Batch_Manager.Regenerate_Batch Generate_Water done");

      Batches_Data.Replace_Element (Batch_Index, theBatch);

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Batch_Manger.Regenerate_Batch!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
         raise;
   end Regenerate_Batch;

   --  -------------------------------------------------------------------------

   procedure Set_AABB_Dimensions (aBatch : in out Batch_Meta) is
   begin
      aBatch.AABB_Mins := (100000.0, 100000.0, 100000.0);
      aBatch.AABB_Maxs := (-100000.0, -100000.0, -100000.0);
      Update_AABB_Dimensions  (aBatch, aBatch.Points);

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Tiles_Manager.Set_AABB_Dimensions!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
         raise;

   end Set_AABB_Dimensions;

   --  -------------------------------------------------------------------------

   procedure Set_Tex_Coords (aBatch : in out Batch_Meta;
                             aTile  : Tiles_Manager.Tile_Data;
                             Side   : Tile_Side; Level : Natural) is
      Offset_Factor     : Singles.Vector2_Array (1 .. 6);
      Texture_Index     : constant Positive := aTile.Texture_Index;
      Half_Atlas_Factor : constant Single := 0.5 * Atlas_Factor;
      Atlas_Row         : constant Tiles_Manager.Tiles_RC_Index
        := Tiles_Manager.Tiles_RC_Index (Texture_Index) / Sets_In_Atlas_Row + 1;
      Atlas_Col         : constant Tiles_Manager.Tiles_RC_Index
        := Tiles_Manager.Tiles_RC_Index (Texture_Index) - (Atlas_Row - 1) *
                            Sets_In_Atlas_Row + 1;
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
         SI       : Natural := 0;
         TI       : Natural := 0;
      begin
         for index in Int range 1 .. 6 loop
            SF := Offset_Factor (index) (GL.X) * Half_Atlas_Factor;
            Tf := Offset_Factor (index) (GL.Y) * Half_Atlas_Factor;
            S_Offset := S + Sf + Single (Atlas_Col) * Atlas_Factor;
            T_Offset := T + Tf + Single (Atlas_Row) * Atlas_Factor;
            Si := Integer (aBatch.Tex_Coords.Length);
            Ti := Si;
            Sf := -Offset_Factor (index) (GL.X);
            Tf := -Offset_Factor (index) (GL.Y);
            STF := (S_Offset + Sf * ST_Offset,
                    T_Offset + Tf * ST_Offset);
            if Si > 0 then
               if SI > aBatch.Tex_Coords.Last_Index then
                  aBatch.Tex_Coords.Set_Length (Ada.Containers.Count_Type (Si));
               end if;
               aBatch.Tex_Coords.Replace_Element (Positive (Si), STF);
            end if;
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

   function Static_Indices  (Batch_Index : Positive)
                             return GL_Maths.Indices_List is
      aBatch : Batch_Meta := Batches_Data.Element (Batch_Index);
   begin
      return aBatch.Static_Light_Indices;
   end Static_Indices;

   --  --------------------------------------------------------------------------

   --     function To_Vector3_Array (Vec : ind)
   --                                return Vector3_Array is
   --        use Ada.Containers;
   --        use GL.Types;
   --        use Vec3_Package;
   --        Curs      : Cursor := Vec.First;
   --        Vec_Array : Vector3_Array (0 .. Int (Vec.Length - 1));
   --     begin
   --        for index in Int range Vec_Array'Range loop
   --           Vec_Array (index) := Vec (Curs);
   --           Next  (Curs);
   --        end loop;
   --        return Vec_Array;
   --
   --     end To_Vector3_Array;

   --  ------------------------------------------------------------------------

   procedure Update_AABB_Dimensions (aBatch     : in out Batch_Meta;
                                     Point_List : GL_Maths.Vec3_List) is
      use GL_Maths.Vec3_Package;
      Curs   : Cursor := Point_List.First;
      aPoint : Singles.Vector3;

      procedure Update_AABB_Dim (Dim : GL.Index_3D) is
      begin
         if aPoint (Dim) < aBatch.AABB_Mins (Dim) then
            aBatch.AABB_Mins (Dim) := aPoint (Dim);
         elsif
           aPoint (Dim) > aBatch.AABB_Maxs (Dim) then
            aBatch.AABB_Maxs (Dim) := aPoint  (Dim);
         end if;
      end Update_AABB_Dim;

   begin
      while Has_Element (Curs) loop
         aPoint := Element (Curs);
         for index in Singles.Vector3'Range loop
            Update_AABB_Dim (GL.X);
            Update_AABB_Dim (GL.Y);
            Update_AABB_Dim (GL.Z);
         end loop;
         Next (Curs);
      end loop;

   end Update_AABB_Dimensions;

   --  -------------------------------------------------------------------------

   procedure Update_Batch (Index : Natural; Data : Batch_Meta) is
   begin
      Batches_Data.Replace_Element (Index, Data);
   end Update_Batch;

   --  ------------------------------------------------------------------------

   procedure Add_Tile_To_Batch (Index : Natural; Tile_Index : Natural) is
      aBatch : Batch_Meta := Batch_List.Element (Index);
   begin
      aBatch.Tile_Indices.Append (Tile_Index);
      Batches_Data.Replace_Element (Index, aBatch);
   end Add_Tile_To_Batch;

   --  ------------------------------------------------------------------------

end Batch_Manager;
