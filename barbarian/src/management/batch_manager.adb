
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Pixels;

with Game_Utils;
with GL_Maths;
with GL_Utils;
with Mesh_Loader;
with Settings;
with Shader_Attributes;

package body Batch_Manager is

    Batches_Data           : Batches_List;
    Static_Lights_List     : Static_Light_Vector;
    Atlas_Factor           : constant Single := 0.25;
    Sets_In_Atlas_Row      : constant Integer := 4;
    ST_Offset              : constant Single := 8.0 / 2048.0;
    Ramp_Mesh_Points         : GL_Maths.Vec3_List;
    Ramp_Mesh_Normals        : GL_Maths.Vec3_List;
    Ramp_Mesh_Smooth_Normals : GL_Maths.Vec3_List;
    Ramp_Mesh_Texcoords      : GL_Maths.Vec2_List;
    Water_Mesh_Points        : GL_Maths.Vec3_List;
    Water_Mesh_Normals       : GL_Maths.Vec3_List;
    Water_Mesh_Texcoords     : GL_Maths.Vec2_List;
    Ramp_Mesh_Point_Count  : Integer := 0;
    Water_Mesh_Point_Count : Integer := 0;
    Total_Points           : Integer := 0;

    function Check_For_OOO (Batch_Index : Positive) return Boolean;
    procedure North_Check (aBatch     : in out Batch_Meta;
                           Row, Col   :   Int; Height : Integer;
                           Tiles      : Tiles_Manager.Tile_List;
                           Tile_Index : positive);
    procedure Set_AABB_Dimensions (aBatch : in out Batch_Meta);
    procedure South_Check (aBatch     : in out Batch_Meta;
                           Row, Col   :   Int; Height : Integer;
                           Tiles      : Tiles_Manager.Tile_List;
                           Tile_Index : positive);
    procedure Update_AABB_Dimensions (aBatch : in out Batch_Meta;
                                      Point_List : GL_Maths.Vec3_List);
    procedure West_Check (aBatch     : in out Batch_Meta;
                          Row, Col   :   Int; Height : Integer;
                          Tiles      : Tiles_Manager.Tile_List;
                          Tile_Index : positive);

    --  -------------------------------------------------------------------------

    procedure Add_Batch (Data : Batch_Meta) is
    begin
        Batches_Data.Append (Data);
    end Add_Batch;

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
        Batch_Index   : Positive := Batches.First_Index;
        New_Light     : Static_Light_Data;

        procedure Process_Batch (Curs : Batches_Package.Cursor) is
            Sorted  : Boolean := False;
            aBatch  : Batch_Meta := Element (Curs);
        begin
            Sorted := False;
            aBatch.Static_Light_Indices.Append (Static_Lights_List.Last_Index);
            Batches_Data.Replace_Element (Curs, aBatch);
            while not Sorted loop
                Sorted := Check_For_OOO (To_Index (Curs));
            end loop;
        end Process_Batch;
    begin
        if not Tiles_Manager.Is_Tile_Valid (Row, Col) then
            raise Batch_Manager_Exception with "Batch_Manager.Add_Static_Light invalid tile";
        end if;
        New_Light.Position := Offset_Pos;
        New_Light.Diffuse := Diffuse;
        New_Light.Specular := Specular;
        New_Light.Light_Range := Light_Range;
        New_Light.Row := Row;
        New_Light.Column := Col;
        Static_Lights_List.Append (New_Light);

        Batches.Iterate (Process_Batch'Access);
    end Add_Static_Light;

    --  ----------------------------------------------------------------------------

    function Batches return Batches_List is
    begin
        return Batches_Data;
    end Batches;

    --  ----------------------------------------------------------------------------

    function Check_For_OOO (Batch_Index : Positive) return Boolean is
        use Maths.Single_Math_Functions;
        use Tile_Indices_Package;
        Half_Batch_Width     : constant Int := Int (Settings.Tile_Batch_Width / 2);
        This_Batch           : Batch_Meta := Batches.Element (Batch_Index);
        Batches_Dn           : constant Int := Int (Batch_Index) / Int (Batches_Across);
        Batches_Ac           : constant Int := Int (Batch_Index) -
                                 Batches_Dn * Int (Batches_Across);
        Batch_Centre_Row     : constant Int :=
                                 Batches_Dn * Int (Settings.Tile_Batch_Width) +
                                 Half_Batch_Width;
        Batch_Centre_Col     : constant Int :=
                                 Batches_Ac * Int (Settings.Tile_Batch_Width) +
                                 Half_Batch_Width;
        Light_Indices        : Tile_Indices_List := This_Batch.Static_Light_Indices;
        Current_Light_Cursor : Cursor := Light_Indices.First;
        Current_Light_Index  : Positive := Element (Current_Light_Cursor);
        Current_Light        : Static_Light_Data :=
                                 Static_Lights_List.Element (Current_Light_Index);
        Prev_Light_Cursor    : Cursor := Light_Indices.First;
        Prev_Light_Index     : Positive;
        Next_Light_Index     : Positive;
        Next_Light           : Static_Light_Data;
        Next_Light_Cursor    : Cursor := Light_Indices.First;
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
                Light_Indices.Swap (Next_Light_Cursor, Current_Light_Cursor);
                if Prev_Light_Cursor /= Light_Indices.First then
                    Prev_Light_Cursor := Next_Light_Cursor;
                    --              else
                    --                 Light_Indices.First_Element := Next_Light_Cursor;
                end if;
            end if;
            Prev_Light_Index := Current_Light_Index;
            Current_Light_Index := Next_Light_Index;
            Current_Light_Cursor := Next_Light_Cursor;
            Next_Light_Index := Element (Next_Light_Cursor);
        end loop;

        This_Batch.Static_Light_Indices := Light_Indices;
        Batches_Data.Replace_Element (Batch_Index, This_Batch);
        return True;
    end Check_For_OOO;

    --  ----------------------------------------------------------------------------

    procedure Clear is
    begin
        Batches_Data.Clear;
        Ramp_Mesh_Point_Count := 0;
        Water_Mesh_Point_Count := 0;
        Total_Points := 0;
    end Clear;

    --  ----------------------------------------------------------------------------

    procedure East_Check (aBatch     : in out Batch_Meta;
                          Row, Col   : Int; Height : Integer;
                          Tiles      : Tiles_Manager.Tile_List;
                          Tile_Index : positive) is
        use Tiles_Manager;
        N_Index  : constant Integer := Tile_Index;
        aTile    : Tile_Data;
        N_Height : Integer;
        Diff     : Integer;
        X        : Single;
        Y        : Single;
        Z        : Single;
    begin
        aTile := Tiles.Element (N_Index);
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
            Z := Single (2 * Row + 1);
            aBatch.Points.Append ((X, Y, Z));

            Z := Single (2 * Row - 1);
            aBatch.Points.Append ((X, Y, Z));
            aBatch.Points.Append ((X, Y, Z));

            Z := Single (2 * Row + 1);
            aBatch.Points.Append ((X, Y, Z));

            Y := Single (2 * (Height + level + 2));
            aBatch.Points.Append ((X, Y, Z));

            for index in 1 .. 6 loop
                aBatch.Normals.Append ((0.0, 0.0, -1.0));
            end loop;
            --              Set_Texcoords ();
        end loop;

    end East_Check;

    --  -------------------------------------------------------------------------

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
                               Tiles  : Tiles_Manager.Tile_List) is
        use Tiles_Manager;
        use GL_Maths;
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
--                  Game_Utils.Game_Log ("Batch_Manager.Generate_Points Tile_Index"
--                                       & Integer'Image (Tile_Index));
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
                end if;

                for index in 1 .. 6 loop
                    aBatch.Normals.Append ((0.0, 1.0, 0.0));
                end loop;

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
                if Row > 1 then
                    South_Check (aBatch, Row, Column, Height, Tiles, Tile_Index);
                end if;
                if Column < Max_Cols then
                    West_Check (aBatch, Row, Column, Height, Tiles, Tile_Index);
                end if;
                if Column > 1 then
                    East_Check (aBatch, Row, Column, Height, Tiles, Tile_Index);
                end if;
            end loop;

            GL_Utils.Bind_VAO (aBatch.VAO);
            aBatch.Points_VBO := GL_Utils.Create_3D_VBO
              (GL_Maths.To_Vector3_Array (aBatch.Points));
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (Shader_Attributes.Attrib_VP, 3, Single_Type, False, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VP);

            Update_AABB_Dimensions  (aBatch, aBatch.Points);
            aBatch.Points.Clear;
        end if;

    end Generate_Points;

    --  ----------------------------------------------------------------------------

    procedure Generate_Ramps (aBatch : in out Batch_Meta;
                              Tiles  : Tiles_Manager.Tile_List) is
        use Singles;
        use Maths;
        use Tiles_Manager;
        use GL_Maths;
        use Vec3_Package;
        aTile          : Tile_Data;
        Row            : Int;
        Column         : Int;
        Facing         : Character;
        Height         : Integer;
        Deg            : Degree;
        Model_Matrix   : Matrix4 := Identity4;
        Rot_Matrix     : Matrix4 := Identity4;
        Curs_N         : Cursor;
        Curs_P         : Cursor;
        Curs_S         : Cursor;
        aNormal        : Vector3;
        aPoint         : Vector3;
        aSmooth_Normal : Vector3;
        VPF            : Singles.Vector4;
        VNF            : Singles.Vector4;
        Smooth_VNF     : Singles.Vector4;

    begin
        if not Is_Empty (Tiles) then
            for Tile_Index in Tiles.First_Index .. Tiles.Last_Index loop
--                  Put_Line ("Batch_Manager.Generate_Ramps, Tile_Index" &
--                              Integer'Image (Integer (Tile_Index)));
                aTile := Tiles.Element (Tile_Index);
                Row := Int (Tile_Index) / Max_Cols + 1;
                Column := Int (Tile_Index) - Row * Max_Cols;
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
                      ((Single (2 * Column), Single (2 * Height), Single (2 * Row)));
                    Curs_P := Ramp_Mesh_Points.First;
                    Curs_N := Ramp_Mesh_Normals.First;
                    Curs_S := Ramp_Mesh_Smooth_Normals.First;
                    while Has_Element (Curs_P) loop
                        Put_Line ("Generate_Ramps, Curs_P");
                        aPoint := Element (Curs_P);
                        Put_Line ("Generate_Ramps, Curs_N");
                        aNormal := Element (Curs_N);
                        Put_Line ("Generate_Ramps, Curs_S");
                        aSmooth_Normal := Element (Curs_S);
                        Put_Line ("Generate_Ramps, Cursors OK");
                        VPF := Model_Matrix * Singles.To_Vector4 (aPoint);
                        VNF := Model_Matrix * Singles.To_Vector4 (aNormal);
                        Smooth_VNF := Model_Matrix * Singles.To_Vector4 (aSmooth_Normal);

                        aBatch.Ramp_Points.Replace_Element (Curs_P, To_Vector3 (VPF));
                        aBatch.Ramp_Points.Replace_Element (Curs_N, To_Vector3 (VNF));
                        aBatch.Ramp_Points.Replace_Element (Curs_S, To_Vector3 (Smooth_VNF));
                        Next (Curs_N);
                        Next (Curs_P);
                        Next (Curs_S);
                    end loop;
                end if;
            end loop;
            Put_Line ("Batch_Manager.Generate_Ramps, Ramp_VBO Ramp_Mesh_Points size: " &
                        Integer'Image (Integer (Ramp_Mesh_Points.Length)));

            Put_Line ("Batch_Manager.Generate_Ramps, Ramp_VBO aBatch.Ramp_Points size: " &
                        Integer'Image (Integer (aBatch.Ramp_Points.Length)));
            aBatch.Ramp_VBO := GL_Utils.Create_3D_VBO
              (GL_Maths.To_Vector3_Array (aBatch.Ramp_Points));
            Put_Line ("Batch_Manager.Generate_Ramps, Ramp_VBO set");
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (Shader_Attributes.Attrib_VP, 3, Single_Type, False, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VP);

            Put_Line ("Batch_Manager.Generate_Ramps, Update_AABB_Dimensions");
            Update_AABB_Dimensions  (aBatch, aBatch.Ramp_Points);
            aBatch.Ramp_Points.Clear;

            Put_Line ("Batch_Manager.Generate_Ramps, Ramp_Normals_VBO");
            aBatch.Ramp_Normals_VBO := GL_Utils.Create_3D_VBO
              (GL_Maths.To_Vector3_Array (aBatch.Ramp_Normals));
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (Shader_Attributes.Attrib_VN, 3, Single_Type, False, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VN);
            aBatch.Ramp_Normals.Clear;

            Put_Line ("Batch_Manager.Generate_Ramps, Tex_Coords_VBO");
            aBatch.Tex_Coords_VBO := GL_Utils.Create_2D_VBO
              (GL_Maths.To_Vector2_Array (aBatch.Tex_Coords));
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (Shader_Attributes.Attrib_Vt, 2, Single_Type, False, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VT);
            aBatch.Tex_Coords.Clear;

            Put_Line ("Batch_Manager.Generate_Ramps, Ramp_Smooth_Normals_VBO");
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
                              Tiles  : Tiles_Manager.Tile_List) is
        use Singles;
        use Maths;
        use Tiles_Manager;
        use GL_Maths;
        use Vec3_Package;
        aTile          : Tile_Data;
        Row            : Int;
        Column         : Int;
        Facing         : Character;
        Height         : Integer;
        Deg            : Degree;
        Model_Matrix   : Matrix4 := Identity4;
        Rot_Matrix     : Matrix4 := Identity4;
        Curs_P         : Cursor;
        aWater_Point   : Vector3;
        VPF            : Singles.Vector4;

    begin
        if not Is_Empty (Tiles) then
            for Tile_Index in Tiles.First_Index .. Tiles.Last_Index loop
                aTile := Tiles.Element (Tile_Index);
                Row := Int (Tile_Index) / Max_Cols + 1;
                Column := Int (Tile_Index) - Row * Max_Cols;
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
                      ((Single (2 * Column), Single (2 * Height), Single (2 * Row)));
                    Curs_P := Water_Mesh_Points.First;
                    while Has_Element (Curs_P) loop
                        aWater_Point := Element (Curs_P);
                        VPF := Model_Matrix * Singles.To_Vector4 (aWater_Point);
                        aBatch.Water_Points.Replace_Element (Curs_P, To_Vector3 (VPF));
                        Next (Curs_P);
                    end loop;  --  end for each Water_Point
                end if;
            end loop;  --  end for each Tile

            GL_Utils.Bind_VAO (aBatch.Water_VAO);
            aBatch.Water_VBO := GL_Utils.Create_3D_VBO
              (GL_Maths.To_Vector3_Array (aBatch.Water_Points));
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (Shader_Attributes.Attrib_VP, 3, Single_Type, False, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VP);

            Update_AABB_Dimensions  (aBatch, aBatch.Water_Points);
            aBatch.Water_Points.Clear;

            aBatch.Ramp_Normals_VBO := GL_Utils.Create_3D_VBO
              (GL_Maths.To_Vector3_Array (aBatch.Ramp_Normals));
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (Shader_Attributes.Attrib_VN, 3, Single_Type, False, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VN);
            aBatch.Ramp_Normals.Clear;

            aBatch.Tex_Coords_VBO := GL_Utils.Create_2D_VBO
              (GL_Maths.To_Vector2_Array (aBatch.Tex_Coords));
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (Shader_Attributes.Attrib_Vt, 2, Single_Type, False, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VT);
            aBatch.Tex_Coords.Clear;

            aBatch.Ramp_Smooth_Normals_VBO := GL_Utils.Create_3D_VBO
              (GL_Maths.To_Vector3_Array (aBatch.Ramp_Smooth_Normals));
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (Shader_Attributes.Attrib_VN, 3, Single_Type, False, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VN);
            aBatch.Ramp_Smooth_Normals.Clear;

        end if;

    end Generate_Water;

    --  -------------------------------------------------------------------------

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

      if not Mesh_Loader.Load_Mesh_Data_Only
        ("src/meshes/ramp_may_2014.apg", Ramp_Mesh_Points,
         Ramp_Mesh_Texcoords, Ramp_Mesh_Normals) then
         raise Batch_Manager_Exception with
           "Batch_Manager.Init error loading ramp mesh data from file "
           & "src/meshes/ramp_may_2014.apg";
      end if;
      Game_Utils.Game_Log ("Batch_Manager.Init ramp_may_2014.apg loaded.");

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
      Game_Utils.Game_Log ("Batch_Manager.Init water.apg loaded.");

    end Init;

    --  -------------------------------------------------------------------------

    procedure North_Check (aBatch     : in out Batch_Meta;
                           Row, Col   : Int; Height : Integer;
                           Tiles      : Tiles_Manager.Tile_List;
                           Tile_Index : positive) is
        use Tiles_Manager;
        N_Index  : constant Integer := Tile_Index - 1 + Integer (Max_Cols);
        aTile    :  Tile_Data;
        N_Height : Integer;
        Diff     : Integer;
        X        : Single;
        Y        : Single;
        Z        : Single;
    begin
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
            for index in 1 .. 6 loop
                aBatch.Normals.Append ((0.0, 0.0, 1.0));
            end loop;
            --              Set_Texcoords ();
        end loop;

    end North_Check;

    --  -------------------------------------------------------------------------

    procedure Regenerate_Batch (Tiles       : Tiles_Manager.Tile_List;
                                Batch_Index : Positive) is
        use Tiles_Manager;
        use Batches_Package;
        use Tile_Indices_Package;
        theBatch    : Batch_Meta := Batches.Element (Batch_Index);
        Batch_Tiles : constant Tile_Indices_List := theBatch.Tiles;
        Curs        : Tile_Indices_Package.Cursor := Batch_Tiles.First;
        Tile_Index  : Positive;
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
                Total_Points := Total_Points + 6 * Diff;
            end if;
        end Add_Point_Count;

    begin
        Free_Batch_Data (Batch_Index);
        if Is_Empty (Tiles) then
            Game_Utils.Game_Log ("Regenerate_Batch, theBatch.Tiles is empty.");
            raise Batch_Manager_Exception with
              "Batch_Manager.Regenerate_Batch, theBatch.Tiles is empty.";
        else
--              Put_Line ("Batch_Manager.Regenerate_Batch Max_Cols " &
--                          Int'Image (Max_Cols));
--              Put_Line ("Batch_Manager.Regenerate_Batch Length " &
--                          Integer'Image (Integer (Batch_Tiles.Length)));
            while Has_Element (Curs) loop
                Tile_Index  := Element (Curs);
                aTile := Tiles.Element (Tile_Index);
                Row := (Int (Tile_Index) + Max_Cols - 1) / Max_Cols;
--                  Put_Line ("Batch_Manager.Regenerate_Batch Tile_Index " &
--                              Integer'Image (Tile_Index));
                if Int (Tile_Index) <= Max_Cols then
                    Column := Int (Tile_Index);
                else
                    Column := Int (Tile_Index - 1) mod (Max_Cols) + 1;
                end if;

                if Row < 1 or Row > Max_Rows or
                  Column < 1 or Column > Max_Cols then
                    raise Batch_Manager_Exception with
                      "Batch_Manager.Regenerate_Batch Tile_Index, " &
                      "Tile_Index with invalid row or col "
                      & Integer'Image (Tile_Index) & ", " &
                      Int'Image (Row) & ", " & Int'Image (Column);
                end if;

--                  Game_Utils.Game_Log ("Batch_Manager.Regenerate_Batch Tile_Index, row, col " &
--                                         Integer'Image (Tile_Index) & ", " &
--                                         Int'Image (Row) & ", " &
--                                         Int'Image (Column));
                Height := aTile.Height;
                if aTile.Tile_Type = '~' then
                    Height := Height - 1;
                    Total_Points := Total_Points + Water_Mesh_Point_Count;
                else
                    Total_Points := Total_Points + 6;
                end if;
                --  Sides count
                if Row > 1 then
                    N_Index := Tile_Index - Integer (Max_Cols) + 1;
                    N_Tile := Tiles.Element (N_Index);
                    N_Height := N_Tile.Height;
                    if N_Tile.Tile_Type = '~' then
                        N_Height := N_Height - 1;
                    end if;
                    Diff := Height - N_Height;

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
                    --                      Game_Utils.Game_Log ("Batch_Manager.Regenerate_Batch sides count: Row, N_Index 2"
                    --                                            & Int'Image (Row) & ", " & Integer'Image (N_Index));
                    if N_Tile.Tile_Type = '~' then
                        N_Height := N_Height - 1;
                    end if;
                    Diff := Height - N_Height;

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

                    if N_Tile.Tile_Type = '/' and then
                      N_Tile.Facing = 'E' then
                        Diff := Diff - 1;
                    end if;
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
                    Add_Point_Count (Diff);
                end if;
                Next (Curs);
            end loop;  -- over tiles
        end if;  --  not Tiles not empty

        theBatch.VAO.Initialize_Id;
        theBatch.Ramp_VAO.Initialize_Id;
        theBatch.Water_VAO.Initialize_Id;

        Generate_Points (theBatch, Tiles);
        Put_Line ("Batch_Manager.Regenerate_Batch Points generated");

        theBatch.Normals_VBO := GL_Utils.Create_3D_VBO
          (GL_Maths.To_Vector3_Array (theBatch.Normals));
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (Shader_Attributes.Attrib_VN, 3, Single_Type, False, 0, 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VN);
        theBatch.Normals.Clear;

        theBatch.Tex_Coords_VBO := GL_Utils.Create_2D_VBO
          (GL_Maths.To_Vector2_Array (theBatch.Tex_Coords));
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (Shader_Attributes.Attrib_VT, 2, Single_Type, False, 0, 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VT);
        theBatch.Tex_Coords.Clear;
        Put_Line ("Batch_Manager.Regenerate_Batch Tex set up");

        Generate_Ramps (theBatch, Tiles);
        Put_Line ("Batch_Manager.Regenerate_Batch Ramps generated");
        Generate_Water (theBatch, Tiles);
        Put_Line ("Batch_Manager.Regenerate_Batch Water generated");

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

    procedure South_Check (aBatch     : in out Batch_Meta;
                           Row, Col   : Int; Height : Integer;
                           Tiles      : Tiles_Manager.Tile_List;
                           Tile_Index : positive) is
        use Tiles_Manager;
        --  On entry, Row > 1; therefore Tile_Index > Max_Cols
        N_Index  : constant Integer := Tile_Index - Integer (Max_Cols) + 1;
        aTile    :  Tile_Data;
        N_Height : Integer;
        Diff     : Integer;
        X        : Single;
        Y        : Single;
        Z        : Single;
    begin
        if N_Index <= Tiles.Last_Index then
            aTile := Tiles.Element (N_Index);
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
                Z := Single (2 * Row - 1);
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
            end loop;
        end if;

    end South_Check;

    --  -------------------------------------------------------------------------

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

    procedure West_Check (aBatch     : in out Batch_Meta;
                          Row, Col   : Int; Height : Integer;
                          Tiles      : Tiles_Manager.Tile_List;
                          Tile_Index : positive) is
        use Tiles_Manager;
        N_Index  : constant Integer := Tile_Index + 1;
        aTile    :  Tile_Data;
        N_Height : Integer;
        Diff     : Integer;
        X        : Single;
        Y        : Single;
        Z        : Single;
    begin
        if N_Index <= Tiles.Last_Index then
            aTile := Tiles.Element (N_Index);
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
                Z := Single (2 * Row - 1);
                aBatch.Points.Append ((X, Y, Z));

                Z := Single (2 * Row + 1);
                aBatch.Points.Append ((X, Y, Z));
                aBatch.Points.Append ((X, Y, Z));

                Z := Single (2 * Row - 1);
                aBatch.Points.Append ((X, Y, Z));

                Y := Single (2 * (Height + level + 2));
                aBatch.Points.Append ((X, Y, Z));

                for index in 1 .. 6 loop
                    aBatch.Normals.Append ((0.0, 0.0, -1.0));
                end loop;            end loop;
        end if;

    end West_Check;

    --  -------------------------------------------------------------------------

end Batch_Manager;
