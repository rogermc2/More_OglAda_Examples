
with Ada.Containers.Vectors;

with GL.Attributes;
with GL.Culling;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;

with Glfw;

with Maths;
with Utilities;

with Batch_Manager;
with Camera;
with Frustum;
with Game_Utils;
with GL_Maths;
with GL_Utils;
with Manifold_Shader_Manager;
with Mesh_Loader;
with Settings;
with Shader_Attributes;
WITH Shadows;
with Texture_Manager;
with Tiles_Manager;
with Water_Shader_Manager;

package body Manifold is
    use GL.Types;

    Batch_Split_Count        : Integer := 0;
    Manifold_Program         : GL.Objects.Programs.Program;
    Water_Program            : GL.Objects.Programs.Program;
    Manifold_Dyn_Light_Dirty : Boolean := True;
    Manifold_Dyn_Light_Pos   : Singles.Vector3 := Maths.Vec3_0;
    Manifold_Dyn_Light_Diff  : Singles.Vector3 := Maths.Vec3_0;
    Manifold_Dyn_Light_Spec  : Singles.Vector3 := Maths.Vec3_0;
    Manifold_Dyn_Light_Range : Single := 1.0;
    Count                    : Integer := 0;

    procedure Draw_Water_Manifold_Around;

    --  ----------------------------------------------------------------------------

    procedure Clear_Manifold_Lights is
        use Batch_Manager;
        use Batches_Package;
        use Tiles_Manager.Tile_Indices_Package;
        theBatches   : constant Batches_List := Batches;
        Batch_Cursor : Batches_Package.Cursor := theBatches.First;
        Batch        : Batch_Manager.Batch_Meta;
    begin
        while Has_Element (Batch_Cursor) loop
            Batch := Element (Batch_Cursor);
            Batch.Static_Light_Indices.Clear;
            Next (Batch_Cursor);
        end loop;
    end Clear_Manifold_Lights;

    --  -------------------------------------------------------------------------

    procedure Draw_Manifold_Around (Camera_Pos : GL.Types.Singles.Vector3;
                                    Radius     : GL.Types.Single;
                                    Tile_Tex, Tile_Spec_Tex, Ramp_Diff_Tex,
                                    Ramp_Spec_Tex : GL.Objects.Textures.Texture) is
        use GL.Attributes;
        use GL.Culling;
        use GL.Toggles;
        use GL.Objects.Buffers;
        use GL.Objects.Programs;
        use GL.Objects.Vertex_Arrays;
        use Maths;
        use Batch_Manager;
        use Batches_Package;
        use Shader_Attributes;
        use Tiles_Manager.Tile_Indices_Package;
        use GL_Maths;
        use Vec3_Package;
        use Manifold_Shader_Manager;
        theBatches    : constant Batches_List := Batches;
        Curs          : Batches_Package.Cursor := theBatches.First;
        aBatch        : Batch_Meta;
        Rad_Dist      : Single;
        Light_Indices : GL_Maths.Indices_List;
        Light_Cursor  : GL_Maths.Indices_Package.Cursor;
        Light_Index1  : Positive;
        Light_Index2  : Positive;
        Tile_Index1   : Int;
        Tile_Index2   : Int;
    begin
        --          GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);
        Use_Program (Manifold_Program);
        if Camera.Is_Dirty then
            Set_View_Matrix (Camera.View_Matrix);
            Set_Projection_Matrix (Camera.Projection_Matrix);
        end if;

        if Manifold_Dyn_Light_Dirty then
            Set_Dynamic_Light_Pos (Manifold_Dyn_Light_Pos);
            Set_Dynamic_Light_Diff (Manifold_Dyn_Light_Diff);
            Set_Dynamic_Light_Spec (Manifold_Dyn_Light_Spec);
            Set_Dynamic_Light_Range (Manifold_Dyn_Light_Range);
        end if;

        if Settings.Shadows_Enabled then
            Set_Shadow_Enabled (1.0);
            Set_Caster_Position (Shadows.Caster_Position);
            Shadows.Bind_Cube_Shadow_Texture (3);
        else
            Set_Shadow_Enabled (0.0);
        end if;

        Set_Model_Matrix (Singles.Identity4);

        while Has_Element (Curs) loop
            aBatch := Element (Curs);
            Rad_Dist := Min (abs (Camera_Pos (GL.X) - aBatch.AABB_Mins (GL.X)),
                             abs (Camera_Pos (GL.X) - aBatch.AABB_Maxs (GL.X)));
            if Rad_Dist <= 2.0 * Radius then
                Rad_Dist := Min (abs (Camera_Pos (GL.Z) - aBatch.AABB_Mins (GL.Z)),
                                 abs (Camera_Pos (GL.Z) - aBatch.AABB_Maxs (GL.Z)));
                if Rad_Dist <= 2.0 * Radius then
                    if Frustum.Is_Aabb_In_Frustum
                      (aBatch.AABB_Mins, aBatch.AABB_Maxs) and
                      not (aBatch.Static_Light_Indices.Is_Empty) then
                        Light_Indices := aBatch.Static_Light_Indices;
                        Light_Cursor := Light_Indices.First;
                        Light_Index1 := Light_Indices.First_Index;
                        Light_Index2 := Light_Index1 + 1;
                        Tile_Index1 := Int (Light_Indices.Element (Light_Index1));
                        Tile_Index2 := Int (Light_Indices.Element (Light_Index2));
                        Set_Static_Light_Indices ((Tile_Index1, Tile_Index2));

                        if not aBatch.Points.Is_Empty then
                            --  flat tiles
                            GL_Utils.Bind_Vao (aBatch.Points_VAO);
                            --  Bind_Texture sets active unit and binds texture
                            --  to Texture_Target Texture_2D
                            Texture_Manager.Bind_Texture (0, Tile_Tex);
                            Texture_Manager.Bind_Texture (1, Tile_Spec_Tex);
                            Draw_Arrays (Triangles, 0, Int (aBatch.Points.Length));
                        end if;

                        if not aBatch.Ramp_Points.Is_Empty then
                            --  ramps
                            GL.Objects.Vertex_Arrays.Bind (aBatch.Ramp_Vao);

                            --  Bind_Texture sets active unit and binds texture
                            --  to Texture_Target Texture_2D
                            Texture_Manager.Bind_Texture (0, Tile_Tex);
                            Texture_Manager.Bind_Texture (1, Tile_Spec_Tex);

                            if Settings.Render_OLS then
                                Set_Front_Face (Clockwise);
                                Set_Outline_Pass (1.0);
                                Draw_Arrays (Triangles, 0, Int (aBatch.Ramp_Points.Length));
                                Set_Outline_Pass (0.0);
                                Set_Front_Face (Counter_Clockwise);
                            end if;
                            --  regular pass
                            --  Bind_Texture sets active unit and binds texture
                            --  to Texture_Target Texture_2D
                            Texture_Manager.Bind_Texture (0, Ramp_Diff_Tex);
                            Texture_Manager.Bind_Texture (1, Ramp_Spec_Tex);
                            --                              Draw_Arrays (Triangles, 0, Int (aBatch.Ramp_Point_Count));
                        end if;
                    end if;
                end if;
            end if;
            Next (Curs);
        end loop;

        Draw_Water_Manifold_Around;

    exception
        when others =>
            Put_Line ("Manifold.Draw_Manifold_Around exception");
            raise;
    end  Draw_Manifold_Around;

    --  ----------------------------------------------------------------------------

    procedure Draw_Manifold_Around_Depth_Only is
        use GL.Attributes;
        use GL.Toggles;
        use GL.Objects.Vertex_Arrays;
        use Batch_Manager;
        use Batches_Package;
        use Shader_Attributes;
        theBatches    : constant Batches_List := Batches;
        Curs          : Cursor := theBatches.First;
        aBatch        : Batch_Meta;
        Light_Indices : GL_Maths.Indices_List;
    begin
        Enable (Depth_Test);
        Shadows.Set_Depth_Model_Matrix (Singles.Identity4);
        while Has_Element (Curs) loop
            aBatch := Element (Curs);
            if Frustum.Is_Aabb_In_Frustum (aBatch.AABB_Mins, aBatch.Aabb_Maxs) then
                --  Flat Tiles
                GL_Utils.Bind_Vao (aBatch.Points_VAO);
                Draw_Arrays (Triangles, 0, Int (aBatch.Points.Length));

                GL_Utils.Bind_Vao (aBatch.Ramp_Vao);
                Draw_Arrays (Triangles, 0, Int (aBatch.Ramp_Points.Length));
            end if;
            Next (Curs);
        end loop;
    end  Draw_Manifold_Around_Depth_Only;

    --  ----------------------------------------------------------------------------

    procedure Draw_Water_Manifold_Around is
        use GL.Attributes;
        use GL.Culling;
        use GL.Toggles;
        use GL.Objects.Programs;
        use GL.Objects.Vertex_Arrays;
        use Maths;
        use Batch_Manager;
        use Batches_Package;
        use GL_Maths;
        use GL_Maths.Indices_Package;
        use Shader_Attributes;
        use Vec3_Package;
        use Water_Shader_Manager;
        theBatches    : constant Batches_List := Batches;
        Curs          : Batches_Package.Cursor := theBatches.First;
        aBatch        : Batch_Meta;
        Light_Indices : Indices_List;
        Light_Cursor  : Indices_Package.Cursor;
        Tile_Index1   : Int;
        Tile_Index2   : Int;
    begin
        Use_Program (Water_Program);
        if Camera.Is_Dirty then
            Set_View_Matrix (Camera.View_Matrix);
            Set_Projection_Matrix (Camera.Projection_Matrix);
        end if;

        if Manifold_Dyn_Light_Dirty then
            Set_Dynamic_Light_Pos (Manifold_Dyn_Light_Pos);
            Set_Dynamic_Light_Diff (Manifold_Dyn_Light_Diff);
            Set_Dynamic_Light_Spec (Manifold_Dyn_Light_Spec);
            Set_Dynamic_Light_Range (Manifold_Dyn_Light_Range);
        end if;

        if Settings.Shadows_Enabled then
            Set_Shadow_Enabled (1.0);
            Set_Caster_Position (Shadows.Caster_Position);
            Shadows.Bind_Cube_Shadow_Texture (3);
        else
            Set_Shadow_Enabled (0.0);
        end if;

        Set_Model_Matrix (Singles.Identity4);
        Set_Animation_Time (Single (Glfw.Time));

        Enable (Blend);
        while Has_Element (Curs) loop
            aBatch := Element (Curs);
            if Frustum.Is_Aabb_In_Frustum
              (aBatch.AABB_Mins, aBatch.AABB_Maxs) and
               not aBatch.Water_Points.Is_Empty then
                Light_Indices := aBatch.Static_Light_Indices;
                Light_Cursor := Light_Indices.First;
                Tile_Index1 := Int (Element (Light_Cursor));
                Tile_Index2 := Int (Element (Next (Light_Cursor)));
                Set_Static_Light_Indices ((Tile_Index1, Tile_Index2));

                GL_Utils.Bind_Vao (aBatch.Water_VAO);
                Draw_Arrays (Triangles, 0, Int (aBatch.Water_Points.Length));
            end if;
            Next (Curs);
        end loop;
        Disable (Blend);
        Manifold_Dyn_Light_Dirty := False;
    end  Draw_Water_Manifold_Around;

    --  ----------------------------------------------------------------------------

    procedure Free_Manifold_Meta_Data is
    begin
        Batch_Manager.Clear;
    end Free_Manifold_Meta_Data;

    --  ----------------------------------------------------------------------------

    function Get_Light_Index (Column, Row : Positive; Light_Number : Integer)
                              return GL.Types.Int is
        use GL.Types;
        use Batch_Manager;
        use GL_Maths.Indices_Package;
        Batch_Index   : constant Positive :=
                          Get_Batch_Index (Column, Row);
        Batch         : Batch_Meta;
        Light_Indices : GL_Maths.Indices_List;
        Light_Cursor  : Cursor;
        --        Light_Index   : Positive;
        --        theLight      : Batch_Manager.Static_Light_Data;
        Found         : Boolean := False;
        Result        : Int := -1;
    begin
        if not Batches.Is_Empty then
            Batch := Batches.Element (Batch_Index);
            Light_Indices := Batch.Static_Light_Indices;
            if Light_Number > Integer (Light_Indices.Length) then
                raise Manifold_Exception with
                  "Manifold.Get_Light_Index; Light number " &
                  Integer'Image (Light_Number) & " requested at ( " &
                  Positive'Image (Column) & "," & Positive'Image (Row) &
                  ") in batch " &  Integer'Image (Batch_Index) &
                  " does not exist.";
            end if;
            Light_Cursor := Light_Indices.First;
            while Has_Element (Light_Cursor) and not Found loop
                Found := Element (Light_Cursor) = Light_Number;
                if not Found then
                    Next (Light_Cursor);
                end if;
            end loop;
        end if;
        if Found then
            --           Light_Index := Element (Light_Cursor);
            --           theLight := Static_Lights.Element (Light_Index);
            Result := Int (Element (Light_Cursor));
        end if;
        return Result;
    end Get_Light_Index;

    --  ------------------------------------------------------------------------

    procedure Init is
        Points       : GL_Maths.Vec3_List;
        Texcoords    : GL_Maths.Vec2_List;
        Points_Count : Integer := 0;

    begin
        Game_Utils.Game_Log ("Initializing manifold.");
        Manifold_Shader_Manager.Init (Manifold_Program);
        GL.Objects.Programs.Use_Program (Manifold_Program);
        Manifold_Shader_Manager.Set_Ambient_Light ((0.0125, 0.0125, 0.0125));
        Manifold_Shader_Manager.Set_Diff_Map (0);
        Manifold_Shader_Manager.Set_Spec_Map (1);
        Manifold_Shader_Manager.Set_Cube_Texture (3);

        Water_Shader_Manager.Init (Water_Program);
        GL.Objects.Programs.Use_Program (Water_Program);
        Water_Shader_Manager.Set_K_Diff ((0.03, 0.50, 0.20, 0.75));
        Water_Shader_Manager.Set_K_Spec ((0.5, 0.5, 0.5, 1.0));
        Water_Shader_Manager.Set_Ambient_Light ((0.0125, 0.0125, 0.0125));
        Water_Shader_Manager.Set_Cube_Texture (3);

        Batch_Manager.Init;

    end Init;

    --  ----------------------------------------------------------------------------

    function Is_Ramp (Pos : Ints.Vector2) return Boolean is
        use Tiles_Manager;
        aTile : constant Tile_Data := Get_Tile (Pos);
    begin
        return aTile.Tile_Type = '/';
    end Is_Ramp;

    --  ----------------------------------------------------------------------------

    function Is_Water (Pos : Ints.Vector2) return Boolean is
        use Tiles_Manager;
        aTile : constant Tile_Data := Get_Tile (Pos);
    begin
        return aTile.Tile_Type = '~';
    end Is_Water;

    --  ----------------------------------------------------------------------------

    procedure Reset_Manifold_Vars is
        use Batch_Manager;
    begin
        Batches_Across := 0;
        Batches_Down := 0;
        Batch_Split_Count := 0;
        Batch_Manager.Max_Cols := 0;
        Batch_Manager.Max_Rows := 0;
        Tiles_Manager.Reset_Vars;
    end Reset_Manifold_Vars;

    --  ----------------------------------------------------------------------------

    procedure Set_Manifold_Ambient_Light (Level : GL.Types.Singles.Vector3) is
    begin
        GL.Objects.Programs.Use_Program (Manifold_Program);
        Manifold_Shader_Manager.Set_Ambient_Light (Level);
        Water_Shader_Manager.Set_Ambient_Light (Level);
    end Set_Manifold_Ambient_Light;

    --  -------------------------------------------------------------------------

    procedure Update_Manifold_Dynamic_Light
      (Pos_Wor, Diff, Spec : GL.Types.Singles.Vector3;
       Light_Range : GL.Types.Single) is
    begin
        Manifold_Dyn_Light_Dirty := True;
        Manifold_Dyn_Light_Pos := Pos_Wor;
        Manifold_Dyn_Light_Diff := Diff;
        Manifold_Dyn_Light_Spec := Spec;
        Manifold_Dyn_Light_Range := Light_Range;
    end Update_Manifold_Dynamic_Light;

    --  -------------------------------------------------------------------------

    procedure Update_Static_Lights_Uniforms  is
        use Batch_Manager;
        use GL_Maths.Indices_Package;
        Lights    : constant Static_Light_Vector := Static_Lights;
        Index     : Positive := Lights.First_Index;
        aLight    : Static_Light_Data;
        Positions : Singles.Vector3_Array (1 .. 32) := (others => Maths.Vec3_0);
        Diffuse   : Singles.Vector3_Array (1 .. 32) := (others => Maths.Vec3_0);
        Specular  : Singles.Vector3_Array (1 .. 32) := (others => Maths.Vec3_0);
        Ranges    : Single_Array (1 .. 32) := (others => 0.0);
    begin
        while Index <= Static_Lights.Last_Index loop
            aLight := Element (Static_Lights, Index);
            Positions (Int (Index)) := aLight.Position;
            Diffuse (Int (Index)) := aLight.Diffuse;
            Specular (Int (Index)) := aLight.Specular;
            Ranges (Int (Index)) := aLight.Light_Range;
            Index := Index + 1;
        end loop;

        GL.Objects.Programs.Use_Program (Manifold_Program);
        Manifold_Shader_Manager.Set_Light_Positions (Positions);
        Manifold_Shader_Manager.Set_Lights_Diffuse (Diffuse);
        Manifold_Shader_Manager.Set_Lights_Specular (Specular);
        Manifold_Shader_Manager.Set_Light_Ranges (Ranges);

        GL.Objects.Programs.Use_Program (Water_Program);
        Water_Shader_Manager.Set_Light_Positions (Positions);
        Water_Shader_Manager.Set_Light_Diffuse (Diffuse);
        Water_Shader_Manager.Set_Light_Specular (Specular);
        Water_Shader_Manager.Set_Light_Range (Ranges);
    end Update_Static_Lights_Uniforms;

    --  ----------------------------------------------------------------------------

end Manifold;
