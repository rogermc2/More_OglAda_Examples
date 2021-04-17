
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Containers.Vectors;
with Ada.Exceptions;

with GL.Attributes;
with GL.Culling;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types.Colors;

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

    Yellow                   : constant GL.Types.Colors.Color := (0.6, 0.6, 0.0, 0.5);
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
        theBatches   : constant Batches_List := Batch_List;
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

    procedure Draw_Manifold_Around (Camera_Pos    : GL.Types.Singles.Vector3;
                                    Radius        : GL.Types.Single;
                                    Tile_Diff_Tex, Tile_Spec_Tex, Ramp_Diff_Tex,
                                    Ramp_Spec_Tex : GL.Objects.Textures.Texture) is
        use GL.Attributes;
        use GL.Culling;
        use GL.Toggles;
        use GL.Objects.Buffers;
        use GL.Objects.Programs;
        use GL.Objects.Vertex_Arrays;
        use GL.Types.Singles;
        use Maths;
        use Batch_Manager;
        use Batches_Package;
        use Shader_Attributes;
        use Tiles_Manager.Tile_Indices_Package;
        use GL_Maths;
        use Vec3_Package;
        use Manifold_Shader_Manager;
        theBatches    : constant Batches_List := Batch_List;
        Batch_Cursor  : Batches_Package.Cursor := theBatches.First;
        aBatch        : Batch_Meta;
        Rad_Dist      : Single;
        Light_Indices : GL_Maths.Indices_List;
        Light_Cursor  : GL_Maths.Indices_Package.Cursor;
        Light_Index1  : Positive;
        Light_Index2  : Positive;
        Tile_Index1   : Int;
        Tile_Index2   : Int;
    begin
        --        Put_Line ("Manifold.Draw_Manifold_Around theBatches size: " &
        --                 Integer'Image (Integer (theBatches.Length)));
        --          GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);
        Use_Program (Manifold_Program);
        if Camera.Is_Dirty then
            Set_View_Matrix (Translation_Matrix ((0.0, 5.0, 0.0)) * Rotate_X_Degree (Camera.View_Matrix, Degree (-80)));
            Set_Projection_Matrix (Translation_Matrix ((0.0, 0.0, -1.8)) * Camera.Projection_Matrix);
        end if;
        --        Utilities.Print_Matrix ("Manifold.Draw_Manifold_Around Camera.View_Matrix",
        --                                Camera.View_Matrix);

        if Manifold_Dyn_Light_Dirty then
            Set_Dynamic_Light_Pos (Manifold_Dyn_Light_Pos);
            Set_Dynamic_Light_Diff (Manifold_Dyn_Light_Diff);
            Set_Dynamic_Light_Spec (Manifold_Dyn_Light_Spec);
            Set_Dynamic_Light_Range (Manifold_Dyn_Light_Range);
        end if;

        --          if Settings.Shadows_Enabled then
        --              Set_Shadow_Enabled (1.0);
        --              Set_Caster_Position (Shadows.Caster_Position);
        --              Shadows.Bind_Cube_Shadow_Texture (3);
        --          else
        Set_Shadow_Enabled (0.0);
        --          end if;

        if not theBatches.Is_Empty then
            Set_Model_Matrix (Singles.Identity4);
            for index in theBatches.First_Index .. theBatches.Last_Index loop
                --          while Has_Element (Batch_Cursor) loop
                --              aBatch := Element (Batch_Cursor);
                aBatch := theBatches.Element (index);
                Rad_Dist := Min (abs (Camera_Pos (GL.X) - aBatch.AABB_Mins (GL.X)),
                                 abs (Camera_Pos (GL.X) - aBatch.AABB_Maxs (GL.X)));
                if Rad_Dist <= 2.0 * Radius then
                    Rad_Dist := Min (abs (Camera_Pos (GL.Z) - aBatch.AABB_Mins (GL.Z)),
                                     abs (Camera_Pos (GL.Z) - aBatch.AABB_Maxs (GL.Z)));
                    if Rad_Dist <= 2.0 * Radius then
                        if Frustum.Is_Aabb_In_Frustum
                          (aBatch.AABB_Mins, aBatch.AABB_Maxs) and
                          not (aBatch.Static_Light_Indices.Is_Empty) then
                            --                          Put_Line ("Manifold.Draw_Manifold_Around Aabb_In_Frustum");
                            Light_Indices := aBatch.Static_Light_Indices;
                            Light_Cursor := Light_Indices.First;
                            Light_Index1 := Light_Indices.First_Index;
                            Light_Index2 := Light_Index1 + 1;
                            Tile_Index1 := Int (Light_Indices.Element (Light_Index1));
                            Tile_Index2 := Int (Light_Indices.Element (Light_Index2));
                            --                          Set_Static_Light_Indices ((Tile_Index1, Tile_Index2));

                            if not aBatch.Points.Is_Empty then
                                --  flat tiles
                                GL_Utils.Bind_Vao (aBatch.Points_VAO);
                                --  Bind_Texture sets active unit and binds texture
                                --  to Texture_Target Texture_2D
                                Texture_Manager.Bind_Texture (0, Tile_Diff_Tex);
                                Texture_Manager.Bind_Texture (1, Tile_Spec_Tex);
                                --                              Put_Line ("Manifold.Draw_Manifold_Around flat tiles Draw_Arrays");
                                --  Draws start scene
                                Draw_Arrays (Triangles, 0, Int (aBatch.Points.Length));
                                --                              Draw_Arrays (Points, 0, 1);
                            end if;

                            if not aBatch.Ramp_Points.Is_Empty then
                                --  ramps
                                GL.Objects.Vertex_Arrays.Bind (aBatch.Ramp_Vao);

                                --  Bind_Texture sets active unit and binds texture
                                --  to Texture_Target Texture_2D
                                --                              Texture_Manager.Bind_Texture (0, Tile_Diff_Tex);
                                --                              Texture_Manager.Bind_Texture (1, Tile_Spec_Tex);

                                if Settings.Render_OLS then
                                    Set_Front_Face (Clockwise);
                                    Set_Outline_Pass (1.0);
                                    --                          Draw_Arrays (Triangles, 0, Int (aBatch.Ramp_Points.Length));
                                    Set_Outline_Pass (0.0);
                                    Set_Front_Face (Counter_Clockwise);
                                end if;
                                --  regular pass
                                --  Bind_Texture sets active unit and binds texture
                                --  to Texture_Target Texture_2D
                                --                              Texture_Manager.Bind_Texture (0, Ramp_Diff_Tex);
                                --                              Texture_Manager.Bind_Texture (1, Ramp_Spec_Tex);
                                --                       Put_Line ("Manifold.Draw_Manifold_Around regular pass Draw_Arrays");
                                --                       Draw_Arrays (Triangles, 0, Int (aBatch.Ramp_Points.Length));
                                --                              Draw_Arrays (Points, 0, 1);
                            end if;
                        end if;
                    end if;
                end if;
                --              Next (Batch_Cursor);
            end loop;
        end if;

        --        Draw_Water_Manifold_Around;

    exception
        when anError : others =>
            Put_Line ("Manifold.Draw_Manifold_Around exception");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
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
        theBatches    : constant Batches_List := Batch_List;
        Curs          : Cursor := theBatches.First;
        aBatch        : Batch_Meta;
        Light_Indices : GL_Maths.Indices_List;
    begin
        Enable (Depth_Test);
        Shadows.Set_Depth_Model_Matrix (Singles.Identity4);
        GL.Objects.Programs.Use_Program (Manifold_Program);
        while Has_Element (Curs) loop
            aBatch := Element (Curs);
            if Frustum.Is_Aabb_In_Frustum (aBatch.AABB_Mins, aBatch.Aabb_Maxs) then
                --  Flat Tiles
                GL_Utils.Bind_Vao (aBatch.Points_VAO);
                Draw_Arrays (Triangles, 0, Int (aBatch.Points.Length));
                --              Put_Line ("Manifold.Draw_Manifold_Around_Depth_Only, Ramp_Points.Length"
                --                        & Integer'Image (Integer (aBatch.Ramp_Points.Length)));
                if not aBatch.Ramp_Points.Is_Empty then
                    GL_Utils.Bind_Vao (aBatch.Ramp_Vao);
                    Draw_Arrays (Triangles, 0, Int (aBatch.Ramp_Points.Length));
                end if;
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
        theBatches    : constant Batches_List := Batch_List;
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
        Batch_Manager.Clear_Batch_Data;
    end Free_Manifold_Meta_Data;

    --  ----------------------------------------------------------------------------

    function Get_Light_Index (Column, Row : Positive; Light_Number : Positive)
                              return GL.Types.Int is
        use Ada.Calendar;
        use GL.Types;
        use Batch_Manager;
        use GL_Maths.Indices_Package;
        --          Start_Time    : Time := Clock;
        Batch_Index   : Positive;
        Light_Indices : GL_Maths.Indices_List;
        Light_Cursor  : Cursor;
        Found         : Boolean := False;
        Result        : Int := -1;
    begin
        Batch_Index := Get_Batch_Index (Column, Row);
        Light_Indices := Static_Indices (Batch_Index);
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

        if Found then
            Result := Int (Element (Light_Cursor));
        end if;
        --          Put_Line ("Manifold.Get_Light_Index time taken : "
        --                   & Duration'Image ((Clock - Start_Time) * 1000) & "ms");
        return Result;

    end Get_Light_Index;

    --  ------------------------------------------------------------------------

    procedure Init is
        Points    : GL_Maths.Vec3_List;
        Texcoords : GL_Maths.Vec2_List;

    begin
        Game_Utils.Game_Log ("Initializing manifold.");
        Manifold_Shader_Manager.Init_Shaders (Manifold_Program);
        GL.Objects.Programs.Use_Program (Manifold_Program);
        Manifold_Shader_Manager.Set_Ambient_Light ((0.0125, 0.0125, 0.0125));
        Manifold_Shader_Manager.Set_Diff_Map (0);
        Manifold_Shader_Manager.Set_Spec_Map (1);
        Manifold_Shader_Manager.Set_Cube_Texture (3);

        Water_Shader_Manager.Init_Shaders (Water_Program);
        GL.Objects.Programs.Use_Program (Water_Program);
        Water_Shader_Manager.Set_K_Diff ((0.03, 0.50, 0.20, 0.75));
        Water_Shader_Manager.Set_K_Spec ((0.5, 0.5, 0.5, 1.0));
        Water_Shader_Manager.Set_Ambient_Light ((0.0125, 0.0125, 0.0125));
        Water_Shader_Manager.Set_Cube_Texture (3);

        Batch_Manager.Init_Batch_Data;

    end Init;

    --  ----------------------------------------------------------------------------

    function Is_Ramp (Pos : Ints.Vector2) return Boolean is
        use Batch_Manager;
        use Tiles_Manager;
        aTile : constant Tile_Data :=
                  Get_Tile (Natural (Pos (GL.X)) * Max_Map_Cols +
                              Natural (Pos (GL.Y)));
    begin
        return aTile.Tile_Type = '/';
    end Is_Ramp;

    --  ----------------------------------------------------------------------------

    function Is_Ramp (Index : Natural) return Boolean is
        use Tiles_Manager;
        aTile : constant Tile_Data := Get_Tile (Index);
    begin
        return aTile.Tile_Type = '/';
    end Is_Ramp;

    --  ----------------------------------------------------------------------------

    function Is_Water (Pos : Ints.Vector2) return Boolean is
        use Batch_Manager;
        use Tiles_Manager;
        aTile : constant Tile_Data :=
                  Get_Tile (Natural (Pos (GL.X)) * Max_Map_Cols +
                              Natural (Pos (GL.Y)));
    begin
        return aTile.Tile_Type = '~';
    end Is_Water;

    --  ----------------------------------------------------------------------------

    function Is_Water (Index : Natural) return Boolean is
        use Tiles_Manager;
        aTile : constant Tile_Data := Get_Tile (Index);
    begin
        return aTile.Tile_Type = '~';
    end Is_Water;

    --  ----------------------------------------------------------------------------

    procedure Reset_Manifold_Vars is
        use Tiles_Manager;
    begin
        Batches_Across := 0;
        Batches_Down := 0;
        Batch_Split_Count := 0;
        Max_Map_Cols := 0;
        Max_Map_Rows := 0;
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
       Light_Range         : GL.Types.Single) is
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
        aLight    : Static_Light_Data;
        Positions : Singles.Vector3_Array (1 .. 32) := (others => Maths.Vec3_0);
        Diffuse   : Singles.Vector3_Array (1 .. 32) := (others => Maths.Vec3_0);
        Specular  : Singles.Vector3_Array (1 .. 32) := (others => Maths.Vec3_0);
        Ranges    : Single_Array (1 .. 32) := (others => 0.0);
    begin
        for Light_Index in Static_Lights.First_Index ..
          Static_Lights.Last_Index loop
            aLight := Element (Static_Lights, Light_Index);
            Positions (Int (Light_Index)) := aLight.Position;
            Diffuse (Int (Light_Index)) := aLight.Diffuse;
            Specular (Int (Light_Index)) := aLight.Specular;
            Ranges (Int (Light_Index)) := aLight.Light_Range;
        end loop;

        --        Utilities.Print_GL_Array3 ("Manifold.Update_Static_Lights_Uniforms Diffuse",
        --                                  Diffuse);
        --        Utilities.Print_GL_Array3 ("Manifold.Update_Static_Lights_Uniforms Specular",
        --                                  Specular);
        --        Utilities.Print_GL_Single_Array
        --            ("Manifold.Update_Static_Lights_Uniforms Ranges", Ranges);

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
