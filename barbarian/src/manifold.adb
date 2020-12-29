
with Ada.Containers.Vectors;

with GL.Culling;
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
WITH Shadows;
with Texture_Manager;
with Tiles_Manager;
with Water_Shader_Manager;

package body Manifold is
   use GL.Types;

   Manifold_Program         : GL.Objects.Programs.Program;
   Water_Program            : GL.Objects.Programs.Program;
   Ramp_Mesh_Points         : GL_Maths.Vec3_List;
   Ramp_Mesh_Normals        : GL_Maths.Vec3_List;
   Ramp_Mesh_Smooth_Normals : GL_Maths.Vec3_List;
   Ramp_Mesh_Texcoords      : GL_Maths.Vec2_List;
   Ramp_Mesh_Point_Count    : Integer := 0;
   Water_Mesh_Points        : GL_Maths.Vec3_List;
   Water_Mesh_Normals       : GL_Maths.Vec3_List;
   Water_Mesh_Texcoords     : GL_Maths.Vec2_List;
   Water_Mesh_Point_Count   : Integer := 0;
   Manifold_Dyn_Light_Dirty : Boolean := True;
   Manifold_Dyn_Light_Pos   : constant Singles.Vector3 := Maths.Vec3_0;
   Manifold_Dyn_Light_Diff  : constant Singles.Vector3 := Maths.Vec3_0;
   Manifold_Dyn_Light_Spec  : constant Singles.Vector3 := Maths.Vec3_0;
   Manifold_Dyn_Light_Range : constant Single := 1.0;

   procedure Draw_Water_Manifold_Around
      (Tile_Tex, Tile_Spec_Tex : GL.Objects.Textures.Texture);

   --  ----------------------------------------------------------------------------

   procedure Clear_Manifold_Lights is
      use Batch_Manager;
      use Batches_Package;
      use Tile_Indices_Package;
      theBatches   : constant Batches_List := Batches;
      Batch_Cursor : Batches_Package.Cursor := theBatches.First;
      Batch        : Batch_Manager.Batch_Meta;
   begin
      while Has_Element (Batch_Cursor) loop
         Batch := Element (Batch_Cursor);
         Clear (Batch.Static_Light_Indices);
         Next (Batch_Cursor);
      end loop;
   end Clear_Manifold_Lights;

   --  -------------------------------------------------------------------------

   procedure Draw_Manifold_Around (Camera_Pos : GL.Types.Singles.Vector3;
                                   Radius     : GL.Types.Single;
                                   Tile_Tex, Tile_Spec_Tex, Ramp_Diff_Tex,
                                   Ramp_Spec_Tex : GL.Objects.Textures.Texture) is
      use GL.Culling;
      use GL.Toggles;
      use GL.Objects.Programs;
      use GL.Objects.Vertex_Arrays;
      use Maths;
      use Batch_Manager;
      use Batches_Package;
      use Tile_Indices_Package;
      use GL_Maths;
      use Vec3_Package;
      use Manifold_Shader_Manager;
      theBatches    : constant Batches_List := Batches;
      Curs          : Batches_Package.Cursor := theBatches.First;
      aBatch        : Batch_Meta;
      Rad_Dist      : Single;
      Light_Indices : Tile_Indices_List;
      Light_Cursor  : Tile_Indices_Package.Cursor;
      Tile_Index1   : Int;
      Tile_Index2   : Int;
   begin
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
         Rad_Dist := Min (abs(Camera_Pos (GL.X) - aBatch.AABB_Mins (GL.X)),
                          abs(Camera_Pos (GL.X) - aBatch.AABB_Maxs (GL.X)));
         if Rad_Dist <= 2.0 * Radius then
--              Put_Line ("Manifold.Draw_Manifold_Around Rad_Dist <= 2.0 * Radius 1");
            Rad_Dist := Min (abs(Camera_Pos (GL.Z) - aBatch.AABB_Mins (GL.Z)),
                             abs(Camera_Pos (GL.Z) - aBatch.AABB_Maxs (GL.Z)));
            if Rad_Dist <= 2.0 * Radius then
               Put_Line ("Manifold.Draw_Manifold_Around Is_Aabb_In_Frustum: "
                & Boolean'Image (Frustum.Is_Aabb_In_Frustum
                           (aBatch.AABB_Mins, aBatch.AABB_Maxs)));
                Utilities.Print_Vector ("Manifold.Draw_Manifold_Around AABB_Mins",
                                        aBatch.AABB_Mins);
                Utilities.Print_Vector ("Manifold.Draw_Manifold_Around AABB_Mins",
                                        aBatch.AABB_Maxs);
               if Frustum.Is_Aabb_In_Frustum
                 (aBatch.AABB_Mins, aBatch.AABB_Maxs) and
                 not (aBatch.Static_Light_Indices.Is_Empty) then
                  Put_Line ("Manifold.Draw_Manifold_Around Is_Aabb_In_Frustum");
                  Light_Indices := aBatch.Static_Light_Indices;
                  Light_Cursor := Light_Indices.First;
                  Tile_Index1 := Int (Element (Light_Cursor));
                  Tile_Index2 := Int (Element (Next (Light_Cursor)));
                  Set_Static_Light_Indices ((Tile_Index1, Tile_Index2));

                  if not Is_Empty (aBatch.Points) then
                     --  flat tiles
                     Texture_Manager.Bind_Texture (0, Tile_Tex);
                     Texture_Manager.Bind_Texture (1, Tile_Spec_Tex);
                     GL_Utils.Bind_Vao (aBatch.Vao);
                     Put_Line ("Manifold.Draw_Manifold_Around drawing aBatch.Points ");
                     Draw_Arrays (Triangles, 0, Int (aBatch.Points.Length));
                  end if;
                  if not Is_Empty (aBatch.Ramp_Points) then
                     --  ramps
                     GL_Utils.Bind_Vao (aBatch.Ramp_Vao);
                     if Settings.Render_OLS then
                        Set_Cull_Face (Front);
                        Set_Front_Face (Clockwise);
                        Set_Outline_Pass (1.0);
                        Draw_Arrays (Triangles, 0, Int (aBatch.Ramp_Points.Length));
                        Set_Outline_Pass (0.0);
                        Set_Front_Face (Counter_Clockwise);
                     end if;
                     --  regular pass
                     Put_Line ("Manifold.Draw_Manifold_Around aBatch.Ramp_Points.Length "
                     & Integer'Image (Integer (aBatch.Ramp_Points.Length)));
                     Texture_Manager.Bind_Texture (0, Ramp_Diff_Tex);
                     Texture_Manager.Bind_Texture (1, Ramp_Spec_Tex);
                     Draw_Arrays (Triangles, 0, Int (aBatch.Ramp_Points.Length));
                  end if;
               end if;
            end if;
         end if;
         Next (Curs);
      end loop;

      Draw_Water_Manifold_Around  (Tile_Tex, Tile_Spec_Tex);
      Manifold_Dyn_Light_Dirty := False;

   exception
            when others =>
                Put_Line ("Manifold.Draw_Manifold_Around exception");
                raise;
   end  Draw_Manifold_Around;

   --  ----------------------------------------------------------------------------

   procedure Draw_Manifold_Around_Depth_Only is
      use GL.Toggles;
      use GL.Objects.Vertex_Arrays;
      use Batch_Manager;
      use Batches_Package;
      theBatches    : constant Batches_List := Batches;
      Curs          : Cursor := theBatches.First;
      aBatch        : Batch_Meta;
      Light_Indices : Tile_Indices_List;
   begin
      Enable (Depth_Test);
      Shadows.Set_Depth_Model_Matrix (Singles.Identity4);
      while Has_Element (Curs) loop
         aBatch := Element (Curs);
         if Frustum.Is_Aabb_In_Frustum (aBatch.AABB_Mins, aBatch.Aabb_Maxs) then
            --  Flat Tiles
            GL_Utils.Bind_Vao (aBatch.Vao);
            Draw_Arrays (Triangles, 0, Int (aBatch.Points.Length));
            GL_Utils.Bind_Vao (aBatch.Ramp_Vao);
            Draw_Arrays (Triangles, 0, Int (aBatch.Points.Length));
         end if;
         Next (Curs);
      end loop;
   end  Draw_Manifold_Around_Depth_Only;

   --  ----------------------------------------------------------------------------

   procedure Draw_Water_Manifold_Around
      (Tile_Tex, Tile_Spec_Tex : GL.Objects.Textures.Texture) is
      use GL.Culling;
      use GL.Toggles;
      use GL.Objects.Programs;
      use GL.Objects.Vertex_Arrays;
      use Maths;
      use Batch_Manager;
      use Batches_Package;
      use Tile_Indices_Package;
      use GL_Maths;
      use Vec3_Package;
      use Water_Shader_Manager;
      theBatches    : constant Batches_List := Batches;
      Curs          : Batches_Package.Cursor := theBatches.First;
      aBatch        : Batch_Meta;
      Light_Indices : Tile_Indices_List;
      Light_Cursor  : Tile_Indices_Package.Cursor;
      Tile_Index1   : Single;
      Tile_Index2   : Single;
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
           not (aBatch.Static_Light_Indices.Is_Empty) then

            if not Is_Empty (aBatch.Water_Points) then
               --  flat tiles
               Texture_Manager.Bind_Texture (0, Tile_Tex);
               Texture_Manager.Bind_Texture (1, Tile_Spec_Tex);
               GL_Utils.Bind_Vao (aBatch.Vao);
               Draw_Arrays (Triangles, 0, Int (aBatch.Points.Length));
            end if;
            if not Is_Empty (aBatch.Ramp_Points) then
               Light_Indices := aBatch.Static_Light_Indices;
               Light_Cursor := Light_Indices.First;
               Tile_Index1 := Single (Element (Light_Cursor));
               Tile_Index2 := Single (Element (Next (Light_Cursor)));
               Set_Static_Light_Indices ((Tile_Index1, Tile_Index2));

               GL_Utils.Bind_Vao (aBatch.Ramp_Vao);
               Draw_Arrays (Triangles, 0, Int (aBatch.Water_Points.Length));
            end if;
         end if;
         Next (Curs);
      end loop;
      Disable (Blend);

   end  Draw_Water_Manifold_Around;

   --  ----------------------------------------------------------------------------

   procedure Free_Manifold_Mesh_Data is
   begin
      Batch_Manager.Clear;
      --        Batch_Split_Count := 0;
   end Free_Manifold_Mesh_Data;

   --  ----------------------------------------------------------------------------

   function Get_Light_Index (Column, Row : GL.Types.Int; Light_Number : Integer)
                             return GL.Types.Int is
      use GL.Types;
      use Batch_Manager;
      use Tile_Indices_Package;
      Batch_Index   : constant Positive :=
                        Get_Batch_Index (Column, Row);
      Batch         : Batch_Meta;
      Light_Indices : Tile_Indices_List;
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
              Int'Image (Column) & "," & Int'Image (Row) &
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
      Manifold_Shader_Manager.Set_Ambient_Light ((0.0125, 0.0125, 0.0125));
      Manifold_Shader_Manager.Set_Diff_Map (0);
      Manifold_Shader_Manager.Set_Spec_Map (1);
      Manifold_Shader_Manager.Set_Cube_Texture (3);

      Water_Shader_Manager.Init (Water_Program);
      Water_Shader_Manager.Set_K_Diff ((0.03, 0.50, 0.20, 0.75));
      Water_Shader_Manager.Set_K_Spec ((0.5, 0.5, 0.5, 1.0));
      Water_Shader_Manager.Set_Ambient_Light ((0.0125, 0.0125, 0.0125));
      Water_Shader_Manager.Set_Cube_Texture (3);

      Game_Utils.Game_Log ("Manifold shaders initialized.");
      Free_Manifold_Mesh_Data;
      if not Mesh_Loader.Load_Mesh_Data_Only
        ("src/meshes/ramp_may_2014.apg", Ramp_Mesh_Points,
         Ramp_Mesh_Texcoords, Ramp_Mesh_Normals, Ramp_Mesh_Point_Count) then
         raise Manifold_Exception with
           "Manifold.Init error loading ramp mesh data from file "
           & "src/meshes/ramp_may_2014.apg";
      end if;

      if not Mesh_Loader.Load_Mesh_Data_Only ("src/meshes/ramp_smooth.apg",
                                              Points, Texcoords,
                                              Ramp_Mesh_Smooth_Normals,
                                              Points_Count) then
         raise Manifold_Exception with
           "Manifold.Init error loading ramp mesh data from file "
           & "src/meshes/ramp_smooth.apg";
      end if;
      Game_Utils.Game_Log ("Manifold.Init ramp_smooth.apg loaded.");

      if not Mesh_Loader.Load_Mesh_Data_Only
        ("src/meshes/water.apg", Water_Mesh_Points, Water_Mesh_Texcoords,
         Water_Mesh_Normals, Water_Mesh_Point_Count) then
         raise Manifold_Exception with
           "Manifold.Init error loading ramp mesh data from file "
           & "src/meshes/water.apg";
      end if;
      Game_Utils.Game_Log ("Manifold initialized.");

   end Init;

   --  ----------------------------------------------------------------------------

   function Is_Ramp (Row, Col : GL.Types.Int) return Boolean is
      use Tiles_Manager;
      aTile : constant Tile_Data := Get_Tile (Row, Col);
   begin
      return aTile.Tile_Type = '/';
   end Is_Ramp;

   --  ----------------------------------------------------------------------------

   function Is_Water (Row, Col : GL.Types.Int) return Boolean is
      use Tiles_Manager;
      aTile : constant Tile_Data := Get_Tile (Row, Col);
   begin
      return aTile.Tile_Type = '~';
   end Is_Water;

   --  ----------------------------------------------------------------------------

   procedure Reset_Manifold_Vars is
      use Batch_Manager;
   begin
      Batches_Across := 0;
      Batches_Down := 0;
      --        Batch_Split_Count := 0;
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

   procedure Update_Static_Lights_Uniforms  is
      use Batch_Manager;
      use Tile_Indices_Package;
      Lights    : constant Static_Light_Vector := Static_Lights;
      Index     : Positive := Lights.First_Index;
      aLight    : Static_Light_Data;
      Positions : Singles.Vector3_Array
        (Int (Static_Lights.First_Index) .. Int (Static_Lights.Last_Index));
      Diffuse   : Singles.Vector3_Array
        (Int (Static_Lights.First_Index) .. Int (Static_Lights.Last_Index));
      Specular  : Singles.Vector3_Array
        (Int (Static_Lights.First_Index) .. Int (Static_Lights.Last_Index));
      Ranges    : Single_Array
        (Int (Static_Lights.First_Index) .. Int (Static_Lights.Last_Index));
   begin
      GL.Objects.Programs.Use_Program (Manifold_Program);
      while Index <= Static_Lights.Last_Index loop
         aLight := Element (Static_Lights, Index);
         Positions (Int (Index)) := aLight.Position;
         Diffuse (Int (Index)) := aLight.Diffuse;
         Specular (Int (Index)) := aLight.Specular;
         Ranges (Int (Index)) := aLight.Light_Range;
         Index := Index + 1;
      end loop;
      Manifold_Shader_Manager.Set_Light_Positions (Positions);
      Water_Shader_Manager.Set_Light_Position (Positions);
      Manifold_Shader_Manager.Set_Lights_Diffuse (Diffuse);
      Water_Shader_Manager.Set_Light_Diffuse (Diffuse);
      Manifold_Shader_Manager.Set_Lights_Specular (Specular);
      Water_Shader_Manager.Set_Light_Specular (Specular);
      Manifold_Shader_Manager.Set_Light_Ranges (Ranges);
      Water_Shader_Manager.Set_Light_Range (Ranges);
   end Update_Static_Lights_Uniforms;

   --  ----------------------------------------------------------------------------

end Manifold;
