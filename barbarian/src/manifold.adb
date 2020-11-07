
with Ada.Containers.Vectors;
with GL.Objects.Programs;

with Maths;

with Batch_Manager;
with Game_Utils;
with GL_Maths;
with Manifold_Shader_Manager;
with Mesh_Loader;
with Tiles_Manager;
with Water_Shader_Manager;

package body Manifold is
   use GL.Types;

   Manifold_Program         : GL.Objects.Programs.Program;
   Water_Program            : GL.Objects.Programs.Program;
   Ramp_Mesh_Points         : GL_Maths.Vector3_List;
   Ramp_Mesh_Normals        : GL_Maths.Vector3_List;
   Ramp_Mesh_Smooth_Normals : GL_Maths.Vector3_List;
   Ramp_Mesh_Texcoords      : GL_Maths.Vector2_List;
   Ramp_Mesh_Point_Count    : Integer := 0;
   Water_Mesh_Points        : GL_Maths.Vector3_List;
   Water_Mesh_Normals       : GL_Maths.Vector3_List;
   Water_Mesh_Texcoords     : GL_Maths.Vector2_List;
   Water_Mesh_Point_Count   : Integer := 0;

   --  ----------------------------------------------------------------------------

   procedure Free_Manifold_Mesh_Data is
   begin
      null;
   end Free_Manifold_Mesh_Data;

   --  ----------------------------------------------------------------------------

   function Get_Light_Index (Column, Row : GL.Types.Int; Light_Number : Integer)
                             return Integer is
      use GL.Types;
      use Batch_Manager;
      use Light_Indices_Package;
      Batch_Index   : constant Positive :=
                        Get_Batch_Index (Column, Row);
      Batch         : Batch_Manager.Batch_Meta;
      Light_Indices : Batch_Manager.Light_Indices_List;
      Light_Cursor  : Cursor;
--        Light_Index   : Positive;
--        theLight      : Batch_Manager.Static_Light_Data;
      Found         : Boolean := False;
      Result        : Integer := -1;
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
         Result := Element (Light_Cursor);
      end if;
      return Result;
   end Get_Light_Index;

   --  ------------------------------------------------------------------------

   procedure Init is
      Points       : GL_Maths.Vector3_List;
      Texcoords    : GL_Maths.Vector2_List;
      Points_Count : Integer := 0;

   begin
      Game_Utils.Game_Log ("Initializing manifold.");
      Manifold_Shader_Manager.Init (Manifold_Program);
      Game_Utils.Game_Log ("Manifold_Program initialized.");
      Manifold_Shader_Manager.Set_Ambient_Light ((0.0125, 0.0125, 0.0125));
      Manifold_Shader_Manager.Set_Diff_Map (0);
      Manifold_Shader_Manager.Set_Spec_Map (1);
      Manifold_Shader_Manager.Set_Cube_Texture (3);

      Water_Shader_Manager.Init (Water_Program);
      Game_Utils.Game_Log ("Water_Program initialized.");
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
      Manifold_Shader_Manager.Set_Ambient_Light (Level);
      Water_Shader_Manager.Set_Ambient_Light (Level);
   end Set_Manifold_Ambient_Light;

   --  ----------------------------------------------------------------------------

   procedure Update_Manifold_Static_Lights_Uniforms  is
      use Batch_Manager;
      use Light_Indices_Package;
      Index     : Positive := Static_Lights.First_Index;
      aLight    : Static_Light_Data;
      Positions : Singles.Vector3_Array
        (Int (Static_Lights.First_Index) .. Int (Static_Lights.Last_Index));
      Diffuse   : Singles.Vector3_Array
        (Int (Static_Lights.First_Index) .. Int (Static_Lights.Last_Index));
      Specular : Singles.Vector3_Array
        (Int (Static_Lights.First_Index) .. Int (Static_Lights.Last_Index));
      Ranges : Single_Array
        (Int (Static_Lights.First_Index) .. Int (Static_Lights.Last_Index));
   begin
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
   end Update_Manifold_Static_Lights_Uniforms;

   --  ----------------------------------------------------------------------------

end Manifold;
