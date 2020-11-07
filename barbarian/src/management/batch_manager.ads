
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;

with Maths;

with Tiles_Manager;
with GL_Maths;

package Batch_Manager is

   Max_Cols               : Int := 0;  --  Set by map file
   Max_Rows               : Int := 0;  --  Set by map file
   Batches_Across         : Integer := 0;
   Batches_Down           : Integer := 0;
   Batch_Split_Count      : Integer := 0;
   Ramp_Mesh_Point_Count  : Integer := 0;
   Water_Mesh_Point_Count : Integer := 0;
   Total_Points           : Integer := 0;

   type Static_Light_Data is record
      Position    : Singles.Vector3 := Maths.Vec3_0;
      Diffuse     : Singles.Vector3 := Maths.Vec3_0;
      Specular    : Singles.Vector3 := Maths.Vec3_0;
      Light_Range : Single := 0.0;
      Row         : Int;
      Column      : Int;
   end record;

   package Static_Light_Package is new
     Ada.Containers.Vectors (Positive, Static_Light_Data);
   type Static_Light_Vector is new Static_Light_Package.Vector with null record;

   package Light_Indices_Package is new Ada.Containers.Doubly_Linked_Lists
     (Positive);
   type Light_Indices_List is new Light_Indices_Package.List with null record;

   package Tile_Indices_Package is new Ada.Containers.Vectors
     (Positive, Positive);
   type Tile_Indices is new Tile_Indices_Package.Vector with null record;

   type Batch_Meta is record
      Tiles                : Tile_Indices;
--        Tile_Count           : Integer := 0;
      AABB_Mins            : Singles.Vector3;
      AABB_Mixs            : Singles.Vector3;
      Points               : GL_Maths.Vector3_List;
--        Point_Count          : Integer := 0;
      Ramp_Points          : GL_Maths.Vector3_List;
--        Ramp_Point_Count     : Integer := 0;
      Water_Points         : GL_Maths.Vector3_List;
--        Water_Point_Count    : Integer := 0;
      Normals              : GL_Maths.Vector3_List;
--        Normal_Count         : Integer := 0;
      Ramp_Normals         : GL_Maths.Vector3_List;
--        Ramp_Normal_Count    : Integer := 0;
      Ramp_Smooth_Normals  : GL_Maths.Singles_List;
      Tex_Coords           : GL_Maths.Vector2_List;
--        Tex_Coord_Count      : Integer := 0;
      Ramp_Tex_Coords      : GL_Maths.Vector2_List;
--        Ramp_Tex_Coord_Count : Integer := 0;
      VAO                  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Ramp_VAO             : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Water_VAO            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Points_VBO           : GL.Objects.Buffers.Buffer;
      Normals_VBO          : GL.Objects.Buffers.Buffer;
      Tex_Coords_VBO       : GL.Objects.Buffers.Buffer;
      Static_Light_Indices : Light_Indices_List;
--        Static_Light_Count   : Integer := 0;
   end record;

   package Batches_Package is new Ada.Containers.Vectors
     (Positive, Batch_Meta);
   type Batches_List is new Batches_Package.Vector with null record;

   Batches                 : Batches_List;
   Static_Lights           : Static_Light_Vector;

   Batch_Manager_Exception : Exception;

   procedure Add_Static_Light (Col, Row                  : Int; Tile_Height_Offset : Integer;
                               Offset, Diffuse, Specular : Singles.Vector3;
                               Light_Range               : Single);
   function Batch_Split_Size return Integer;
   function Get_Batch_Index (Column, Row : Int) return Integer;
   procedure Regenerate_Batch (Tiles       : Tiles_Manager.Tile_List;
                               Batch_Index : Positive);

end Batch_Manager;
