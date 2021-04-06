
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;

with Maths;

with Tiles_Manager;
with GL_Maths;

package Batch_Manager is

   type Batch_Meta is record
      Tiles                : Tiles_Manager.Tile_Indices_List;
      AABB_Mins            : Singles.Vector3 := (0.0, 0.0, 0.0);
      AABB_Maxs            : Singles.Vector3 := (0.0, 0.0, 0.0);
      Points               : GL_Maths.Vec3_List;
      Ramp_Points          : GL_Maths.Vec3_List;
      Water_Points         : GL_Maths.Vec3_List;
      Normals              : GL_Maths.Vec3_List;
      Ramp_Normals         : GL_Maths.Vec3_List;
      Ramp_Smooth_Normals  : GL_Maths.Vec3_List;
      Tex_Coords           : GL_Maths.Vec2_List;
      Ramp_Tex_Coords      : GL_Maths.Vec2_List;
      Points_VAO           : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Ramp_VAO             : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Water_VAO            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Points_VBO           : GL.Objects.Buffers.Buffer;
      Normals_VBO          : GL.Objects.Buffers.Buffer;
      Ramp_VBO             : GL.Objects.Buffers.Buffer;
      Ramp_Normals_VBO     : GL.Objects.Buffers.Buffer;
      Ramp_Smooth_Normals_VBO : GL.Objects.Buffers.Buffer;
      Ramp_Texcoords_VBO   : GL.Objects.Buffers.Buffer;
      Water_VBO            : GL.Objects.Buffers.Buffer;
      Tex_Coords_VBO       : GL.Objects.Buffers.Buffer;
      Static_Light_Indices : GL_Maths.Indices_List;
   end record;

   package Batches_Package is new Ada.Containers.Vectors (Positive, Batch_Meta);
   type Batches_List is new Batches_Package.Vector with null record;

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

   Max_Cols                 : Int := 0;  --  Set by map file
   Max_Rows                 : Int := 0;  --  Set by map file
   Batches_Across           : Integer := 0;
   Batches_Down             : Integer := 0;

   Batch_Manager_Exception : Exception;

   procedure Add_Static_Light (Row, Col : Int; Tile_Height_Offset : Integer;
                               Offset_Pos, Diffuse, Specular  : Singles.Vector3;
                               Light_Range                    : Single);
   procedure Add_Batch (Data : Batch_Meta);
   procedure Clear;
   function Get_Batch_Index (Column, Row : Positive) return Integer;
   procedure Init;
   function Batches return Batches_List;
   procedure Regenerate_Batch (Tiles       : Tiles_Manager.Tile_Row_List;
                               Batch_Index : Positive);
   function Static_Lights return Static_Light_Vector;
   procedure Update_Batch (Index : Positive; Data : Batch_Meta);

end Batch_Manager;
