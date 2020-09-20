
with Ada.Containers.Vectors;

with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;

with GL_Maths;

package Batch_Manager is

    subtype Tile_Index is Integer;
    package Tile_Nodes_Package is new Ada.Containers.Vectors
      (Positive, Tile_Index);
    type Tile_Nodes_List is new Tile_Nodes_Package.Vector with null record;

    type Batch_Meta is record
        Tiles                : Tile_Nodes_List;
        Tile_Count           : Integer := 0;
        AABB_Mins            : Singles.Vector3;
        AABB_Mixs            : Singles.Vector3;
        Points               : GL_Maths.Singles_List;
        Normals              : GL_Maths.Singles_List;
        Tex_Coords           : GL_Maths.Singles_List;
        VAO                  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        Points_VBO           : GL.Objects.Buffers.Buffer;
        Normals_VBO          : GL.Objects.Buffers.Buffer;
        Tex_Coords_VBO       : GL.Objects.Buffers.Buffer;
        Static_Light_Indices : GL_Maths.Integers_List;
    end record;

    package Batches_Package is new Ada.Containers.Vectors
      (Positive, Batch_Meta);
    type Batches_List is new Batches_Package.Vector with null record;

    function Regenerate_Batch return Boolean;

end Batch_Manager;
