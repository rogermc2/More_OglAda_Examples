
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Streams.Stream_IO; use Ada.Streams;

with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types;

package Manifold is

    subtype Tile_Index is Integer;

    package Tile_Nodes_Package is new Ada.Containers.Doubly_Linked_Lists
      (Tile_Index);
    type Tile_Nodes_List is new Tile_Nodes_Package.List with null record;

    type Batch_Meta is private;

    Max_Tile_Cols : constant GL.Types.Int := 64;
    Manifold_Parsing_Exception : Exception;

    function Batch_Split_Size return Integer;
    function Init_Manifold return Boolean;
    procedure Load_Tiles (Input_Stream : Stream_IO.Stream_Access);
    function Number_Of_Tiles return Integer;
    procedure Reset_Manifold_Vars;

private
    type Batch_Meta is record
      Tiles           : Tile_Nodes_List;
      Tile_Count      : Integer := 0;
      AABB_Mins       : GL.Types.Singles.Vector3;
      AABB_Mixs       : GL.Types.Singles.Vector3;
      VAO             : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Points_VBO      : GL.Objects.Buffers.Buffer;
      Normals_VBO     : GL.Objects.Buffers.Buffer;
      Tex_Coords_VBO  : GL.Objects.Buffers.Buffer;
    end record;

end Manifold;
