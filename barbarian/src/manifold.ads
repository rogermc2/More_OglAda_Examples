
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO; use Ada.Streams;

with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types;

package Manifold is
    use GL.Types;

    package Singles_Data_Package is new Ada.Containers.Doubly_Linked_Lists
      (Single);
    type Singles_Data_List is new Singles_Data_Package.List with null record;

    subtype Tile_Index is Integer;
    package Tile_Nodes_Package is new Ada.Containers.Vectors
      (Positive, Tile_Index);
    type Tile_Nodes_List is new Tile_Nodes_Package.Vector with null record;

    type Batch_Meta is private;

    Manifold_Exception : Exception;
    Manifold_Parsing_Exception : Exception;

    function Batch_Split_Size return Integer;
    function Get_Batch_Index (Column, Row : Int) return Int;
    function Get_Light_Index (Column, Row : Int; Light_Number : Integer)
                              return Int;
    function Init_Manifold return Boolean;
    function Is_Tile_Valid (Col, Row : Int) return Boolean;
    procedure Load_Tiles (Input_Stream : Stream_IO.Stream_Access);
    function Number_Of_Tiles return Integer;
    procedure Reset_Manifold_Vars;

private
    type Batch_Meta is record
        Tiles                : Tile_Nodes_List;
        AABB_Mins            : Singles.Vector3;
        AABB_Mixs            : Singles.Vector3;
        Points               : Singles_Data_List;
        Normals              : Singles_Data_List;
        Tex_Coords           : Singles_Data_List;
        VAO                  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        Points_VBO           : GL.Objects.Buffers.Buffer;
        Normals_VBO          : GL.Objects.Buffers.Buffer;
        Tex_Coords_VBO       : GL.Objects.Buffers.Buffer;
        Static_Light_Indices : Tile_Nodes_List;
    end record;

end Manifold;
