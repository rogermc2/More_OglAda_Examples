
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types;

with GL_Maths;

package Manifold is
    use GL.Types;

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
    function Load_Tiles (File : File_Type) return Boolean;
    function Number_Of_Tiles return Integer;
    procedure Reset_Manifold_Vars;

private
    type Batch_Meta is record
        Tiles                : Tile_Nodes_List;
        AABB_Mins            : Singles.Vector3;
        AABB_Mixs            : Singles.Vector3;
        Points               : GL_Maths.Singles_List;
        Normals              : GL_Maths.Singles_List;
        Tex_Coords           : GL_Maths.Singles_List;
        VAO                  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        Points_VBO           : GL.Objects.Buffers.Buffer;
        Normals_VBO          : GL.Objects.Buffers.Buffer;
        Tex_Coords_VBO       : GL.Objects.Buffers.Buffer;
        Static_Light_Indices : Tile_Nodes_List;
    end record;

end Manifold;
