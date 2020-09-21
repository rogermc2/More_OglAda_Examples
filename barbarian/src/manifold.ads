
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with Maths;

package Manifold is
    use GL.Types;

    Manifold_Exception : Exception;
    Manifold_Parsing_Exception : Exception;

    function Batch_Split_Size return Integer;
    function Get_Light_Index (Column, Row, Light_Number : Integer)
                              return Integer;
    procedure Init;
    function Is_Tile_Valid (Col, Row : Integer) return Boolean;
    function Load_Tiles (File : File_Type) return Boolean;
    function Number_Of_Tiles return Integer;
    procedure Reset_Manifold_Vars;

private

    type Static_Light_Data is record
        Row      : Integer := 0;
        Column   : Integer := 0;
        Position : GL.Types.Singles.Vector3 := Maths.Vec3_0;
        Diffuse  : GL.Types.Singles.Vector3 := Maths.Vec3_0;
        Specular : GL.Types.Singles.Vector3 := Maths.Vec3_0;
        Distance : GL.Types.Single := 0.0;
    end record;
    package Static_Light_Package is new Ada.Containers.Vectors
      (Positive, Static_Light_Data);
    type Static_Light_List is new Static_Light_Package.Vector with null record;

end Manifold;
