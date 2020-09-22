
package Manifold is

    Manifold_Exception : Exception;
    Manifold_Parsing_Exception : Exception;

    function Get_Light_Index (Column, Row, Light_Number : Integer)
                              return Integer;
    procedure Init;
    function Is_Tile_Valid (Col, Row : Integer) return Boolean;
    procedure Reset_Manifold_Vars;

end Manifold;
