
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

package Tiles_Manager is

    type Tile_Data is record
        Height    : Integer := 0;
        Texture   : Integer := 0;
        Facing    : Character := 'N';  --  North
        Tile_Type : Character := ASCII.NUL;
    end record;

    package Tile_Data_Package is new Ada.Containers.Vectors
      (Positive, Tile_Data);
    type Tile_List is new Tile_Data_Package.Vector with null record;

    Tiles_Manager_Exception : Exception;

    procedure Load_Tiles (File : File_Type);
    function Number_Of_Tiles return Integer;
    procedure Reset_Vars;

end Tiles_Manager;
