
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures;
with GL.Types; use GL.Types;

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
   Out_Of_Bounds_Height    : constant Single := 1024.0;

   function Get_Facing (Col, Row : Int) return Character;
   function Get_Tile (Col, Row : Int) return Tile_Data;
   function Get_Tile_Height
     (X, Z : Single; Consider_Water, Respect_Ramps : Boolean) return Single;
   function Get_Tiles_Across return Int;
   function Is_Tile_Valid (Row, Col : GL.Types.Int) return Boolean;
   procedure Load_Tiles (File : File_Type;
                         Tile_Tex, Tile_Spec_Tex, Ramp_Diff_Tex,
                         Ramp_Spec_Tex : in out GL.Objects.Textures.Texture);
   function Number_Of_Tiles return Integer;
   procedure Reset_Vars;

end Tiles_Manager;
