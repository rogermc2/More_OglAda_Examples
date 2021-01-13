
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures;
with GL.Types; use GL.Types;

package Tiles_Manager is

   type Tile_Data is record
      Height        : Natural := 0;
      Texture_Index : Natural := 0;
      Facing        : Character := 'N';  --  North
      Tile_Type     : Character := ASCII.NUL;
   end record;

   package Tile_Column_Package is new Ada.Containers.Vectors
     (Positive, Tile_Data);
   subtype Tile_Column_List is Tile_Column_Package.Vector;
   subtype Tile_Column_Cursor is Tile_Column_Package.Cursor;
   use Tile_Column_Package;

   package Tile_Row_Package is new Ada.Containers.Vectors
     (Positive, Tile_Column_List);
   subtype Tile_2D_List is Tile_Row_Package.Vector;
   subtype Tile_Row_Cursor is Tile_Row_Package.Cursor;

   use GL.Types.Ints;
   package Tile_Indices_Package is new Ada.Containers.Doubly_Linked_Lists
     (GL.Types.Ints.Vector2);
   subtype Tile_Indices_List is Tile_Indices_Package.List;
   subtype Tile_Indices_Cursor is Tile_Indices_Package.Cursor;

   Tiles_Manager_Exception : Exception;
   Out_Of_Bounds_Height    : constant Single := 1024.0;

   function Get_Facing  (Row, Col : Positive) return Character;
   function Get_Tile (Row_Curs : Tile_Row_Package.Cursor;
                      Col_Curs : Tile_Column_Package.Cursor) return Tile_Data;
   function Get_Tile (Row, Col : Positive) return Tile_Data;
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
