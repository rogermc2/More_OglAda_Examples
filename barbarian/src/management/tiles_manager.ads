
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures;
with GL.Types; use GL.Types;

package Tiles_Manager is

   Max_Tile_Cols    : constant Positive := 64;

   Max_Map_Cols   : Natural := 0;  --  Set by map file
   Max_Map_Rows   : Natural := 0;  --  Set by map file
   Batches_Across : Natural := 0;  --  Set by Load_Tiles
   Batches_Down   : Natural := 0;  --  Set by Load_Tiles

   Total_Tiles    : Natural := 0;
   subtype Tiles_Index is Natural;
   subtype Tiles_RC_Index is Natural;

   type Tile_Data is record
      Height        : Natural := 0;
      Texture_Index : Natural := 0;
      Facing        : Character := 'N';  --  North
      Tile_Type     : Character := ASCII.NUL;
   end record;

   package Tile_Column_Package is new Ada.Containers.Vectors
     (Natural, Tile_Data);
   subtype Tile_Column_List is Tile_Column_Package.Vector;
   subtype Tile_Column_Cursor is Tile_Column_Package.Cursor;
   use Tile_Column_Package;

   package Tile_Row_Package is new Ada.Containers.Vectors
     (Natural, Tile_Column_List);
   subtype Tile_Row_List is Tile_Row_Package.Vector;
   subtype Tile_Row_Cursor is Tile_Row_Package.Cursor;

   package Tile_Indices_Package is new Ada.Containers.Vectors
      (Natural, Natural);
   subtype Tile_Indices_List is Tile_Indices_Package.Vector;
--     subtype Tile_Indices_Cursor is Tile_Indices_Package.Cursor;

   Tiles_Manager_Exception : Exception;
   Out_Of_Bounds_Height    : constant Single := 1024.0;

   function Get_Facing (Index : Tiles_RC_Index) return Character;
   function Get_Facing (Map : Ints.Vector2) return Character;
   function Get_Tile (Pos : Ints.Vector2) return Tile_Data;
   function Get_Tile (Tile_Index : Natural) return Tile_Data;
   function Get_Tile_Height
     (X, Z : Single; Consider_Water, Respect_Ramps : Boolean) return Single;
   function Get_Tile_Type (Index : Tiles_RC_Index) return Character;
   function Get_Tiles_Across return Natural;
   function Is_Tile_Valid (Map : Ints.Vector2) return Boolean;
   procedure Load_Tiles (File : File_Type;
                         Tile_Tex, Tile_Spec_Tex, Ramp_Diff_Tex,
                         Ramp_Spec_Tex : in out GL.Objects.Textures.Texture);
   function Number_Of_Tiles return Integer;

end Tiles_Manager;
