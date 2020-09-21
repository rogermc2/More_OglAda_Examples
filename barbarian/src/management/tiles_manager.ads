
with Ada.Containers.Vectors;

with GL.Objects.Textures;

package Tiles_Manager is

    type Tile_Data is record
        Height    : Integer;
        Texture   : GL.Objects.Textures.Texture;
        Facing    : Character := ASCII.NUL;
        Tile_Type : Character := ASCII.NUL;
    end record;

    package Tile_Data_Package is new Ada.Containers.Vectors
      (Positive, Tile_Data);
    type Tile_List is new Tile_Data_Package.Vector with null record;

end Tiles_Manager;
