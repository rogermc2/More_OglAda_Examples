
with GL.Types; use GL.Types;

--  The idea is to have a tile-by-tile 2D map of the world so that
--  when rendering from the top-left tile to the bottom-right tile
--  we can just look up the matching tiles here.
--  Each tile here will have a list of sprites in bottom-to-top order
--  so that they can be rendered in back-to-front order for transparency.
--  This also means that when first placing or moving sprites to a new tile
--  the sprites need to be removed and re-inserted in the correct place
--  in the new tile's list

package Sprite_World_Map is

    Sprite_World_Map_Exception : Exception;

    procedure Add_New_Sprite_To_World_Map (U, V : Int; Y : Single;
                                           Sprite_ID : Positive);
    procedure Cache_Sprites_Around (U, V, Tile_Range : Int);
    procedure Free_Sprite_World_Map;
    procedure Init;
    procedure Move_Sprite_In_World_Map (From_U, From_V, To_U, To_V : Int;
                                        Y : Single; Sprite_ID : Positive);
    procedure Remove_Sprite_From_World_Map (U, V : Int; Sprite_ID : Positive);
end Sprite_World_Map;
