
with Manifold;
package body Sprite_World_Map is

    Max_Sprites_In_Tile : constant Integer := 64;

    type Sprites_Int_Array is array
      (1 .. Manifold.Max_Tile_Cols, 1 .. Manifold.Max_Tile_Cols,
       1 .. Max_Sprites_In_Tile) of Integer;
    type Sprites_Count_Array is array
      (1 .. Manifold.Max_Tile_Cols, 1 .. Manifold.Max_Tile_Cols) of Integer;
    type Sprites_Float_Array is array
      (1 .. Manifold.Max_Tile_Cols, 1 .. Manifold.Max_Tile_Cols,
       1 .. Max_Sprites_In_Tile) of Float;

    Index_Of_Sprites          : Sprites_Int_Array :=
                                  (others => (others => (others => 0)));
    Height_Of_Sprites         : Sprites_Float_Array :=
                                  (others => (others => (others => (0.0))));
    Count_Of_Sprites_In_Tiles : Sprites_Count_Array := (others => (others => 0));

    --  ------------------------------------------------------------------------

    procedure Add_New_Sprite_To_World_Map (U, V : Integer; Y : Float;
                                           Sprite_ID : Integer) is
        Count : constant Integer := Count_Of_Sprites_In_Tiles (U, V) + 1;
    begin
        Index_Of_Sprites (U, V, Count) := Sprite_ID;
        Height_Of_Sprites (U, V, Count) := Y;
        Count_Of_Sprites_In_Tiles (U, V) := Count;
    end Add_New_Sprite_To_World_Map;

    --  ------------------------------------------------------------------------

    procedure Move_Sprite_To_World_Map (From_U, From_V, To_U, To_V : Integer;
                                        Y : Float; Sprite_ID : Integer) is
    begin
        if From_U > 0 and From_V > 0 then
            Remove_Sprite_From_World_Map (From_U, From_V, Sprite_ID);
        end if;
        Add_New_Sprite_To_World_Map (To_U, To_V, Y, Sprite_ID);
    end Move_Sprite_To_World_Map;

    --  ------------------------------------------------------------------------

    procedure Remove_Sprite_From_World_Map (U, V : Integer;
                                            Sprite_ID : Integer) is
        Count : constant Integer := Count_Of_Sprites_In_Tiles (U, V);
        Idx   : Integer := 0;
    begin
        for index in 1 .. Count loop
            if Index_Of_Sprites (U, V, index) = Sprite_ID then
                Index_Of_Sprites (U, V, index) := 0;
                Height_Of_Sprites (U, V, index) := 0.0;
                Idx := index;
            end if;
        end loop;
        if Idx <= 0 then
            raise Sprite_World_Map_Exception with
              "Sprite_World_Map.Remove_Sprite_From_World_Map, Sprite ID " &
              Integer'Image (Sprite_ID) & " was not found.";
        end if;

        for index in reverse Idx .. Count - 1 loop
            Index_Of_Sprites (U, V, index) := Index_Of_Sprites (U, V, index + 1);
            Height_Of_Sprites (U, V, index) := Height_Of_Sprites (U, V, index + 1);
        end loop;
        Count_Of_Sprites_In_Tiles (U, V) := Count_Of_Sprites_In_Tiles (U, V) - 1;

    end Remove_Sprite_From_World_Map;

    --  ------------------------------------------------------------------------

end Sprite_World_Map;
