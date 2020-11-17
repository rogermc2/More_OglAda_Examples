
with Maths;

with Batch_Manager;
with Sprite_Renderer;
with Tiles_Manager;
with Transparency;

package body Sprite_World_Map is

   Max_Sprites_In_Tile : constant Integer := 64;

   type Sprites_Int_Array is array
     (1 .. Batch_Manager.Max_Rows, 1 .. Batch_Manager.Max_Cols,
      1 .. Max_Sprites_In_Tile) of Integer;
   type Sprites_Count_Array is array
     (1 .. Int (Batch_Manager.Max_Rows), 1 .. Int (Batch_Manager.Max_Cols)) of Integer;
   type Sprites_Single_Array is array
     (1 .. Batch_Manager.Max_Rows, 1 .. Batch_Manager.Max_Cols,
      1 .. Max_Sprites_In_Tile) of Single;

   type Sprite_Tiles_Data is record
      Index_Of_Sprites          : Sprites_Int_Array :=
                                    (others => (others => (others => 0)));
      Height_Of_Sprites         : Sprites_Single_Array :=
                                    (others => (others => (others => (0.0))));
      Count_Of_Sprites_In_Tiles : Sprites_Count_Array := (others => (others => 0));
   end record;

   Sprite_Tiles : Sprite_Tiles_Data;

   --  ------------------------------------------------------------------------

   procedure Add_New_Sprite_To_World_Map (U, V      : Int; Y : Single;
                                          Sprite_ID : Integer) is
      Count : constant Integer := Sprite_Tiles.Count_Of_Sprites_In_Tiles (U, V) + 1;
   begin
      Sprite_Tiles.Index_Of_Sprites (U, V, Count) := Sprite_ID;
      Sprite_Tiles.Height_Of_Sprites (U, V, Count) := Y;
      Sprite_Tiles.Count_Of_Sprites_In_Tiles (U, V) := Count;
   end Add_New_Sprite_To_World_Map;

   --  ------------------------------------------------------------------------

   procedure Cache_Sprites_Around (U, V, Tile_Range : Int) is
      use Maths;
      use Tiles_Manager;
      use Transparency;
      U_First   : constant Int := Max_Int (0, U - Tile_Range) + 1;
      V_First   : constant Int := Max_Int (0, V - Tile_Range) + 1;
      U_Last    : constant Int := Min_Int
        (U + Tile_Range, Get_Tiles_Across - 1) + 1;
      Sprite_ID : Positive;
      V_Last    : constant Int :=
                    Min_Int (V + Tile_Range, Get_Tiles_Across - 1) + 1;
      Pos       : Singles.Vector3;
      Count     : Integer;
   begin
      for index in V_First .. V_Last loop
         for index in U_First .. U_Last loop
            Count := Sprite_Tiles.Count_Of_Sprites_In_Tiles (U, V);
            for index in 1 .. Count loop
               Sprite_ID := Sprite_Tiles.Index_Of_Sprites (U, V, index);
               Pos := Sprite_Renderer.Get_Sprite_World_Pos (Sprite_ID);
               Add_Transparency_Item (Tr_Sprite, Sprite_ID, Pos, 1.0);
            end loop;
         end loop;
      end loop;
   end Cache_Sprites_Around;

   --  ------------------------------------------------------------------------

   procedure Free_Sprite_World_Map is
   begin
      for row in 1 .. Batch_Manager.Max_Rows loop
         for col in 1 .. Batch_Manager.Max_Cols loop
            Sprite_Tiles.Count_Of_Sprites_In_Tiles (row, col) := 0;
            for sprite in 1 .. Max_Sprites_In_Tile loop
               Sprite_Tiles.Index_Of_Sprites (row, col, sprite) := 0;
               Sprite_Tiles.Height_Of_Sprites (row, col, sprite) := 0.0;
            end loop;
         end loop;
      end loop;
   end Free_Sprite_World_Map;

   --  ------------------------------------------------------------------------

   procedure Move_Sprite_In_World_Map (From_U, From_V, To_U, To_V : Int;
                                       Y                          : Single; Sprite_ID      : Integer) is
   begin
      if From_U > 0 and From_V > 0 then
         Remove_Sprite_From_World_Map (From_U, From_V, Sprite_ID);
      end if;
      Add_New_Sprite_To_World_Map (To_U, To_V, Y, Sprite_ID);
   end Move_Sprite_In_World_Map;

   --  ------------------------------------------------------------------------

   procedure Remove_Sprite_From_World_Map (U, V      : Int;
                                           Sprite_ID : Integer) is
      Count : constant Integer := Sprite_Tiles.Count_Of_Sprites_In_Tiles (U, V);
      Idx   : Integer := 0;
   begin
      for index in 1 .. Count loop
         if Sprite_Tiles.Index_Of_Sprites (U, V, index) = Sprite_ID then
            Sprite_Tiles.Index_Of_Sprites (U, V, index) := 0;
            Sprite_Tiles.Height_Of_Sprites (U, V, index) := 0.0;
            Idx := index;
         end if;
      end loop;

      if Idx <= 0 then
         raise Sprite_World_Map_Exception with
           "Sprite_World_Map.Remove_Sprite_From_World_Map, Sprite ID " &
           Integer'Image (Sprite_ID) & " was not found.";
      end if;

      for index in reverse Idx .. Count - 1 loop
         Sprite_Tiles.Index_Of_Sprites (U, V, index) :=
           Sprite_Tiles.Index_Of_Sprites (U, V, index + 1);
         Sprite_Tiles.Height_Of_Sprites (U, V, index) :=
           Sprite_Tiles.Height_Of_Sprites (U, V, index + 1);
      end loop;
      Sprite_Tiles.Count_Of_Sprites_In_Tiles (U, V) :=
        Sprite_Tiles.Count_Of_Sprites_In_Tiles (U, V) - 1;

   end Remove_Sprite_From_World_Map;

   --  ------------------------------------------------------------------------

end Sprite_World_Map;
