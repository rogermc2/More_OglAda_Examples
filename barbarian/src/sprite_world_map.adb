
with Maths;

with Batch_Manager;
with Game_Utils;
with Sprite_Renderer;
with Tiles_Manager;
with Transparency;

package body Sprite_World_Map is

   Max_Sprites_In_Tile : constant Integer := 64;
   subtype Sprite_Index is Positive range 1 .. Max_Sprites_In_Tile;

   type Sprites_Int_Array is array
     (Sprite_Index range <>, Sprite_Index range <>, Sprite_Index range <>) of Int;
   type Sprites_Count_Array is array (Sprite_Index range <>, Sprite_Index range <>) of Int;
   type Sprites_Single_Array is array
     (Sprite_Index range <>, Sprite_Index range <>, Sprite_Index range <>) of Single;
   type Sprite_Tiles_Data (Rows, Cols, Val : Sprite_Index := 1) is record
      Index_Of_Sprites          : Sprites_Int_Array
          (1 .. Rows, 1 .. Cols, 1 .. Val) :=
                                    (others => (others => (others => 0)));
      Height_Of_Sprites         : Sprites_Single_Array
      (1 .. Rows, 1 .. Cols, 1 .. Val) :=
                                    (others => (others => (others => (0.0))));
      Count_Of_Sprites_In_Tiles : Sprites_Count_Array (1 .. Rows, 1 .. Cols) :=
                                      (others => (others => 0));
   end record;


   Sprite_Tiles : Sprite_Tiles_Data;

   --  ------------------------------------------------------------------------

   procedure Add_New_Sprite_To_World_Map (U, V      : Int; Y : Single;
                                          Sprite_ID : Int) is
      S_U : constant Sprite_Index := Sprite_Index (U);
      S_V : constant Sprite_Index := Sprite_Index (V);
      Count : constant Sprite_Index :=
                  Sprite_Index (Sprite_Tiles.Count_Of_Sprites_In_Tiles (S_U, S_V)) + 1;
   begin
      Sprite_Tiles.Index_Of_Sprites (S_U, S_V, Count) := Int (Sprite_ID);
      Sprite_Tiles.Height_Of_Sprites (S_U, S_V, Count) := Y;
      Sprite_Tiles.Count_Of_Sprites_In_Tiles (S_U, S_V) := Int (Count);
   end Add_New_Sprite_To_World_Map;

   --  ------------------------------------------------------------------------

   procedure Cache_Sprites_Around (U, V, Tile_Range : Int) is
      use Maths;
      use Tiles_Manager;
      use Transparency;
      S_U       : constant Sprite_Index := Sprite_Index (U);
      S_V       : constant Sprite_Index := Sprite_Index (V);
      U_First   : constant Sprite_Index := Sprite_Index (Max_Int (0, U - Tile_Range) + 1);
      V_First   : constant Sprite_Index := Sprite_Index (Max_Int (0, V - Tile_Range) + 1);
      U_Last    : constant Sprite_Index := Sprite_Index (Min_Int
        (U + Tile_Range, Get_Tiles_Across - 1) + 1);
      Sprite_ID : Int;
      V_Last    : constant Sprite_Index :=
                    Sprite_Index (Min_Int (V + Tile_Range, Get_Tiles_Across - 1) + 1) ;
      Pos       : Singles.Vector3;
      Count     : Int;
   begin
      for index in V_First .. V_Last loop
         for index in U_First .. U_Last loop
            Count := Sprite_Tiles.Count_Of_Sprites_In_Tiles (S_U, S_V);
            for index in 1 .. Count loop
               Sprite_ID := Sprite_Tiles.Index_Of_Sprites (S_U, S_V, Sprite_Index (index));
               Pos := Sprite_Renderer.Get_Sprite_World_Pos (Sprite_ID);
               Add_Transparency_Item (Tr_Sprite, Sprite_ID, Pos, 1.0);
            end loop;
         end loop;
      end loop;

   end Cache_Sprites_Around;

   --  ------------------------------------------------------------------------

   procedure Free_Sprite_World_Map is
   begin
      for row in 1 .. Sprite_Index (Batch_Manager.Max_Rows) loop
         for col in 1 .. Sprite_Index (Batch_Manager.Max_Cols) loop
            Sprite_Tiles.Count_Of_Sprites_In_Tiles (row, col) := 0;
            for sprite in Sprite_Index range 1 .. Sprite_Index (Max_Sprites_In_Tile) loop
               Sprite_Tiles.Index_Of_Sprites (row, col, sprite) := 0;
               Sprite_Tiles.Height_Of_Sprites (row, col, sprite) := 0.0;
            end loop;
         end loop;
      end loop;
   end Free_Sprite_World_Map;

   --  ------------------------------------------------------------------------

   procedure Init is
        New_Sprite_Tiles : Sprite_Tiles_Data
        (Sprite_Index (Batch_Manager.Max_Rows),
        Sprite_Index (Batch_Manager.Max_Cols), Sprite_Tiles.Val);
   begin
      if Batch_Manager.Max_Rows <= 0 or Batch_Manager.Max_Cols <= 0 or
      Sprite_Tiles.Val <= 0 then
            raise Sprite_World_Map_Exception with
            "Sprite_World_Map.Init; invalid Batch_Manager data.";
      end if;

      Sprite_Tiles := New_Sprite_Tiles;
   end Init;

   --  ------------------------------------------------------------------------

   procedure Move_Sprite_In_World_Map (From_U, From_V,
                                       To_U, To_V : Int;  Y : Single;
                                       Sprite_ID   : Int) is
   begin
      if From_U > 0 and From_V > 0 then
         Remove_Sprite_From_World_Map (From_U, From_V, Sprite_ID);
      end if;
      Add_New_Sprite_To_World_Map (To_U, To_V, Y, Sprite_ID);
   end Move_Sprite_In_World_Map;

   --  ------------------------------------------------------------------------

   procedure Remove_Sprite_From_World_Map (U, V      : Int;
                                           Sprite_ID : Int) is
      S_U   : constant Sprite_Index := Sprite_Index (U);
      S_V   : constant Sprite_Index := Sprite_Index (V);
      Count : constant Sprite_Index :=
                  Sprite_Index (Sprite_Tiles.Count_Of_Sprites_In_Tiles (S_U, S_V));
      Idx   : Sprite_Index;
   begin
      for index in 1 .. Count loop
         if Sprite_Tiles.Index_Of_Sprites (S_U, S_V, index) = Sprite_ID then
            Sprite_Tiles.Index_Of_Sprites (S_U, S_V, index) := 0;
            Sprite_Tiles.Height_Of_Sprites (S_U, S_V, index) := 0.0;
            Idx := index;
         end if;
      end loop;

      if Idx <= 0 then
         raise Sprite_World_Map_Exception with
           "Sprite_World_Map.Remove_Sprite_From_World_Map, Sprite ID " &
           Int'Image (Sprite_ID) & " was not found.";
      end if;

      for index in reverse Idx .. Count - 1 loop
         Sprite_Tiles.Index_Of_Sprites (S_U, S_V, index) :=
           Sprite_Tiles.Index_Of_Sprites (S_U, S_V, index + 1);
         Sprite_Tiles.Height_Of_Sprites (S_U, S_V, index) :=
           Sprite_Tiles.Height_Of_Sprites (S_U, S_V, index + 1);
      end loop;
      Sprite_Tiles.Count_Of_Sprites_In_Tiles (S_U, S_V) :=
        Sprite_Tiles.Count_Of_Sprites_In_Tiles (S_U, S_V) - 1;

   end Remove_Sprite_From_World_Map;

   --  ------------------------------------------------------------------------

end Sprite_World_Map;
