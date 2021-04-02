
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with Game_Utils;
with Tiles_Manager;

package body Character_Map is

   use Character_Map_Package;
   package Character_Column_Package is new Ada.Containers.Vectors
     (Positive, Character_Map_List);
   subtype Character_Column is Character_Column_Package.Vector;

   use Character_Column_Package;
   package Character_Row_Package is new Ada.Containers.Vectors
     (Positive, Character_Column);
   subtype Character_Row is Character_Row_Package.Vector;

   Characters_In_Tiles : Character_Row;

   procedure Replace_Characters_In (U, V     : GL.Types.Int;
                                    New_List : Character_Map_List);

   --  -------------------------------------------------------------------------

   procedure Add_New_Character_To_Map (U, V       : GL.Types.Int;
                                       Char_Index : Positive) is
      use Ada.Containers;
      use Character_Map_Package;
      Col : Character_Column;
      CM : Character_Map_List;
   begin
      if not Tiles_Manager.Is_Tile_Valid ((U, V)) then
         raise Character_Map_Exception with
           "Character_Map.Add_New_Character_To_Map has invalid U, V";
      end if;

      if Characters_In_Tiles.Length < Count_Type (U) then
         Characters_In_Tiles.Set_Length (Count_Type (U));
      end if;

      Col := Characters_In_Tiles.Element (Integer (U));
      if Col.Length < Count_Type (V) then
         Col.Set_Length (Count_Type (V));
      end if;

      CM := Col.Element (Integer (V));
      CM.Append (Char_Index);
      Col.Replace_Element (Integer (V), CM);
      Characters_In_Tiles.Replace_Element (Integer (U), Col);

   end Add_New_Character_To_Map;

   --  -------------------------------------------------------------------------

   procedure Free_Character_Map is
      use Character_Map_Package;
   begin
      Characters_In_Tiles.Clear;
   end Free_Character_Map;

   --  -------------------------------------------------------------------------

   function Get_Characters_In (U, V : GL.Types.Int) return Character_Map_List is
      use Ada.Containers;
      use Character_Map_Package;
      Col : Character_Column;
      CM : Character_Map_Package.List;
   begin
      if not Tiles_Manager.Is_Tile_Valid ((U, V)) then
         raise Character_Map_Exception with
           "Character_Map Get_Characters_In has invalid U, V: " &
           GL.Types.Int'Image (U) & ", " & GL.Types.Int'Image (V);
      end if;

      Col := Characters_In_Tiles.Element (Positive (U));
      if Col.Length < Count_Type (V) then
         Col.Set_Length (Count_Type (V));
      end if;

      CM := Col.Element (Positive (V));
      return CM;
   end Get_Characters_In;

   --  -------------------------------------------------------------------------

   procedure Init is
   begin
      Characters_In_Tiles.Clear;
   end Init;

   --  -------------------------------------------------------------------------

   function Move_Character_In_Map
     (From_Map : GL.Types.Ints.Vector2; To_U, To_V : GL.Types.Int;
      Char_Index : Integer)
      return Boolean is
      use GL.Types;
      use Character_Controller;
      use Character_Map_Package;
      From_U : constant Int := From_Map (GL.X);
      From_V : constant Int := From_Map (GL.Y);
      Chars_N : Character_Map_List;
      Curs    : Character_Map_Package.Cursor;
      Data    : Positive;
      Result  : Boolean := From_U = To_U and From_V = To_V;
      Found   : Boolean := Result;
   begin
      if not Result then
         Result := Tiles_Manager.Is_Tile_Valid (From_Map);
         Result := Result and Tiles_Manager.Is_Tile_Valid (From_Map);
         if Result then
            Chars_N := Get_Characters_In (From_U, From_V);
            Result := not Chars_N.Is_Empty;
            if Result then
               Curs := Chars_N.Find (Char_Index);
               Found := Curs /= Character_Map_Package.No_Element;
               if Found then
                  Data := Element (Curs);
                  Chars_N.Delete (Curs);
                  Replace_Characters_In (From_U, From_V, Chars_N);
               end if;
            end if;
         end if;
      end if;

      if not Found then
         Game_Utils.Game_Log ("ERROR: Character_Map.Move_Character_In_Map, character was not in tile. Char_Index: "
                              & Positive'Image (Char_Index));
      else
         Chars_N := Get_Characters_In (To_U, To_V);
         Chars_N.Append (Data);
         Replace_Characters_In (To_U, To_V, Chars_N);
      end if;
      return Found;

   end Move_Character_In_Map;

   --  -------------------------------------------------------------------------

   procedure Replace_Characters_In (U, V     : GL.Types.Int;
                                    New_List : Character_Map_List) is
      use Ada.Containers;
      use Character_Map_Package;
      Col : Character_Column;
      CM : Character_Map_List;
   begin
      if not Tiles_Manager.Is_Tile_Valid ((U, V)) then
         raise Character_Map_Exception with
           "Character_Map Get_Characters_In has invalid U, V: " &
           GL.Types.Int'Image (U) & ", " & GL.Types.Int'Image (V);
      end if;

      Col := Characters_In_Tiles.Element (Positive (U));
      Col.Replace_Element (Positive (V), New_List);
      Characters_In_Tiles.Replace_Element (Positive (U), Col);

   end Replace_Characters_In;

   --  -------------------------------------------------------------------------

end Character_Map;
