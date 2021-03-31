
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with Game_Utils;
with Tiles_Manager;

package body Character_Map is

    use Character_Map_Package;
    package Character_Vector2_Package is new Ada.Containers.Vectors
      (Positive, Character_Map_List);
    subtype Character_Vector2 is Character_Vector2_Package.Vector;

    use Character_Vector2_Package;
    package Character_Vector1_Package is new Ada.Containers.Vectors
      (Positive, Character_Vector2);
    subtype Character_Vector1 is Character_Vector1_Package.Vector;

    Characters_In_Tiles : Character_Vector1;
    --  Character_Vector1 (U : Character_Map_Vector2)
    --  CharacterVector2 (V : Character_Map_List)

    --  -------------------------------------------------------------------------

    procedure Add_New_Character_To_Map (U, V       : GL.Types.Int;
                                        Char_Index : Positive) is
        use Ada.Containers;
        use Character_Map_Package;
        V2 : Character_Vector2;
        CM : Character_Map_List;
    begin
        if not Tiles_Manager.Is_Tile_Valid ((U, V)) then
            raise Character_Map_Exception with
              "Character_Map.Add_New_Character_To_Map has invalid U, V";
        end if;

        if Characters_In_Tiles.Length < Count_Type (U) then
            Characters_In_Tiles.Set_Length (Count_Type (U));
        end if;

        V2 := Characters_In_Tiles.Element (Integer (U));
        if V2.Length < Count_Type (V) then
            V2.Set_Length (Count_Type (V));
        end if;

        CM := V2.Element (Integer (V));
        CM.Append (Char_Index);
        V2.Replace_Element (Integer (V), CM);
        Characters_In_Tiles.Replace_Element (Integer (U), V2);

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
        V2 : Character_Vector2;
        CM : Character_Map_Package.List;
    begin
        if not Tiles_Manager.Is_Tile_Valid ((U, V)) then
            raise Character_Map_Exception with
              "Character_Map Get_Characters_In has invalid U, V: " &
              GL.Types.Int'Image (U) & ", " & GL.Types.Int'Image (V);
        end if;

        V2 := Characters_In_Tiles.Element (Positive (U));
        if V2.Length < Count_Type (V) then
            V2.Set_Length (Count_Type (V));
        end if;

        CM := V2.Element (Positive (V));
        return CM;
    end Get_Characters_In;

    --  -------------------------------------------------------------------------

    procedure Init is
    begin
        Characters_In_Tiles.Clear;
    end Init;

    --  -------------------------------------------------------------------------

   function Move_Character_In_Map
     (From_U, From_V, To_U, To_V, Char_Index : Positive) return Boolean is
      use GL.Types;
      use Character_Controller;
      use Character_Map_Package;
      To_IU   : constant Int := Int (To_U);
      To_IV   : constant Int := Int (To_V);
      From_IU : constant Int := Int (From_U);
      From_IV : constant Int := Int (From_V);
      Chars_N : Character_Map_List;
      Curs    : Character_Map_Package.Cursor;
      Data    : Positive;
      Result  : Boolean := From_U = To_U and From_V = To_V;
      Found   : Boolean := Result;
   begin
      if not Result then
         Result := Tiles_Manager.Is_Tile_Valid ((From_IU, From_IV));
         Result := Result and Tiles_Manager.Is_Tile_Valid ((To_IU, To_IV));
         if Result then
            Chars_N := Get_Characters_In (From_IU, From_IV);
            Result := not Chars_N.Is_Empty;
            if Result then
               Curs := Chars_N.Find (Char_Index);
               Found := Curs /= Character_Map_Package.No_Element;
               if Found then
                  Data := Element (Curs);
                  Chars_N.Delete (Curs);
               end if;
            end if;
         end if;
      end if;

      if not Found then
         Game_Utils.Game_Log ("ERROR: Character_Map.Move_Character_In_Map, character was not in tile. Char_Index: "
                                & Positive'Image (Char_Index));
      else

         Chars_N := Get_Characters_In (To_IU, To_IV);
         if Chars_N.Is_Empty then
            Chars_N.Append (Data);
         else
            Chars_N.Prepend (Data);
         end if;
      end if;
      return Found;

    end Move_Character_In_Map;

    --  -------------------------------------------------------------------------

   procedure Replace_Characters_In (U, V     : GL.Types.Int;
                                    New_List : Character_Map_List) is
        use Ada.Containers;
        use Character_Map_Package;
        V1 : Character_Vector1;
        V2 : Character_Vector2;
    begin
        if not Tiles_Manager.Is_Tile_Valid ((U, V)) then
            raise Character_Map_Exception with
              "Character_Map Get_Characters_In has invalid U, V: " &
              GL.Types.Int'Image (U) & ", " & GL.Types.Int'Image (V);
        end if;

        V2 := Characters_In_Tiles.Element (Positive (U));
        if V2.Length < Count_Type (V) then
            V2.Set_Length (Count_Type (V));
        end if;

        CM := V2.Element (Positive (V));

    end Replace_Characters_In;

    --  -------------------------------------------------------------------------

end Character_Map;
