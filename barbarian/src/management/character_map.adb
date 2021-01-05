
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

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
        V2 : Character_Vector2;   --
        CM : Character_Map_List;
    begin
        if not Tiles_Manager.Is_Tile_Valid (U, V) then
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
        if not Tiles_Manager.Is_Tile_Valid (U, V) then
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

end Character_Map;
