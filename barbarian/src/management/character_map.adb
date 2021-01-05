
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
        Put_Line ("Character_Map.Add_New_Character_To_Map, U, V: "
                  & GL.Types.Int'Image (U) & ", " & GL.Types.Int'Image (V));

        if not Tiles_Manager.Is_Tile_Valid (U, V) then
            raise Character_Map_Exception with
              "Character_Map.Add_New_Character_To_Map has invalid U, V";
        end if;

        Put_Line ("Character_Map.Add_New_Character_To_Map, Characters_In_Tiles length: "
                  & Integer'Image (Integer (Characters_In_Tiles.Length)));

        if Characters_In_Tiles.Length < Count_Type (U) then
            Put_Line ("Character_Map.Add_New_Character_To_Map Set_Length to "
                       & GL.Types.Int'Image (U));
            Characters_In_Tiles.Set_Length (Count_Type (U));
        end if;

        V2 := Characters_In_Tiles.Element (Integer (U));
        Put_Line ("Character_Map.Add_New_Character_To_Map V2 set, Length " &
                   Count_Type'Image (V2.Length) & ", V " & GL.Types.Int'Image (V));
        if V2.Length < Count_Type (V) then
            raise Character_Map_Exception with
              "Character_Map.Add_New_Character_To_Map has invalid V2.Length: "
              & Count_Type'Image (V2.Length);
        end if;

        if not V2.Is_Empty then
            Put_Line ("Character_Map.Add_New_Character_To_Map setting CM");
            CM := V2.Element (Integer (V));
        end if;
        Put_Line ("Character_Map.Add_New_Character_To_Map CM set");

        CM.Append (Char_Index);
        Put_Line ("Character_Map.Add_New_Character_To_Map Cm updated");

        V2.Append (CM);
        Put_Line ("Character_Map.Add_New_Character_To_Map V2 updated");
        if Characters_In_Tiles.Length < Count_Type (U) then
            Characters_In_Tiles.Set_Length (Count_Type (U));
        end if;
        Characters_In_Tiles.Replace_Element (Integer (U), V2);

        Put_Line ("Character_Map.Add_New_Character_To_Map, Characters_In_Tiles length: "
                  & Integer'Image (Integer (Characters_In_Tiles.Length)));
        Put_Line ("Character_Map.Add_New_Character_To_Map done");
        New_Line;
    end Add_New_Character_To_Map;

    --  -------------------------------------------------------------------------

    procedure Free_Character_Map is
        use Character_Map_Package;
        --          Finger_Of_Doom  : Character_Map_List;
    begin
        Characters_In_Tiles.Clear;
    end Free_Character_Map;

    --  -------------------------------------------------------------------------

    function Get_Characters_In (U, V : GL.Types.Int) return Character_Map_List is
        use Character_Map_Package;
        V2 : Character_Vector2;
        CM : Character_Map_Package.List;
    begin
        if not Tiles_Manager.Is_Tile_Valid (U, V) then
            raise Character_Map_Exception with
              "Character_Map Get_Characters_In has invalid U, V: " &
              GL.Types.Int'Image (U) & ", " & GL.Types.Int'Image (V);
        end if;

        Put_Line ("Character_Map Get_Characters_In getting V2 ");
        V2 := Characters_In_Tiles.Element (Positive (U));
        if Is_Empty (V2) then
            raise Character_Map_Exception with
              "Character_Map Get_Characters_In V2 is empty ";
        end if;
        Put_Line ("Character_Map Get_Characters_In getting CM, V2.Length, V: " &
                    Integer'Image (Integer (V2.Length)) & ", " & GL.Types.Int'Image (V));
        CM := V2.Element (Positive (V));
        Put_Line ("Character_Map Get_Characters_In U, V: " &
                    GL.Types.Int'Image (U) & ", " & GL.Types.Int'Image (V));
        return CM;
    end Get_Characters_In;

    --  -------------------------------------------------------------------------

    procedure Init is
    begin
        Characters_In_Tiles.Clear;
    end Init;

    --  -------------------------------------------------------------------------

end Character_Map;
