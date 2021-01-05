
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with Tiles_Manager;

package body Character_Map is
    use Character_Map_Package;
    package Character_Map_Vector2_Package is new Ada.Containers.Vectors
      (Positive, Character_Map_List);
    subtype Character_Map_Vector2 is Character_Map_Vector2_Package.Vector;

    use Character_Map_Vector2_Package;
    package Character_Map_Vector_Package is new Ada.Containers.Vectors
      (Positive, Character_Map_Vector2);
    subtype Character_Map_Vector is Character_Map_Vector_Package.Vector;

    Characters_In_Tiles : Character_Map_Vector;
    --  Character_Map_Vector (Character_Map_Vector2)
    --  Character_Map_Vector2 (Character_Map_List)

    --  -------------------------------------------------------------------------

    procedure Add_New_Character_To_Map (U, V       : GL.Types.Int;
                                        Char_Index : Positive) is
        use Ada.Containers;
        use Character_Map_Package;
        CM : Character_Map_Package.List := Empty_List;
        V1 : Character_Map_Vector;
        V2 : Character_Map_Vector2;
    begin
        if not Tiles_Manager.Is_Tile_Valid (U, V) then
            raise Character_Map_Exception with
              "Character_Map.Add_New_Character_To_Map has invalid U, V";
        end if;

        CM.Append (Char_Index);
        V2.Append (CM);
        Characters_In_Tiles.Append (V2);
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
        --  Character_Map_Vector (Character_Map_Vector2)
        --  Character_Map_Vector2 (Character_Map_List)
        V2 : constant Character_Map_Vector2 := Characters_In_Tiles.Element (Positive (U));
        CM : constant Character_Map_Package.List := V2.Element (Positive (V));
    begin
        if not Tiles_Manager.Is_Tile_Valid (U, V) then
            raise Character_Map_Exception with
              "Character_Map Get_Characters_In has invalid U, V: " &
              GL.Types.Int'Image (U) & ", " & GL.Types.Int'Image (V);
        end if;
        Put_Line ("Character_Map Get_Characters_In Max Rows, Cols: " &
                    GL.Types.Int'Image (Batch_Manager.Max_Rows) & ", " &
                    GL.Types.Int'Image (Batch_Manager.Max_Cols));
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
