
package body Character_Map is

   Characters_In_Tiles : Character_Map_List;

   --  -------------------------------------------------------------------------

    procedure Free_Character_Map is
    begin
        Characters_In_Tiles.Clear;
   end Free_Character_Map;

   --  -------------------------------------------------------------------------

    procedure Init is
    begin
        Characters_In_Tiles.Clear;
   end Init;

   --  -------------------------------------------------------------------------

end Character_Map;
