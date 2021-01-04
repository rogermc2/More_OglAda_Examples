
with Ada.Text_IO; use Ada.Text_IO;

with Tiles_Manager;

package body Character_Map is

   type Character_Map_Array is array
     (1 .. Batch_Manager.Max_Rows, 1 .. Batch_Manager.Max_Cols) of
     Character_Map_List;

   Characters_In_Tiles : Character_Map_Array;

   --  -------------------------------------------------------------------------

   procedure Add_New_Character_To_Map (U, V       : GL.Types.Int;
                                       Char_Index : Positive) is
      use Character_Map_Package;
   begin
      if not Tiles_Manager.Is_Tile_Valid (U, V) then
         raise Character_Map_Exception with
            "Character_Map.Add_New_Character_To_Map has invalid U, V";
      end if;

      Put_Line ("Character_Map.Add_New_Character_To_Map"  &
              GL.Types.Int'Image (U) & ", " & GL.Types.Int'Image (V));
      Characters_In_Tiles (U, V).Append (Char_Index);
   end Add_New_Character_To_Map;

   --  -------------------------------------------------------------------------

   procedure Free_Character_Map is
      use Character_Map_Package;
      Finger_List    : Character_Map_List;
    begin
      for u_index in Character_Map_Array'Range loop
         for v_index in Character_Map_Array'Range (2) loop
            if not Characters_In_Tiles (u_index, v_index).Is_Empty then
               Finger_List := Characters_In_Tiles (u_index, v_index);
               Finger_List.Clear;
            end if;
         end loop;
      end loop;
   end Free_Character_Map;

   --  -------------------------------------------------------------------------

   function Get_Characters_In (U, V : GL.Types.Int) return Character_Map_List is
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
        return Characters_In_Tiles (U, V);
   end Get_Characters_In;

   --  -------------------------------------------------------------------------

    procedure Init is
   begin
      for u_index in Character_Map_Array'Range loop
         for v_index in Character_Map_Array'Range (2) loop
            Characters_In_Tiles (u_index, v_index).Clear;
         end loop;
      end loop;
   end Init;

   --  -------------------------------------------------------------------------

end Character_Map;
