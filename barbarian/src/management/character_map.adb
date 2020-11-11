
with Tiles_Manager;

package body Character_Map is

   type Character_Map_Array is array
     (1 .. Batch_Manager.Max_Cols, 1 .. Batch_Manager.Max_Cols) of
     Character_Map_List;

   Characters_In_Tiles : Character_Map_Array;

   --  -------------------------------------------------------------------------

   procedure Add_New_Character_To_Map (U, V       : GL.Types.Int;
                                       Char_Index : Positive) is
      Character_List    : Character_Map_List;
   begin
      if not Tiles_Manager.Is_Tile_Valid (U, V) then
         raise Character_Map_Exception;
      end if;
      Character_List := Characters_In_Tiles (U, V);
      if not Character_List.Contains (Char_Index) then
         Character_List.Append (Char_Index);
      end if;
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
