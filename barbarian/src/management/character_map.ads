
with Ada.Containers.Doubly_Linked_Lists;

with GL.Types;

with Batch_Manager;

package Character_Map is
    use GL.Types.Ints;

    package Character_Map_Package is new
     Ada.Containers.Doubly_Linked_Lists (Positive);
    type Character_Map_List is new Character_Map_Package.List with null record;

   Character_Map_Exception : Exception;

   procedure Add_New_Character_To_Map (U, V : GL.Types.Int;
                                       Char_Index : Positive);
   procedure Free_Character_Map;
   function Get_Characters_In (U, V : GL.Types.Int) return Character_Map_List;
   procedure Init;

end Character_Map;
