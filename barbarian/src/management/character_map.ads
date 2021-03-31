
with Ada.Containers.Doubly_Linked_Lists;

with GL.Types;

with Batch_Manager;
with Character_Controller;

package Character_Map is
    use GL.Types.Ints;

    package Character_Map_Package is new
     Ada.Containers.Doubly_Linked_Lists (Positive);
    subtype Character_Map_List is Character_Map_Package.List;

   Character_Map_Exception : Exception;

   procedure Add_New_Character_To_Map (U, V : GL.Types.Int;
                                       Char_Index : Positive);
   procedure Free_Character_Map;
   function Get_Characters_In (U, V : GL.Types.Int) return Character_Map_List;
   procedure Init;
   function Move_Character_In_Map
     (From_U, From_V, To_U, To_V, Char_Index : Positive) return Boolean;

end Character_Map;
