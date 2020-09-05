
with Ada.Containers.Doubly_Linked_Lists;

with GL.Types;

package Character_Map is
    use GL.Types.Ints;

    package Character_Map_Package is new
      Ada.Containers.Doubly_Linked_Lists (Vector2);
    type Character_Map_List is private;

    procedure Init;

private
    type Character_Map_List is new Character_Map_Package.List with null record;

end Character_Map;
