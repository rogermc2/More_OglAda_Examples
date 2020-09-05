
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Maps_Manager is

    package Story_Lines_Package is new Ada.Containers.Doubly_Linked_Lists
      (Unbounded_String);
    type Story_Lines_List is new Story_Lines_Package.List with null record;

    type Map is private;

    procedure Load_Map (Path : String; theMap : out Map;
                        Current_Pos : out Integer);
    procedure Set_Title (aMap : in out Map; Title : Unbounded_String);
    procedure Set_Par_Time (aMap : in out Map; Time : Unbounded_String);
    procedure Set_Story_Lines (aMap : in out Map; Lines : Unbounded_String);
    procedure Set_Music_Track (aMap : in out Map; Track : Unbounded_String);
    procedure Set_Hammer_Music_Track (aMap : in out Map; Track : Unbounded_String);

private
    type Map is record
       Level_Title        : Unbounded_String := Null_Unbounded_String;
       Level_Par_Time     : Unbounded_String := Null_Unbounded_String;
       Story_Lines        : Unbounded_String := Null_Unbounded_String;
       Music_Track        : Unbounded_String := Null_Unbounded_String;
       Hammer_Music_Track : Unbounded_String := Null_Unbounded_String;
    end record;

end Maps_Manager;
