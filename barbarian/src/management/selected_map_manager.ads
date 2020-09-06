
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Selected_Map_Manager is

 type Selected_Map_Data is record
        Map_Title        : Unbounded_String := To_Unbounded_String ("");
        Map_Intro_Text   : Unbounded_String := To_Unbounded_String ("");
        Par_Time         : Unbounded_String := To_Unbounded_String ("");
        Music_Track      : Unbounded_String := To_Unbounded_String ("");
        Hammer_Track     : Unbounded_String := To_Unbounded_String ("");
        Has_Hammer_Track : Boolean := False;
        Locked           : Boolean := False;
    end record;

    package Selected_Maps_Package is new Ada.Containers.Vectors
      (Positive, Selected_Map_Data);
    type Selected_Maps_List is new Selected_Maps_Package.Vector with null record;

    function Get_Map_Name (Maps : Selected_Maps_List; Selected_Map_ID : Positive)
                           return String;
    procedure Load_Map (Path : String; theMap : in out Selected_Map_Data;
                        Has_Hammer_Track : out Boolean);
    function Map_Locked (aMap : Selected_Map_Data) return Boolean;
    procedure Set_Map_Lock (aMap : in out Selected_Map_Data; Lock : Boolean);

end Selected_Map_Manager;
