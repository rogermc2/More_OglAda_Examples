
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Maps_Manager;

package Selected_Level_Manager is

   type Selected_Level_Type is (Level_Introduction, Level_Threedoors,
                                Level_Warlock, Level_Winder, Level_Under,
                                Level_Sky_Temple, Level_Hall, Level_Attercoppe,
                                Level_None);

   type Selected_Level_Data is record
      Title            : Unbounded_String := To_Unbounded_String ("");
      Level_Type       : Selected_Level_Type;
      Intro_Text       : Maps_Manager.Story_Lines_List;
      Par_Time         : Unbounded_String := To_Unbounded_String ("");
      Music_Track      : Unbounded_String := To_Unbounded_String ("");
      Hammer_Track     : Unbounded_String := To_Unbounded_String ("");
      Has_Hammer_Track : Boolean := False;
      Locked           : Boolean := False;
   end record;

   procedure Load_Map (Path             : String;
                       theMap           : in out Selected_Level_Data;
                       Has_Hammer_Track : out Boolean);
   function Level_Locked (aMap : Selected_Level_Data) return Boolean;
   procedure Set_Level_Lock (aMap : in out Selected_Level_Data; Lock : Boolean);

end Selected_Level_Manager;
