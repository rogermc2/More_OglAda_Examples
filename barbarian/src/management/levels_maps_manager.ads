
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Types; use GL.Types;

with Maps_Manager;

package Levels_Maps_Manager is

   type Level_Map_Data is record
      Map_Name         : Unbounded_String := To_Unbounded_String ("");
      Map_Name_Text_ID : Positive := 1;
      Par_Time         : Unbounded_String := To_Unbounded_String ("");
      Level_Map        : Maps_Manager.Map;
      Music_Track      : Unbounded_String := To_Unbounded_String ("");
      Hammer_Track     : Unbounded_String := To_Unbounded_String ("");
      Locked           : Boolean := False;
   end record;

   package Maps_Package is new Ada.Containers.Vectors
     (Positive, Level_Map_Data);
   type Custom_Maps_List is new Maps_Package.Vector with null record;
   type Maps_List is new Maps_Package.Vector with null record;

   Levels_Maps_Manager_Exception : Exception;

   function Get_Map_Name (Maps : Maps_List; Selected_Map_ID : Positive)
                           return String;
   procedure Init_Maps (Maps : in out Maps_List; Selected_Map_ID : Positive;
                        Left_Margin_Cl, Top_Margin_Cl : Single);
   procedure Load_Names (Path : String; Names : in out Maps_List);
   procedure Load_Map (Path             : String; theMap : in out Level_Map_Data;
                       Has_Hammer_Track : out Boolean);
   function Number_Of_Maps return Integer;

end Levels_Maps_Manager;
