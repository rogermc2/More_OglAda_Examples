
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Types; use GL.Types;

with Maps_Manager;

package Level_Menu_Manager is

   type Level_Map_Data is record
      Level_Name       : Unbounded_String := To_Unbounded_String ("");
      Name_Text_ID     : Positive := 1;
      Par_Time         : Unbounded_String := To_Unbounded_String ("");
      Level_Map        : Maps_Manager.Map;
      Music_Track      : Unbounded_String := To_Unbounded_String ("");
      Hammer_Track     : Unbounded_String := To_Unbounded_String ("");
      Locked           : Boolean := False;
   end record;

   package Levels_Package is new Ada.Containers.Vectors
     (Positive, Level_Map_Data);
   type Custom_Levels_List is new Levels_Package.Vector with null record;
   type Levels_List is new Levels_Package.Vector with null record;

   Levels_Maps_Manager_Exception : Exception;

   function Get_Map_Name (Maps : Levels_List; Selected_Map_ID : Positive)
                           return String;
   procedure Init_Level_Maps (Maps            : in out Levels_List;
                              Selected_Map_ID : Positive;
                              Left_Margin_Cl, Top_Margin_Cl : Single);
   procedure Load_Story (Path     : String; theMap : in out Level_Map_Data;
                       Has_Hammer_Track : out Boolean);
   procedure Load_Story_Names (Path : String; Names : in out Levels_List);
   function Number_Of_Levels return Integer;

end Level_Menu_Manager;
