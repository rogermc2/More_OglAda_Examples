
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Types; use GL.Types;

with Maps_Manager;
with Selected_Level_Manager;

package Level_Menu_Manager is
    --  Based on gui_level_chooser.cpp map arrays
   type Level_Data is record
      Level_Name       : Unbounded_String := To_Unbounded_String ("");
      Level_Type       : Selected_Level_Manager.Selected_Level_Type
          := Selected_Level_Manager.Level_None;
      Name_Text_ID     : Positive := 1;
      Intro_Text       : Maps_Manager.Story_Lines_List;
      Par_Time         : Unbounded_String := To_Unbounded_String ("");
      Level_Map        : Maps_Manager.Map;
      Music_Track      : Unbounded_String := To_Unbounded_String ("");
      Hammer_Track     : Unbounded_String := To_Unbounded_String ("");
      Locked           : Boolean := False;
   end record;

   package Levels_Package is new Ada.Containers.Vectors
     (Positive, Level_Data);
   subtype Custom_Levels_List is Levels_Package.Vector;
   subtype Levels_List is Levels_Package.Vector;

   Levels_Manager_Exception : Exception;

   function Get_Level_Data (Maps : Levels_List; Selected_Map_ID : Positive)
                            return Level_Data;
   function Get_Level_Name (Maps : Levels_List; Selected_Map_ID : Positive)
                            return String;
   procedure Init_Levels (Maps            : in out Levels_List;
                          Selected_Map_ID : Positive;
                           Left_Margin_Cl, Top_Margin_Cl : Single);
   procedure Load_Story (Path     : String; theMap : in out Level_Data;
                         Has_Hammer_Track : out Boolean);
   procedure Load_Story_Names (Path : String; Names_List : in out Levels_List);
   function Number_Of_Levels return Integer;
   procedure Set_Level_Data (Maps : in out Levels_List; Map_ID : Positive;
                             Data : Level_Data);

end Level_Menu_Manager;
