
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Types; use GL.Types;

package Custom_Levels_Manager is

   type Custom_Data is record
      Name    : Unbounded_String := To_Unbounded_String ("");
      Text_ID : Integer := 0;
   end record;
   package Custom_Levels_Package is new Ada.Containers.Vectors
     (Positive, Custom_Data);
   type Custom_Levels_List is new Custom_Levels_Package.Vector with null record;

   function Get_Custom_Map_Name (Custom_Maps  : Custom_Levels_List;
                                 Selected_Map : Positive) return String;
   procedure Load_Custom_Map (Path                                       : String;
                              Custom_Maps                                : in out Custom_Levels_List;
                              Top_Margin_Cl, Left_Margin_Cl, Text_Height : Single;
                              Num_Custom_Maps                            : in out Integer);
   procedure Replace_Custom_Map (Path        : String;
                                 Levels      : in out Custom_Levels_List;
                                 Top_Margin_Cl, Left_Margin_Cl,
                                 Text_Height : Single; Level_ID : Positive);
end Custom_Levels_Manager;
