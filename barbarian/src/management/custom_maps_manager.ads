
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Custom_Maps_Manager is

    type Custom_Names_Data is record
        Name    : Unbounded_String := To_Unbounded_String ("");
        Text_ID : Integer := 0;
    end record;
    package Custom_Names_Package is new Ada.Containers.Vectors
      (Positive, Custom_Names_Data);
    type Custom_Names_List is new Custom_Names_Package.Vector with null record;

     procedure Load_Custom_Names_List (Path : String;
                                       Names : in out Custom_Names_List;
                                       Top_Margin_Cl, Left_Margin_Cl,
                                       Text_Height : Float;
                                       Num_Custom_Maps : in out Integer);
end Custom_Maps_Manager;
