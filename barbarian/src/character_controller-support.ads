
with Specs_Manager;

package Character_Controller.Support is

    procedure Attack_With_Javelin (Character : in out Barbarian_Character;
                                   Specs_Index : Positive);
    procedure Check_End_Of_Level_Stairs
      (Character : in out Barbarian_Character;
       Level_Time : Float; Level_Par_Time : String);

end Character_Controller.Support;
