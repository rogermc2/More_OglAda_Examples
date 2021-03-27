
with Prop_Renderer_Support;
with Specs_Manager;

package Character_Controller.Support is

    procedure Attack_With_Javelin (Character : in out Barbarian_Character;
                                   Specs_Index : Positive);
    procedure Check_End_Of_Level_Stairs
      (Character : in out Barbarian_Character;
       Level_Time : Float; Level_Par_Time : String);
    procedure Grab_Nearby_Gold
      (Character : in out Barbarian_Character; Player_ID : Integer);
    procedure  Update_Camera_Position (Character : in out Barbarian_Character);

end Character_Controller.Support;
