
with GL.Types; use GL.Types;

package Projectile_Manager is

    type Projectile_Type is (Javelin_Proj_Type, Dart_Proj_Type,
                             Arrow_Proj_Type, Fireball_Proj_Type,
                             Skull_Proj_Type, Na_Proj_Type);

    Update_Exception : Exception;

    procedure Init;
--      procedure Reset_Projectiles;
--      procedure Update_Projectiles (Step_Time : Float);

private
    type Characters_Hit_Array is  array (1 .. 10) of Int;
    type Projectile_Status is record
        World_Pos                 : Singles.Vector3 := (0.0, 0.0, 0.0);
        Velocity                  : Singles.Vector3 := (0.0, 0.0, 0.0);
        Current_Wind              : Singles.Vector3 := (0.0, 0.0, 0.0);
        Heading_Degrees           : Single := 0.0;
        Characters_Hit            : Characters_Hit_Array := (others => -1);
        Fired_By_Index            : Int := -1;
        Sprite_Index              : Int := -1;
	Broken_Sprite_Index       : Int := -1;
	Last_Character_Hit_Index  : Int := -1;
	Proj_Type                 : Projectile_Type := Na_Proj_Type;
	Number_Of_Characters_Hit  : Int := 0;
	Is_Airborne               : Boolean := False;
    end record;

end Projectile_Manager;
