
with GL.Types; use GL.Types;

with Maths;

package Projectile_Manager is

    type Projectile_Type is (Javelin_Proj_Type, Dart_Proj_Type,
                             Arrow_Proj_Type, Fireball_Proj_Type,
                             Skull_Proj_Type, Na_Proj_Type);

    Update_Exception : Exception;

    procedure Init;
    procedure Create_Javelin (Fired_By : Positive; Position : Singles.Vector3;
                              Heading : Maths.Degree);
    procedure Reset_Projectiles;
    procedure Update_Projectiles (Seconds : Float);

private
    type Characters_Hit_Array is  array (1 .. 10) of Int;
    type Projectile_Status is record
        World_Pos                 : Singles.Vector3 := (0.0, 0.0, 0.0);
        Velocity                  : Singles.Vector3 := (0.0, 0.0, 0.0);
        Current_Wind              : Singles.Vector3 := (0.0, 0.0, 0.0);
        Heading_Degrees           : Maths.Degree := 0.0;
        Characters_Hit            : Characters_Hit_Array := (others => -1);
        Fired_By_Index            : Natural := 0;
        Sprite_Index              : Natural := 0;
	Broken_Sprite_Index       : Natural := 0;
	Last_Character_Hit_Index  : Natural := 0;
	Proj_Type                 : Projectile_Type := Na_Proj_Type;
	Number_Of_Characters_Hit  : Integer := 0;
	Is_Airborne               : Boolean := False;
    end record;

end Projectile_Manager;
