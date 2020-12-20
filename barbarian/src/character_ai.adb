
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with Maths;

with Audio;
with Projectile_Manager;
with Specs_Manager;
with Sprite_Renderer;

package body Character_AI is

    Logic_Step_Seconds   : constant Float := 0.01;

    procedure Step_Hostile_AI (theCharacter : in out Barbarian_Character);

    --  ------------------------------------------------------------------------

    function Player_In_Line_Of_Sight
      (theCharacter : in out Barbarian_Character) return Boolean is
        SI : constant Positive := Spec_Index (theCharacter);
    begin
        return False;
    end Player_In_Line_Of_Sight;

    --  ----------------------------------------------------------------------------

    procedure Step_AI_For_Character
      (theCharacter : in out Barbarian_Character) is
        SI : constant Positive := Spec_Index (theCharacter);
    begin
        if Alive (theCharacter) and On_Ground (theCharacter) then
            if SI = Specs_Manager.Team_ID (SI) then
                Step_Hostile_AI (theCharacter);
            end if;
        end if;
    end Step_AI_For_Character;

    --  -------------------------------------------------------------------------

    procedure Step_Hostile_AI (theCharacter : in out Barbarian_Character) is
        use GL.Types;
        use Singles;
        use Maths;
        use Projectile_Manager;
        SI              : constant Positive := Spec_Index (theCharacter);
        Proj_Type       : constant Projectile_Type :=
                            Specs_Manager.Projectile_Kind (SI);
        Target_Pos      : constant GL.Types.Singles.Vector3 := Position (1);

        Tile_Dist_U     : constant Int :=
                            Abs (Map (1) (GL.X) - Map (theCharacter) (GL.X));
        Tile_Dist_V     : constant Int :=
                            Abs (Map (1) (GL.Y) - Map (theCharacter) (GL.Y));
        Tiles_To_Player : constant Int := Max_Int (Tile_Dist_U, Tile_Dist_V);
        Has_Los         : Boolean := False;
        Computed_Los    : Boolean := False;
        Sq_X_Dist       : Float;
        Weapon_Id       : Specs_Manager.Weapon_Type;
	Weapon_Range    : Float;
	Sq_Weapon_Range : Float;
        Dist_To_Player  : Singles.Vector3;
        Sprite_Pos      : Singles.Vector3;
    begin
        Put_Line ("Character_AI.Step_Hostile_AI");
        --  If warlock then recharge spells
        if Proj_Type = Skull_Proj_Type then
            Set_Skull_Countdown (theCharacter, Max_Float (0.0, Skull_Countdown
                                 (theCharacter) - Logic_Step_Seconds));
            Set_Fireball_Countdown (theCharacter, Max_Float (0.0, Fireball_Countdown
                                    (theCharacter) - Logic_Step_Seconds));
            Set_Teleport_Countdown (theCharacter, Max_Float (0.0, Teleport_Countdown
                                    (theCharacter) - Logic_Step_Seconds));
        end if;

        --  Work out if close enough to see player
        if Tiles_To_Player < Int (Specs_Manager.Sight_Range_Tiles (SI)) then
            Has_Los := Player_In_Line_Of_Sight (theCharacter);
            Computed_Los := True;
            if Has_Los then
                if Alert_Cooldown (theCharacter) <= 0.0 then
                    Audio.Play_Sound
                      (Specs_Manager.Alert_Sound_File_Name (SI), False);
                    Set_Alert_Cooldown (theCharacter, 5.0);
                end if;
                Set_Chasing_Enemy (theCharacter, True);
                Set_Has_Pathing_Destination (theCharacter, True);
                end if;
          --  Keep updating AI for a few seconds so it doesn't stop like a robot
          Update_Decay (theCharacter, 3.0);
        else
          Set_Chasing_Enemy (theCharacter, False);
          Set_Has_Pathing_Destination (theCharacter, False);
        end if;

        --  Work out "path" to enemy if not too close
        Set_Desired_Direction (theCharacter, Vec3_0);
        --  First check if close enough to attack
        if Chasing_Enemy (theCharacter) then
            Sq_X_Dist := Float (Length_Sq (Position (theCharacter) - Target_Pos));
            Weapon_Id := Current_Weapon (theCharacter);
	    Weapon_Range := Specs_Manager.Attack_Range (SI, Weapon_Id);
            Sq_Weapon_Range := Weapon_Range ** 2;
            if Sq_X_Dist < Sq_Weapon_Range then
                Set_Chasing_Enemy (theCharacter, False);
                Set_Has_Pathing_Destination (theCharacter, False);
                Dist_To_Player := Target_Pos - Position (theCharacter);
                Set_Heading (theCharacter, Direction_To_Heading (Dist_To_Player));
                if Attacking (theCharacter) then
                    Sprite_Renderer.Set_Sprite_Heading
                      (Sprite_Index (theCharacter), Heading  (theCharacter));
                      Sprite_Pos := Position (theCharacter);
                      Sprite_Pos (GL.Y) := Sprite_Pos (GL.Y) + 1.0;
                      Sprite_Renderer.Set_Sprite_Position
                      (Sprite_Index (theCharacter), Sprite_Pos);
                      Start_Attack (theCharacter);
                end if;
            end if;
        end if;

    exception
            when others =>
            Put_Line ("Character_AI.Step_Hostile_AI exception");
            raise;

    end Step_Hostile_AI;

    --  ----------------------------------------------------------------------------

end Character_AI;
