
with GUI;
with Sprite_Renderer;

package body Character_Controller.Support is

    procedure Change_Weapon (Character : in out Barbarian_Character;
                             Character_Specs : Specs_List;
                             Weapon : Weapon_Type);
    procedure Decrement_Weapon_Count (Character : in out Barbarian_Character;
                                      Projectle : Projectile_Manager.Projectile_Type);
    procedure Set_Idle_Animation (Character : in out Barbarian_Character);
    procedure Switch_Animation (Character : in out Barbarian_Character;
                                Animation_ID : Positive);

    --  ------------------------------------------------------------------------

    procedure Attack_With_Javelin (Character : in out Barbarian_Character;
                                   Character_Specs : Specs_Manager.Specs_List) is
        use Projectile_Manager;
    begin
        Decrement_Weapon_Count (Character, Javelin_Proj_Type);
        Gui.Set_GUI_Javalin_Ammo (Javelin_Count (Character));
        --  Draw sword
        if Javelin_Count (Character) = 0 then
            Change_Weapon (Character, Character_Specs, Sword_Wt);
        end if;
    end Attack_With_Javelin;

    --  ------------------------------------------------------------------------

    procedure Change_Weapon (Character : in out Barbarian_Character;
                             Character_Specs : Specs_List;
                             Weapon : Weapon_Type) is
        use Projectile_Manager;
        Spec_ID : constant Positive := Character.Specs_Index;
        aSpec   : constant Spec_Data :=
                    Character_Specs.Element (Character.Specs_Index);
    begin
        Character.Current_Weapon := Weapon;
        Character.Is_Attacking := False;
        Character.Attack_Countdown :=
          Specs_Manager.Weapon_Attack_Time (Spec_ID, Weapon);
    end Change_Weapon;

    --  -------------------------------------------------------------------------

    procedure Decrement_Weapon_Count (Character : in out Barbarian_Character;
                                      Projectle : Projectile_Manager.Projectile_Type) is
        use Projectile_Manager;
    begin
        case Projectle is
            when Javelin_Proj_Type =>
                if Character.Javelin_Count > 0 then
                    Character.Javelin_Count := Character.Javelin_Count - 1;
                end if;
            when others => null;
        end case;
    end Decrement_Weapon_Count;

    --  -------------------------------------------------------------------------

    procedure Set_Idle_Animation (Character : in out Barbarian_Character) is
        Animation_Number : constant Positive
          := (3 * Weapon_Type'Enum_Rep (Character.Current_Weapon)) mod
          Integer (Max_Animations) + 1;
    begin
        Switch_Animation (Character, Animation_Number);
    end Set_Idle_Animation;

    --  -------------------------------------------------------------------------

    procedure Switch_Animation (Character : in out Barbarian_Character;
                                Animation_ID : Positive) is
        use Anim_Frame_Package;
        use Specs_Manager;
        Spec_ID     : constant Positive := Character.Specs_Index;
        Atlas_Index : constant Positive :=
                        Animation_Index (Spec_ID, Animation_ID, 1);
    begin
        if Character.Current_Anim /= Animation_ID then
            Character.Current_Anim := Animation_ID;
            Character.Current_Anim_Frame_Time := 0.0;
            Character.Current_Anim_Frame := 1;
            Sprite_Renderer.Set_Sprite_Current_Sprite (Character.Specs_Index,
                                                       Atlas_Index);
        end if;
    end Switch_Animation;

    --  -------------------------------------------------------------------------

end Character_Controller.Support;
