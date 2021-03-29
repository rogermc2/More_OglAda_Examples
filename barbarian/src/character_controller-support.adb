with Audio;
with Camera;
with FB_Effects;
with GUI;
with GUI_Level_Chooser;
with Main_Menu;
with Prop_Renderer;
with Settings;
with Shadows;
with Sprite_Renderer;

package body Character_Controller.Support is

    Enter_Portal_Sound : constant String := "enter_portal.wav";

    procedure Change_Weapon (Character : in out Barbarian_Character;
                             Specs_Index : Positive;  Weapon : Weapon_Type);
    procedure Decrement_Weapon_Count
      (Character : in out Barbarian_Character;
       Projectle : Projectile_Manager.Projectile_Type);
    procedure Switch_Animation (Character : in out Barbarian_Character;
                                Animation_ID : Positive);

    --  ------------------------------------------------------------------------

    procedure Attack_With_Javelin (Character : in out Barbarian_Character;
                                   Specs_Index : Positive) is
        use Projectile_Manager;
    begin
        Decrement_Weapon_Count (Character, Javelin_Proj_Type);
        Gui.Set_GUI_Javalin_Ammo (Javelin_Count (Character));
        --  Draw sword
        if Javelin_Count (Character) = 0 then
            Change_Weapon (Character, Specs_Index, Sword_Wt);
        end if;
    end Attack_With_Javelin;

    --  ------------------------------------------------------------------------

    procedure Change_Weapon (Character : in out Barbarian_Character;
                             Specs_Index : Positive; Weapon : Weapon_Type) is
        use Projectile_Manager;
    begin
        Character.Current_Weapon := Weapon;
        Character.Is_Attacking := False;
        Character.Attack_Countdown :=
          Specs_Manager.Weapon_Attack_Time (Specs_Index, Weapon);
    end Change_Weapon;

    --  -------------------------------------------------------------------------

    procedure Check_End_Of_Level_Stairs
      (Character : in out Barbarian_Character;
       Level_Time : Float; Level_Par_Time : String) is
        Distance : constant Float :=
                     Prop_Renderer.Sq_Dist_To_End_Level_Portal (Character.World_Pos);
    begin
        if Distance < 28.0 then
            if Distance < 1.0 then
                Audio.Stop_All_Boulder_Sounds;
                if not GUI.Show_Victory then
                    GUI_Level_Chooser.Unlock_Next_Map (Main_Menu.Are_We_In_Custom_Maps);
                    GUI.Show_Victory_Screen (True, Level_Time, Level_Par_Time);
                    Audio.Play_Sound (Enter_Portal_Sound, False);
                    Camera.Set_End_Camera;
                    Character.World_Pos := Camera.World_Position;
                end if;
            Portal_Feedback_Started := True;
            end if;
            if not Portal_Feedback_Started then
                FB_Effects.Set_Feedback_Screw (1.0 - Distance / 128.0);
            end if;
        end if;

    end Check_End_Of_Level_Stairs;

    --  ------------------------------------------------------------------------

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

    procedure Grab_Nearby_Gold (Character : in out Barbarian_Character;
                                Player_ID : Integer) is
        use Prop_Renderer_Support;
        use Specs_Package;
        U             : constant Int := Character.Map (GL.X);
        V             : constant Int := Character.Map (GL.Y);
        Pos           : constant Singles.Vector3 := Character.World_Pos;
        Player_Health : constant Integer := Character.Current_Health;
        S_Index       : constant Positive := Character.Specs_Index;
        Spec          : constant Specs_Manager.Spec_Data :=
                          Specs_Manager.Get_Spec (S_Index);
        Radius        : constant Float := Spec.Width_Radius;
        Item_Type     : Property_Type := Generic_Prop;
        Value         : constant Integer := Prop_Renderer.Pick_Up_Item_In
          (Character.Map, Pos, Radius, Player_Health, Item_Type);
        Health_Factor : Single;
    begin
        case Item_Type is
            when Treasure_Prop =>
                Add_Gold_Current;
                GUI.Set_GUI_Gold (Gold_Current);
                FB_Effects.FB_Gold_Flash;
            when Food_Prop =>
                Character.Current_Health := Character.Current_Health + Value;
                Health_Factor := Single (Player_Health) /
                  Single (Spec.Initial_Health);
                GUI.Change_Health_Bar (Int (Player_ID), Health_Factor,
                                       To_String (Spec.Name));
                gui.Change_Crong_Head (Health_Factor);
                FB_Effects.FB_Green_Flash;
            when Hammer_Prop =>
                Character.Has_Hammer := True;
                Change_Weapon (Character, S_Index, Hammer_Wt);
            when others => null;
        end case;
    end Grab_Nearby_Gold;

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
        use Specs_Manager;
        use Specs_Manager.Animation_Frame_Package;
        Spec_ID     : constant Positive := Character.Specs_Index;
        Atlas_Index : constant Positive :=
                        Animation_Index (Spec_ID, Animation_ID, 1);
    begin
        if Character.Current_Animation /= Animation_ID then
            Character.Current_Animation := Animation_ID;
            Character.Current_Anim_Frame_Time := 0.0;
            Character.Current_Animation_Frame := 1;
            Sprite_Renderer.Set_Sprite_Current_Sprite (Character.Specs_Index,
                                                       Atlas_Index);
        end if;
    end Switch_Animation;

    --  -------------------------------------------------------------------------

    procedure  Update_Camera_Position (Character : in out Barbarian_Character) is
        Castor_Pos : Singles.Vector3;
        Camera_Pos : Singles.Vector3;
    begin
        if Settings.Shadows_Enabled then
            Castor_Pos := Character.World_Pos;
            Castor_Pos (GL.Y) := Castor_Pos (GL.Y) + 1.3;
            Shadows.Set_Caster_Position (Castor_Pos);
        end if;

        if not GUI.Show_Victory then
            Camera_Pos := Character.World_Pos;
            Camera_Pos (GL.Y) := Camera_Pos (GL.Y) + Camera.Camera_Height;
        end if;

    end Update_Camera_Position;

    --  -------------------------------------------------------------------------

end Character_Controller.Support;
