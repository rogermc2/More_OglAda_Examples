
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Objects.Textures;

with Maths;

with Audio;
with Batch_Manager;
with Blood_Splats;
with Camera;
with Character_AI;
with Character_Controller.Support;
with Character_Map;
with Event_Controller;
with FB_Effects;
with Game_Utils;
with GUI;
with GUI_Level_Chooser;
with Input_Callback;
with Input_Handler;
with Manifold;
with Particle_System;
with Particle_System_Manager;
with Prop_Renderer;
with Sprite_Renderer;
with Tiles_Manager;

package body Character_Controller is
    Max_Blood_Fountains                 : constant Integer := 5;
    Max_Blood_Damage_Emitters           : constant Integer := 5;
    Character_Sprite_Offset_Over_Ground : constant Single := 1.0;

    --      type Integer_Array is array (Integer range <>) of Integer;
    type Character_Data is record
        Script_File   : Unbounded_String := To_Unbounded_String ("");
        U             : GL.Types.Int := 0;
        V             : GL.Types.Int := 0;
        Heading       : Maths.Degree := 0.0;
    end record;

    Current_Blood_Fountain       : Natural := 0;
    Current_Blood_Damage_Emitter : Natural := 0;
    Teleport_From_Particles_Idx  : Natural := 0;
    Teleport_To_Particles_Idx    : Natural := 0;
    Bf_Parts_Last_Attached_To    : array (1 .. Max_Blood_Fountains) of Integer
      := (-1, -1, -1, -1, -1);
    Bd_Parts_Last_Attached_To    : array (1 .. Max_Blood_Damage_Emitters) of Integer
      := (-1, -1, -1, -1, -1);
    Most_Characters_Updated_In_One_Frame : Natural := 0;

    Hammer_Hit_Armour_Sound      : constant String :=
                                     "SWORD_Hit_Metal_Armor_RR3_mono.wav";
    Sword_Hit_Armour_Sound       : constant String :=
                                     "HAMMER_Hit_Metal_Armor_stereo.wav";
    Characters                   : Character_List;
    Character_Specs              : Specs_List;
    Torch_Light_Index            : array (1 .. 2) of Positive := (1, 1);
    Torch_Particles_Index        : array (1 .. 2) of Positive := (1, 1);

    --      Portal_Fadeout_Started  : Boolean := False;
    Characters_To_Reserve   : constant Integer := 256;
    Characters_Allocd_Count : Integer := Characters_To_Reserve;
    Character_Count         : Integer := Characters_To_Reserve;
    --      Specs_Allocd_Count      : Integer := 0;
    --      Specs_Count             : Integer := 0;
    --      Gold_Current            : constant Integer := 0;
    Kills_Current                   : Integer := 0;
    Kills_Max                       : Integer := 0;
    Characters_Updated              : Integer := 0;
    --      Character_Controller_Loaded     : Boolean := False;

    Teleport_From_Particles_Index   : Integer := 1;
    Teleport_To_Particles_Index     : Integer := 1;
    Blood_Fountain_Particles_Index  : array (1 .. Max_Blood_Fountains)
      of Integer := (others => -1);
    Blood_Damage_Particles_Index    : array (1 .. Max_Blood_Damage_Emitters)
      of Integer := (others => -1);
    Bfparts_Last_Attached_To        : array (1 .. Max_Blood_Fountains)
      of Integer := (others => -1);
    Bdparts_Last_Attached_To        : array (1 .. Max_Blood_Damage_Emitters)
      of Integer := (others => -1);

    function Close_Enough (Character    : Barbarian_Character;
                           World_Pos    : Singles.Vector3;
                           Height, Dist : Single) return Boolean;
    function Damage_Character (Char_ID : Positive; Doer_ID  : Natural;
                               Angle : Maths.Degree; Weapon : Weapon_Type)
                               return Boolean;
    procedure Damage_Doer_1 (Character : in out Barbarian_Character;
                             Character_1 : Barbarian_Character);
    procedure Decapitated_Head_Check (Character : Barbarian_Character;
                                      Weapon : Weapon_Type);
    procedure Detach_Particle_System (Char_ID, Partsys_ID : Positive);
    procedure Detach_Particle_System_From_Character
      (Char_ID, Partsys_ID : Positive);
    --      function Is_Character_Valid (Char_Index : Integer) return Boolean;
    --      function Is_Spec_Valid (Spec_Index : Integer) return Boolean;
    procedure Knock_Back (Character         : in out Barbarian_Character;
                          Self_Id, Char_ID  : Positive; Weapon : Weapon_Type;
                          World_Pos         : Singles.Vector3;
                          Throw_Back_Mps    : Single);
    procedure Set_Character_Defaults (aCharacter : in out Barbarian_Character);
    procedure Spray_Screen_Check (Character : Barbarian_Character;
                                  World_Pos : Singles.Vector3);
    procedure Set_Idle_Animation (Character : in out Barbarian_Character);
    procedure Switch_Animation (Character : in out Barbarian_Character;
                                Anim_Num  : Natural);
    procedure Update_Player (Window    : in out Input_Callback.Barbarian_Window;
                             Player_ID : Integer; Seconds : Float);

    --  -------------------------------------------------------------------------

    function Alert_Cooldown (Character : Barbarian_Character) return Float is
    begin
        return Character.Alert_Cooldown_Sec;
    end Alert_Cooldown;

    --  -------------------------------------------------------------------------

    function Alive (Character : Barbarian_Character) return Boolean is
    begin
        return Character.Is_Alive;
    end Alive;

    --  --------------------------------------------------------------------------

    function Attacking (Character : Barbarian_Character) return Boolean is
    begin
        return Character.Is_Attacking;
    end Attacking;

    --  -------------------------------------------------------------------------

    procedure Attach_Particle_System_To_Character
      (Char_ID, Particle_System_ID  : Positive) is
        use Particle_System;
        Character        : Barbarian_Character := Characters.Element (Char_ID);
        Particle_Systems : Particle_Systems_List;
        System           : Particle_System.Particle_System;
        Found_Free_Slot  : Boolean := False;
        Index            : Positive := 1;
        Looping_Index    : Positive := 1;
        Script           : Particle_System_Manager.Particle_Script;

    begin
        while Index <= Character.Particle_System_Ids'Last and
          not Found_Free_Slot loop
            if Get_Particle_System (Index, System) then
                Found_Free_Slot := not Is_Running (Index);
                if not Found_Free_Slot then
                    Script := Particle_System.Get_Particle_Script
                      (Script_Index (Index));
                    if Script.Is_Looping then
                        Looping_Index := Index;
                    end if;
                end if;
            else
                raise Character_Controller_Exception;
            end if;
            Index := Index + 1;
        end loop;

        if not Found_Free_Slot then
            Game_Utils.Game_Log
              ("Character_Controller.Attach_Particle_System_To_Character " &
                 "WARNING: no free particle slot found in character - all in use. " &
                 "overwriting... Char_ID: " & Integer'Image (Char_ID));
        end if;

        Character.Particle_System_Ids (Looping_Index) := Particle_System_ID;
        Characters.Replace_Element (Char_ID, Character);

    exception
        when others => Put_Line
              ("Character_Controller.Attach_Particle_System_To_Character exception");
            raise;
    end Attach_Particle_System_To_Character;

    --  -------------------------------------------------------------------------

    procedure Change_Weapon (Character : in out Barbarian_Character;
                             Weapon : Weapon_Type) is
        Spec_ID : constant Positive := Character.Specs_Index;
    begin
        Character.Current_Weapon := Weapon;
        Character.Is_Attacking := False;
        Character.Attack_Countdown :=
          Specs_Manager.Weapon_Attack_Time (Spec_ID, Weapon);
        Set_Idle_Animation (Character);
    end Change_Weapon;

    --  ------------------------------------------------------------------------

    function Chasing_Enemy (Character : Barbarian_Character) return Boolean is
    begin
        return Character.Is_Chasing_Enemy;
    end Chasing_Enemy;

    --  ------------------------------------------------------------------------
    -- derived from _damage_all_near part // work out if close enough
    function Close_Enough (Character    : Barbarian_Character;
                           World_Pos    : Singles.Vector3;
                           Height, Dist : Single) return Boolean is
        use Singles;
        use Maths;
        C_P             : constant Singles.Vector3 := Character.World_Pos;
        Distance_Top    : constant Singles.Vector3 :=
                            World_Pos - (C_P (Gl.X), (C_P (Gl.Y) + Height),
                                         C_P (Gl.Z));
        Distance_Middle : constant Singles.Vector3
          := World_Pos - (C_P (Gl.X), C_P (Gl.Y) + Height * 0.5, C_P (Gl.Z));
        Distance_Bottom : constant Singles.Vector3
          := World_Pos - (C_P (Gl.X), C_P (Gl.Y), C_P (Gl.Z));
        Sq_Dist_Top     : constant Single := Length_Sq (Distance_Top);
        Sq_Dist_Middle  : constant Single := Length_Sq (Distance_Middle);
        Sq_Dist_Bottom  : constant Single := Length_Sq (Distance_Bottom);
    begin
        return Dist ** 2 >=  Min (Min (Sq_Dist_Top, Sq_Dist_Bottom),
                                  Sq_Dist_Middle);
    exception
        when others =>
            Put_Line ("Character_Controller.Close_Enough exception");
            raise;
            return False;
    end Close_Enough;

    --  -------------------------------------------------------------------------

    procedure Create_Character (Source       : Character_Data;
                                theCharacter : in out Barbarian_Character) is
        Diff_U     : GL.Objects.Textures.Texture;
        Spec_U     : GL.Objects.Textures.Texture;
        Spec       : Spec_Data;
        Rows       : Integer;
        Cols       : Integer;
        Tile_Y     : Single;
        W_Pos      : Singles.Vector3;
        Sprite_Pos : Singles.Vector3;
    begin
        if Source.Script_File = "" then
            raise Character_Controller_Exception with
              "Character_Controller.Create_Character, no script file name.";
        end if;

        if Tiles_Manager.Is_Tile_Valid (Source.U, Source.V) then
            Game_Utils.Game_Log ("Character_Controller.Create_Character creating character from " &
                                   To_String (Source.Script_File));
            Set_Character_Defaults (theCharacter);
            theCharacter.Heading_Deg := Source.Heading;
            theCharacter.Map := (Source.U, Source.V);
            theCharacter.Specs_Index :=
              Specs_Manager.Get_Script_Index (To_String (Source.Script_File));
            Spec := Specs_Manager.Get_Spec (theCharacter.Specs_Index);
            Diff_U := Spec.Atlas_Diffuse_ID;
            Spec_U := Spec.Atlas_Specular_ID;
            Rows := Spec.Atlas_Rows;
            Cols := Spec.Atlas_Cols;
            theCharacter.Sprite_Index :=
              Sprite_Renderer.Add_Sprite (Diff_U, Spec_U, Rows, Cols);
            theCharacter.Current_Health := Spec.Initial_Health;
            Sprite_Renderer.Set_Sprite_Heading
              (theCharacter.Sprite_Index, Source.Heading);
            Tile_Y := Tiles_Manager.Get_Tile_Height
              (Single (2 * Source.U), Single (2 * Source.V), True, True);
            W_Pos := (Single (2 * Source.U), Tile_Y, Single (2 * Source.V));
            Sprite_Pos := W_Pos;
            Sprite_Pos (GL.Y) := Sprite_Pos (GL.Y) + Character_Sprite_Offset_Over_Ground
              + Single (Spec.Sprite_Offset_Adjust);
            Sprite_Renderer.Set_Sprite_Position
              (theCharacter.Sprite_Index, Sprite_Pos);
            theCharacter.Current_Weapon := Spec.Default_Weapon;
            theCharacter.World_Pos := W_Pos;
            Characters.Append (theCharacter);
            Character_Count := Character_Count + 1;

            Character_Map.Add_New_Character_To_Map
              (Source.U, Source.V, Characters.Last_Index);
            if Spec.Team_ID = 1 then
                Kills_Max := Kills_Max + 1;
            end if;
            Game_Utils.Game_Log ("Character_Controller.Create_Character character created from " &
                                   To_String (Source.Script_File));
        else
            raise Character_Controller_Exception with
              "Character_Controller.Create_Character, invalid tile siza" &
              Int'Image (Source.U) & "x" & Int'Image (Source.V);
        end if;

    exception
        when others =>
            Put_Line ("Character_Controller.Create_Character exception");
            raise;
    end Create_Character;

    --  -------------------------------------------------------------------------

    function Current_Health (Character_ID : Positive) return Integer is
        aCharacter : constant Barbarian_Character := Characters.Element (Character_ID);
    begin
        return aCharacter.Current_Health;
    end Current_Health;

    --  -------------------------------------------------------------------------

    procedure Cycle_Weapons (Character : in out Barbarian_Character) is
    begin
        case Character.Current_Weapon is
            when Sword_Wt =>
                if Character.Javelin_Count > 0 then
                    Change_Weapon (Character, Missile_Wt);
                else
                    Change_Weapon (Character, Hammer_Wt);
                end if;
            when Missile_Wt =>
                if Character.Has_Hammer then
                    Change_Weapon (Character, Hammer_Wt);
                else
                    Change_Weapon (Character, Sword_Wt);
                end if;
            when Hammer_Wt => Change_Weapon (Character, Sword_Wt);
            when others => Change_Weapon (Character, Sword_Wt);
        end case;
    end Cycle_Weapons;

    --  -------------------------------------------------------------------------

    function Current_Kills return Integer is
    begin
        return Kills_Current;
    end Current_Kills;

    --  -------------------------------------------------------------------------

    function Current_Weapon (Character : Barbarian_Character)
                             return Specs_Manager.Weapon_Type is
    begin
        return Character.Current_Weapon;
    end Current_Weapon;

    --  -------------------------------------------------------------------------

    function Damage_All_Near
      (Self_Id        : Positive; World_Pos : Singles.Vector3;
       Damage_Range   : Single; Throw_Back_Mps : Single;  Exclude_Id : Positive;
       Weapon         : Specs_Manager.Weapon_Type) return Natural is
        use Maths;
        use Character_Map;
        use Character_Map_Package;
        Map_U               : constant Int := Int (0.5 * (World_Pos (Gl.X) + 1.0));
        Map_V               : constant Int := Int (0.5 * (World_Pos (Gl.Z) + 1.0));
        Left                : constant Int := Maths.Max_Int (0, Map_U - 1);
        Right               : constant Int := Maths.Min_Int (Batch_Manager.Max_Cols - 1, Map_U + 1);
        Up                  : constant Int := Maths.Max_Int (0, Map_V - 1);
        Down                : constant Int := Maths.Min_Int (Batch_Manager.Max_Rows - 1, Map_V + 1);
        Last_Character_Hit  : Integer := -1;
        Character_IDs       : Character_Map_List;
        Curs                : Cursor;
        Char_ID             : Positive;
        Character           : Barbarian_Character;
        Spec_ID             : Positive;
        aSpec               : Spec_Data;
        Height              : Single;
    begin
        Put_Line ("Character_Controller.Damage_All_Near");
        for index_v in Up .. Down loop
            for index_h in Left .. Right loop
                Character_IDs := Get_Characters_In (index_v, index_h);
                Curs := Character_IDs.First;
                while Has_Element (Curs) loop
                    Char_ID := Element (Curs);
                    if Char_ID /= Self_Id and Char_ID /= Exclude_Id then
                        Character := Characters.Element (Char_ID);
                        if Character.Is_Alive then
                            Spec_ID := Character.Specs_Index;
                            aSpec := Character_Specs.Element (Spec_ID);
                            if  aSpec.Initial_Health > 0 then
                                Height := Single (aSpec.Height_Metre);
                                if Close_Enough
                                  (Character, World_Pos, Height, Damage_Range) then
                                    Last_Character_Hit := Char_ID;
                                    Knock_Back (Character, Self_Id, Char_ID,
                                                Weapon, World_Pos, Throw_Back_Mps);
                                end if;
                            end if;
                        end if;
                    end if;
                    Next (Curs);
                end loop;
            end loop;
        end loop;

        Put_Line ("Character_Controller.Damage_All_Near done");
        return 0;

    exception
        when others =>
            Put_Line ("Character_Controller.Damage_All_Near exception");
            raise;
            return 0;
    end Damage_All_Near;

    --  -------------------------------------------------------------------------

    function Damage_Character (Char_ID   : Positive; Doer_ID  : Natural;
                               Angle : Maths.Degree;  Weapon : Weapon_Type)
                               return Boolean is
        use Singles;
        use Particle_System;
        use Projectile_Manager;
        Character   : Barbarian_Character := Characters.Element (Char_ID);
        Character_1 : constant Barbarian_Character := Characters.First_Element;
        Is_Sorcerer : Boolean := False;
        S_I         : constant Positive := Character.Specs_Index;
        aSpec       : constant Spec_Data := Character_Specs.Element (S_I);
        Max_Health  : Integer;
        Blood_Fountain_Id : Positive;
        Decap       : Boolean := False;
        H_Fac       : Single := 0.0;
        Tile_Height : Single := 0.0;
        Result      : Boolean := False;
    begin
       Put_Line ("Character_Controller.Damage_Character entered");
        Is_Sorcerer := aSpec.Projectile = Skull_Proj_Type;
        --  Sorcerer is invulnerable until all mirrors gone
        Result := not Is_Sorcerer or Prop_Renderer.Get_Num_Live_Mirrors <= 0;
        if Result then
            Character.Current_Health := Character.Current_Health - 1;
            Max_Health := aSpec.Initial_Health;
            Result := Max_Health > 0;
            if Result then
                H_Fac := Single (Character.Current_Health / Max_Health);
                if Char_ID = 1 then
                    GUI.Change_Health_Bar (0, H_Fac, To_String (aSpec.Name));
                    GUI.Change_Crong_Head (H_Fac);
                    FB_Effects.Set_Feedback_Effect (FB_Effects.FB_Red_Flash);
                else
                    GUI.Change_Health_Bar (1, H_Fac, To_String (aSpec.Name));
                end if;

                if Doer_ID = 1 then
                    Damage_Doer_1 (Character, Character_1);
                end if;

                if Character.Current_Health <= 0 then
                    if Weapon = Hammer_Wt then
                        GUI_Level_Chooser.Increment_Hammer_Kills;
                    end if;
                    Character.Is_Alive := False;

                    if aSpec.Tx_On_Death >= 0 then
                        Event_Controller.Transmit_Code (aSpec.Tx_On_Death);
                    end if;
                    Audio.Play_Sound (To_String (aSpec.Death_Sound_File_Name), True);

                    if Char_ID = 1 then
                        Camera.Screen_Shake (3.0, 1.0, 50.0);
                        Stop_Particle_System (Positive (Torch_Particles_Index (1)));
                        Audio.Stop_All_Boulder_Sounds;
                        GUI.Show_Defeated_Screen (True);
                    elsif not Character.Death_Was_Counted then
                        Character.Death_Was_Counted := True;
                        if aSpec.Team_ID = 2 then
                            Kills_Current := Kills_Current + 1;
                            GUI.Set_GUI_Kills (Kills_Current);
                        end if;
                    end if;  --  endif player killed

                    --  stop zombified movement
                    Character.Desired_Direction := (0.0, 0.0, 0.0);
                    --  splatter_all_tiles_near (g_characters[char_idx].world_pos);
                    Blood_Fountain_Id :=
                      Blood_Fountain_Particles_Index (Current_Blood_Fountain);
                    Detach_Particle_System_From_Character
                      (Bf_Parts_Last_Attached_To (Current_Blood_Fountain),
                       Blood_Fountain_Id);
                    Set_Particle_System_Position
                      (Blood_Fountain_Id, Character.World_Pos);
                    Start_Particle_System
                      (Blood_Fountain_Particles_Index (Current_Blood_Fountain));
                    Attach_Particle_System_To_Character (1, Blood_Fountain_Id);
                    Bf_Parts_Last_Attached_To (Current_Blood_Fountain) := 1;
                    Current_Blood_Fountain := (Current_Blood_Fountain + 1)
                    mod Max_Blood_Fountains + 1;

                    if Doer_ID > 0 then
                        Decapitated_Head_Check (Character, Weapon);
                    end if;
                end if;
            end if;
        end if;  --  not Is_Sorcerer or ...

        Audio.Play_Sound (To_String (aSpec.Hurt_Sound_File_Name), True);
        if Char_ID = 1 then
            Camera.Screen_Shake (0.4, 0.15, 50.0);
        end if;

        Detach_Particle_System_From_Character
          (Bfparts_Last_Attached_To (Current_Blood_Fountain),
           Blood_Fountain_Particles_Index (Current_Blood_Fountain));
        Set_Particle_System_Position
          (Blood_Fountain_Particles_Index (Current_Blood_Fountain),
           Character.World_Pos);
        Start_Particle_System
          (Blood_Fountain_Particles_Index (Current_Blood_Fountain));
        Attach_Particle_System_To_Character
          (Char_ID, Blood_Fountain_Particles_Index (Current_Blood_Fountain));
        Bfparts_Last_Attached_To (Current_Blood_Fountain) := Char_ID;
        Current_Blood_Fountain := (Current_Blood_Fountain + 1) * Max_Blood_Fountains;

        if Doer_ID > 0 then
            Decapitated_Head_Check (Character, Weapon);
            Decap := True;
            Spray_Screen_Check (Character, Character.World_Pos);
        end if;

        if Weapon = Pillar_Wt then
            null; --  g_pillar_crushes++
        elsif Weapon = Boulder_Wt then
            null; --  g_boulder_crushes++
        elsif Weapon = Fall_Wt then
            null; --  g_fall_kills++
        end if;

        --  if on water splash and make invisible
        if Manifold.Is_Water
          (Character.Map (GL.X), Character.Map (GL.Y)) then
            Tile_Height := Tiles_Manager.Get_Tile_Height
              (Character.World_Pos (GL.X), Character.World_Pos (GL.Z),
               True, True);
            if Character.World_Pos (GL.Y) - Tile_Height <= 0.0 then
                Prop_Renderer.Splash_Particles_At (Character.World_Pos);
                Sprite_Renderer.Set_Sprite_Visible
                  (Character.Sprite_Index, False);
            end if;
        else  --  change sprite and make it closer to ground
            Sprite_Renderer.Set_Sprite_Position
              (Character.Sprite_Index, Character.World_Pos + (0.0, 0.15, 0.0));
        end if;

        if Decap then
            Switch_Animation (Character, 16);
        else
            Switch_Animation (Character, 15);
        end if;  --  Current_Health <= 0 ???

        Audio.Play_Sound (To_String (aSpec.Hurt_Sound_File_Name), True);
        if Char_ID = 1 then
            Camera.Screen_Shake (0.4, 0.15, 50.0);
        end if;

        --  Detach damage particles
        Detach_Particle_System_From_Character
          (Bdparts_Last_Attached_To (Current_Blood_Damage_Emitter),
           Blood_Damage_Particles_Index (Current_Blood_Damage_Emitter));
        Set_Particle_System_Position
          (Blood_Damage_Particles_Index (Current_Blood_Damage_Emitter),
           Character.World_Pos);
        Set_Particle_System_Heading
          (Blood_Damage_Particles_Index (Current_Blood_Damage_Emitter),
           Angle);
        Start_Particle_System
          (Blood_Damage_Particles_Index (Current_Blood_Damage_Emitter));
        Attach_Particle_System_To_Character
          (Char_ID, Blood_Damage_Particles_Index (Current_Blood_Damage_Emitter));
        BDparts_Last_Attached_To (Current_Blood_Damage_Emitter) := Char_ID;
        Current_Blood_Damage_Emitter := (Current_Blood_Damage_Emitter + 1)
        mod Max_Blood_Damage_Emitters + 1;
        return Result;

    exception
        when others =>
            Put_Line ("Character_Controller.Damage_Character exception");
            raise;
            return False;
    end Damage_Character;

    --  -------------------------------------------------------------------------

    procedure Damage_Doer_1 (Character : in out Barbarian_Character;
                             Character_1 : Barbarian_Character) is
    begin
        if Character.Current_Weapon = Sword_Wt then
            Camera.Screen_Shake (0.2, 0.5, 50.0);
            Audio.Play_Sound (Sword_Hit_Armour_Sound, True);
        elsif Character_1.Current_Weapon = Hammer_Wt then
            Camera.Screen_Shake (0.2, 0.5, 50.0);
            Audio.Play_Sound (Hammer_Hit_Armour_Sound, True);
        end if;
    end Damage_Doer_1;

    --  -------------------------------------------------------------------------

    --  Derived from approx line 1149
    --  launch decapitated head if died by slashing weapon
    procedure Decapitated_Head_Check (Character : Barbarian_Character;
                                      Weapon : Weapon_Type) is
        use Maths;
        use GUI_Level_Chooser;
        use Projectile_Manager;
        Chance    : constant Integer := Integer (100.0 * Abs (Random_Float));
        S_I       : constant Positive := Character.Specs_Index;
        aSpec     : constant Spec_Data := Character_Specs.Element (S_I);
        DH_S_I    : constant Positive := aSpec.Decapitated_Head_Prop_Script_ID;
        Proj_Type : Projectile_Type;
        Pos       : Singles.Vector3 := Character.World_Pos;
        Decap     : Boolean := False;
    begin
        case Weapon is
        when Sword_Wt =>
            if Chance < 25 then
                Proj_Type := aSpec.Projectile;
                --                 if Proj_Type = Fireball_Proj_Type and Is_Map_Warlock and
                --                 not Cheated_On_Map and Map_Is_Unmodified then
                --                    null;
                --                 end if;
                Pos (GL.Y) := Pos (GL.Y) + 2.0;
                Prop_Renderer.Launch_Decap_Head (DH_S_I, Pos);
                Decap := True;
            end if;
        when Hammer_Wt =>
            Spray_Screen_Check (Character, Pos);
        when Pillar_Wt => null;
        when Boulder_Wt => null;
        when Fall_Wt => null;
        when others => null;
        end case;

    exception
        when others =>
            Put_Line ("Character_Controller.Launch_Decapitated_Head exception");
            raise;
    end Decapitated_Head_Check;

    --  ------------------------------------------------------------------------

    procedure Detach_Particle_System (Char_ID, Partsys_ID : Positive) is
    begin
        Detach_Particle_System_From_Character (Char_ID, Partsys_ID);

        Bdparts_Last_Attached_To (Current_Blood_Damage_Emitter) := Char_ID;
        Current_Blood_Damage_Emitter :=
          (Current_Blood_Damage_Emitter + 1) mod Max_Blood_Damage_Emitters + 1;

    exception
        when others =>
            Put_Line ("Character_Controller.Update_Player exception");
            raise;
    end Detach_Particle_System;

    --  -------------------------------------------------------------------------

    procedure Detach_Particle_System_From_Character
      (Char_ID, Partsys_ID : Positive) is
    begin
        null;
    end Detach_Particle_System_From_Character;

    --  -------------------------------------------------------------------------

    function Fireball_Countdown (Character : Barbarian_Character) return Float is
    begin
        return Character.Fireball_Countdown;
    end Fireball_Countdown;

    --  -------------------------------------------------------------------------

    function Get_Character (Character_ID : Positive) return Barbarian_Character is
    begin
        return Characters.Element (Character_ID);
    end Get_Character;

    --  -------------------------------------------------------------------------

    function Get_Character_Position (Character_ID : Positive)
                                 return Singles.Vector3  is
        theChar : constant Barbarian_Character := Characters.Element (Character_ID);
    begin
        return theChar.World_Pos;
    end Get_Character_Position;

    --  -------------------------------------------------------------------------

    function Heading (Character : Barbarian_Character) return Maths.Degree is
    begin
        return Character.Heading_Deg;
    end Heading;

    --  -------------------------------------------------------------------------

    procedure Init is
    begin
        Character_Map.Init;
        Torch_Particles_Index (1) := Particle_System.Create_Particle_System
          ("torch_smoke.particles", True, True, True);
        Torch_Particles_Index (2) :=  Torch_Particles_Index (1);

        Blood_Fountain_Particles_Index (1) := Particle_System.Create_Particle_System
          ("blood_fountain.particles", False, True, False);
        for index in 2 .. Blood_Fountain_Particles_Index'Last loop
            Blood_Fountain_Particles_Index (index) :=
              Blood_Fountain_Particles_Index (1);
        end loop;

        Blood_Damage_Particles_Index (1) := Particle_System.Create_Particle_System
          ("blood_damage.particles", False, True, False);
        for index in 2 .. Blood_Damage_Particles_Index'Last loop
            Blood_Damage_Particles_Index (index) :=
              Blood_Damage_Particles_Index (1);
        end loop;

        Teleport_From_Particles_Index := Particle_System.Create_Particle_System
          ("teleport_from.particles", False, True, False);
        Teleport_To_Particles_Index  := Particle_System.Create_Particle_System
          ("teleport_to.particles", False, True, False);

    exception
        when others =>
            Put_Line ("Character_Controller.Init exception");
            raise;
    end Init;

    --  -------------------------------------------------------------------------

    function On_Ground (Character : Barbarian_Character) return Boolean is
    begin
        return Character.Is_On_Ground;
    end On_Ground;

    --  -------------------------------------------------------------------------
    --      function Is_Character_Valid (Char_Index : Integer) return Boolean is
    --      begin
    --          return Char_Index >= 0 and Char_Index < Characters_Allocd_Count;
    --      end Is_Character_Valid;

    --  -------------------------------------------------------------------------

    --      function Is_Spec_Valid (Spec_Index : Integer) return Boolean is
    --      begin
    --          return Spec_Index >= 0 and Spec_Index < Specs_Allocd_Count;
    --      end Is_Spec_Valid;

    --  -------------------------------------------------------------------------

    function Javelin_Count (Character_ID : Positive) return Natural is
        aCharacter : constant Barbarian_Character := Characters.Element (Character_ID);
    begin
        return aCharacter.Javelin_Count;
    end Javelin_Count;

    --  -------------------------------------------------------------------------

    function Javelin_Count (Character : in out Barbarian_Character)
                        return Natural is
    begin
        return Character.Javelin_Count;
    end Javelin_Count;

    --  ---------------------------------------------------------

    procedure Knock_Back (Character         : in out Barbarian_Character;
                          Self_Id, Char_ID  : Positive; Weapon : Weapon_Type;
                          World_Pos         : Singles.Vector3;
                          Throw_Back_Mps    : Single) is
        use GL.Types.Singles;
        use Maths;
        Distance       : constant Vector3 := Character.World_Pos - World_Pos;
        Direction      : constant Vector3 := Maths.Normalized (Distance);
        Momentum       : Vector3 := Direction * Throw_Back_Mps;
        Pos            : Vector3;
        Y              : constant Single := Character.Velocity (GL.Y);
        Dir_Deg        : constant Degree :=
                           -Direction_To_Heading ((Direction (Gl.X), 0.0, -Direction (Gl.Z)));
        Did_Damage     : Boolean := Damage_Character (Char_ID, Self_Id, Dir_Deg,
                                                      Weapon);

    begin
        if Character.Is_On_Ground then
            Momentum := 0.2 * Momentum;
        end if;
        Character.Velocity := Character.Velocity + Momentum;
        Character.Velocity (GL.Y) := Y;
        Did_Damage := Damage_Character (Char_ID, Self_Id, Dir_Deg, Weapon);
        if Did_Damage then
            Pos := Character.World_Pos;
            Pos (GL.X) := Pos (GL.X) + 1.0;
            Blood_Splats.Splat_Event (Pos, 3.0);
        end if;
    exception
        when others =>
            Put_Line ("Character_Controller.Knock_Back exception");
            raise;
    end Knock_Back;

    --  -------------------------------------------------------------------------
    --  read characters from an already open file stream
    procedure Load_Characters (Input_File : File_Type; Editor_Mode : Boolean) is
        use Ada.Strings.Fixed;
        Pos1           : Integer;
        Pos2           : Integer;
        Num_Characters : Integer := 0;
        Field          : Character_Data;
        aCharacter     : Barbarian_Character;
    begin
        Game_Utils.Game_Log
          ("Character_Controller.Load_Characters, loading characters.");
        --          Portal_Fadeout_Started := False;
        Specs_Manager.Clear_Specs;

        if Characters.Is_Empty then
            Characters_Allocd_Count := Characters_To_Reserve;
        else
            Game_Utils.Game_Log
              ("Character_Controller.Load_Characters, " &
                 Ada.Containers.Count_Type'Image (Characters.Length) & "/" &
                 Integer'Image (Characters_To_Reserve) & "were used last time.");
        end if;
        Characters.Clear;
        Kills_Current := 0;
        Kills_Max := 0;

        if not Editor_Mode then
            Put_Line ("Character_Controller.Load_Characters not Editor_Mode");
            GUI.Set_GUI_Gold (Gold_Current);
            GUI.Set_GUI_Kills (Kills_Current);
            GUI.Set_GUI_Javalin_Ammo (0);
        end if;

        declare
            aLine    : constant String := Get_Line (Input_File);
            L_Length : constant Integer := aLine'Length;
        begin
            if aLine (1 .. 6) /= "chars " then
                raise Character_Controller_Exception with
                  "Character_Controller.Load_Characters, invalid Character header: "
                  & aLine;
            end if;
            Pos1 := Index (aLine (7 .. L_Length), " ");
            Num_Characters := Integer'Value (aLine (7 .. Pos1 - 1));
        end;

        Put_Line ("Character_Controller.Load_Characters Num_Characters: " &
                    Integer'Image (Num_Characters));
        for count in 1 .. Num_Characters loop
            declare
                aLine    : constant String := Get_Line (Input_File);
                L_Length : constant Integer := aLine'Length;
            begin
                Pos1 := Index (aLine, " ");
                Field.Script_File := To_Unbounded_String (aLine (1 .. Pos1 - 1));
                Pos2 := Index (aLine (Pos1 + 1 .. L_Length), ",");
                Field.U := Int'Value (aLine (Pos1 + 1 .. Pos2 - 1));
                Pos1 := Index (aLine (Pos2 + 1 .. L_Length), " ");
                Field.V := Int'Value (aLine (Pos2 + 1 .. Pos1 - 1));
                Field.Heading :=
                  Maths.Degree (Int'Value (aLine (Pos1 + 1 .. L_Length)));
                Create_Character (Field, aCharacter);
            end;
        end loop;
        Game_Utils.Game_Log
          ("Character_Controller.Load_Characters, all characters loaded.");

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Character_Controller.Load_Characters!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Load_Characters;

    --  ----------------------------------------------------------------------------

    function Map (Character_ID : Positive) return Ints.Vector2 is
        aCharacter : constant Barbarian_Character :=
                       Characters.Element (Character_ID);
    begin
        return aCharacter.Map;
    end Map;

    --  -------------------------------------------------------------------------

    function Map  (Character : Barbarian_Character) return Ints.Vector2 is
    begin
        return Character.Map;
    end Map;

    --  -------------------------------------------------------------------------

    function Max_Kills return Integer is
    begin
        return Kills_Max;
    end Max_Kills;

    --  -------------------------------------------------------------------------

    procedure Open_Door (Window  : in out Input_Callback.Barbarian_Window;
                         Character : Barbarian_Character) is
        use Maths;
        Hand_U : Int := Map (Character) (GL.X);
        Hand_V : Int := Map (Character) (GL.Y);
        Hdg    : Degree := Heading (Character);
        Result : Boolean := False;
    begin
        if Input_Handler.Was_Action_Pressed
          (Window, Input_Handler.Open_Door_Action) then
            if Hdg < 0.0 then
                Hdg := Hdg + 180.0;
            end if;
            if Hdg < 45.0 or Hdg > 315.0 then
                Hand_V := Hand_V - 1;
            elsif Hdg < 135.0 then
                Hand_U := Hand_U - 1;
            elsif Hdg < 225.0 then
                Hand_V := Hand_V + 1;
            else
                Hand_U := Hand_U + 1;
            end if;

            Result := Prop_Renderer.Activate_Door_In_Tile
              (Hand_U, Hand_V, 1.0 + Character.World_Pos (GL.Y),
               Prop_Renderer.Prop_Activator_Player_State);
            Result := Prop_Renderer.Activate_Door_In_Tile
              (Character.Map (GL.X), Character.Map (GL.Y), 1.0 + Character.World_Pos (GL.Y),
               Prop_Renderer.Prop_Activator_Player_State);
        end if;

    end Open_Door;

    --  -------------------------------------------------------------------------

    procedure Process_Characters_2 (Self_ID, Exclude_ID : Positive;
                                    World_Pos : Singles.Vector3;
                                    Char_List : Character_Map.Character_Map_List) is
        use Character_Map.Character_Map_Package;
        use GL.Types;
        use Singles;
        use Maths;
        Index           : Positive;
        Spec_Index      : Positive;
        Spec_Health     : Integer;
        Spec_Height     : Single;
        Char_Position   : Singles.Vector3;
        Distance_Top    : Singles.Vector3;
        Distance_Middle : Singles.Vector3;
        Distance_Bottom : Singles.Vector3;
        Sq_Dist_Top     : Single;
        Sq_Dist_Middle  : Single;
        Sq_Dist_Bottom  : Single;
        Sq_Dist         : Single;

        aCharacter  : Barbarian_Character;
        Curs        : Cursor := Char_List.First;
    begin
        while Has_Element (Curs) loop
            Index := Element (Curs);
            if not (Index = Self_ID) and not (Index = Exclude_ID) then
                aCharacter := Characters.Element (Index);
                if aCharacter.Is_Alive  then
                    Spec_Index := aCharacter.Specs_Index;
                    Spec_Health := Initial_Health (Spec_Index);
                    if Spec_Health > 0 then
                        -- Work out if close enough - add some height off floor
                        Spec_Height := Single (Height (Spec_Index));
                        Char_Position := aCharacter.World_Pos;
                        Distance_Top :=
                          World_Pos - (Char_Position (GL.X),
                                       Char_Position (GL.Y) + Spec_Height,
                                       Char_Position (GL.Z));
                        Distance_Middle :=
                          World_Pos - (Char_Position (GL.X),
                                       Char_Position (GL.Y) + 0.5 * Spec_Height,
                                       Char_Position (GL.Z));
                        Distance_Bottom := World_Pos - Char_Position;
                        Sq_Dist_Top :=
                          Distance_Top (GL.X) ** 2 + Distance_Top (GL.Y) ** 2 +
                          Distance_Top (GL.Z) ** 2;
                        Sq_Dist_Middle :=
                          Distance_Middle (GL.X) ** 2 +
                          Distance_Middle (GL.Y) ** 2 +
                          Distance_Middle (GL.Z) ** 2;
                        Sq_Dist_Bottom :=
                          Distance_Bottom (GL.X) ** 2 +
                          Distance_Bottom (GL.Y) ** 2 +
                          Distance_Bottom (GL.Z) ** 2;
                        Sq_Dist :=
                          Min (Min (Sq_Dist_Top, Sq_Dist_Bottom), Sq_Dist_Middle);
                    end if;
                end if;
            end if;
            Next (Curs);
        end loop;

    exception
        when others =>
            Put_Line ("Character_Controller.Process_Characters_2 exception");
            raise;
    end Process_Characters_2;

    --  -------------------------------------------------------------------------

    function Position (Character_ID : Positive) return Singles.Vector3 is
        aCharacter : constant Barbarian_Character :=
                       Characters.Element (Character_ID);
    begin
        return aCharacter.World_Pos;
    end Position;

    --  -------------------------------------------------------------------------

    function Position (Character : Barbarian_Character) return Singles.Vector3 is
    begin
        return Character.World_Pos;
    end Position;

    --  -------------------------------------------------------------------------

    function Skull_Countdown (Character : Barbarian_Character) return Float is
    begin
        return Character.Skull_Countdown;
    end Skull_Countdown;

    --  -------------------------------------------------------------------------

    procedure Set_Alert_Cooldown (Character : in out Barbarian_Character;
                                  Value : Float) is
    begin
        Character.Alert_Cooldown_Sec := Value;
    end Set_Alert_Cooldown;

    --  ------------------------------------------------------------------------

    procedure Set_Character_Defaults (aCharacter : in out Barbarian_Character) is
    begin
        aCharacter.Destination_Tile := (-1, -1);
        aCharacter.Is_Alive := True;
        aCharacter.Is_On_Ground := True;
    end Set_Character_Defaults;

    --  -------------------------------------------------------------------------

    procedure Set_Chasing_Enemy (Character : in out Barbarian_Character;
                                 State : Boolean) is
    begin
        Character.Is_Chasing_Enemy := State;
    end Set_Chasing_Enemy;

    --  ------------------------------------------------------------------------

    procedure Set_Desired_Direction (Character : in out Barbarian_Character;
                                     Direction : Singles.Vector3) is
    begin
        Character.Desired_Direction := Direction;
    end Set_Desired_Direction;

    --  ------------------------------------------------------------------------

    procedure Set_Has_Pathing_Destination
      (Character : in out Barbarian_Character; State : Boolean) is
    begin
        Character.Has_Pathing_Destination := State;
    end Set_Has_Pathing_Destination;

    --  ------------------------------------------------------------------------

    procedure Set_Fireball_Countdown (Character : in out Barbarian_Character;
                                      Seconds : Float) is
    begin
        Character.Fireball_Countdown := Seconds;
    end Set_Fireball_Countdown;

    --  ------------------------------------------------------------------------

    procedure Set_Heading (Character : in out Barbarian_Character;
                           Heading : Maths.Degree) is
    begin
        Character.Heading_Deg := Heading;
    end Set_Heading;

    --  ------------------------------------------------------------------------

    procedure Set_Idle_Animation (Character : in out Barbarian_Character) is
    begin
        Switch_Animation
          (Character, Weapon_Type'Enum_Rep (Character.Current_Weapon) + 1);
    end Set_Idle_Animation;

    --  ------------------------------------------------------------------------

    procedure Set_Skull_Countdown (Character : in out Barbarian_Character;
                                   Seconds : Float) is
    begin
        Character.Skull_Countdown := Seconds;
    end Set_Skull_Countdown;

    --  ------------------------------------------------------------------------

    procedure Set_Teleport_Countdown (Character : in out Barbarian_Character;
                                      Seconds : Float) is
    begin
        Character.Teleport_Countdown := Seconds;
    end Set_Teleport_Countdown;

    --  ------------------------------------------------------------------------

    function Spec_Index (Character : Barbarian_Character) return Positive is
    begin
        return Character.Specs_Index;
    end Spec_Index;

    --  -------------------------------------------------------------------------

    function Spec_Index (Character_ID : Positive) return Positive is
        aCharacter : constant Barbarian_Character :=
                       Characters.Element (Character_ID);
    begin
        return aCharacter.Specs_Index;
    end Spec_Index;

    --  -------------------------------------------------------------------------

    procedure Spray_Screen_Check (Character : Barbarian_Character;
                                  World_Pos : Singles.Vector3) is
        Cam_Y       : constant Single := Character.World_Pos (Gl.Y) + 13.0;
        Emi_Y       : constant Single := World_Pos (Gl.Y);
        --   check right height range
        Y_Above     : constant Single := Cam_Y - Emi_Y - 0.5;
        Yfac        : constant Single := Y_Above / 15.0;
        --   now check xz distance
        Cam_X       : constant Single := Character.World_Pos (Gl.X);
        Cam_Z       : constant Single := Character.World_Pos (Gl.Z);
        Emi_X       : constant Single := World_Pos (Gl.X);
        Emi_Z       : constant Single := World_Pos (Gl.Z);
        Xdist       : constant Single := Cam_X - Emi_X;
        Zdist       : constant Single := Cam_Z - Emi_Z;
        Sqxzdist    : constant Single := Xdist ** 2 + Zdist ** 2;
        --   max xz dist is a funnel shape 0.5 at bottom of height range, 4.0 at top
        Maxxzdist   : constant Single := 0.5 + Yfac * 4.0;
        Sqmaxxzdist : constant Single := Maxxzdist ** 2;
        Nearfac     : constant Single :=  Sqxzdist / Sqmaxxzdist;
        Num_Splats  : constant Integer := Integer (40.0 * Nearfac) + 3;
    begin

        if Y_Above >= 0.0 and Y_Above <= 15.0 and
          Sqxzdist <= Sqmaxxzdist then
            GUI.Add_Screen_Splats (Num_Splats);
        end if;

    exception
        when others =>
            Put_Line ("Character_Controller.Spray_Screen_Check exception");
            raise;
    end Spray_Screen_Check;

    --  -------------------------------------------------------------------------

    function Sprite_Index (Character : Barbarian_Character) return Positive is
    begin
        return Character.Sprite_Index;
    end Sprite_Index;

    --  -------------------------------------------------------------------------

    procedure Start_Attack (Character : in out Barbarian_Character) is
        use Singles;
        use Projectile_Manager;
        use  Character_Controller.Support;
        Spec_Index      : constant Positive := Character.Specs_Index;
        Spec            : constant Spec_Data :=
                            Character_Specs.Element (Spec_Index);
        Weapon          : constant Weapon_Type := Character.Current_Weapon;
        Recharging      : Boolean := False;
        Projectile      : Projectile_Manager.Projectile_Type;
        Offset_Pos      : Singles.Vector4;
        Rotation_Matrix : Singles.Matrix4 := Singles.Identity4;
        Facing          : Singles.Vector3;
    begin
        Put_Line ("Character_Controller.Start_Attack");
        if not Character.Is_Attacking then
            Character.Is_Attacking := True;
            if Weapon = Missile_Wt or Weapon = Skull_Wt or
              Weapon = Teleport_Wt then
                Projectile := Spec.Projectile;
                case Projectile is
                when  Arrow_Proj_Type => Offset_Pos := (0.0, 1.5, -1.0, 1.0);
                when Fireball_Proj_Type => Offset_Pos := (0.0, 1.5, -0.5, 1.0);
                when others => Offset_Pos := (0.5, 1.5, 0.0, 1.0);
                end case;
            end if;

            Rotation_Matrix := Maths.Rotate_Y_Degree
              (Rotation_Matrix, Heading (Character));
            Facing := To_Vector3 (Rotation_Matrix * Offset_Pos) +
              Position (Character);
            case Projectile is
            when Javelin_Proj_Type => Attack_With_Javelin
                  (Character, Character.Specs_Index);
            when Arrow_Proj_Type =>
                null;
            when Fireball_Proj_Type =>
                null;
            when Skull_Proj_Type
                =>
                null;
            when others => null;
            end case;
        end if;

    exception
        when others =>
            Put_Line ("Character_Controller.Start_Attack exception");
            raise;
    end Start_Attack;

    --  -------------------------------------------------------------------------

    procedure Switch_Animation (Character : in out Barbarian_Character;
                                Anim_Num  : Natural) is
        Spec_Index  : constant Positive := Character.Specs_Index;
        Spec        : constant Spec_Data := Character_Specs.Element (Spec_Index);
        Atlas_Index : constant Positive := Animation_Index (Spec_Index, Anim_Num, 1);
    begin
        Put_Line ("Character_Controller.Switch_Animation");
        if Character.Current_Anim /= Anim_Num then
            if Anim_Num > Natural (Max_Animations) then
                raise Character_Controller_Exception;
            end if;
            Character.Current_Anim := Anim_Num;
            Character.Current_Anim_Frame_Time := 0.0;
            Character.Current_Anim_Frame := 0;
            Sprite_Renderer.Set_Sprite_Current_Sprite
              (Character.Sprite_Index, Atlas_Index);
        end if;

    exception
        when others =>
            Put_Line ("Character_Controller.Switch_Animation exception");
            raise;
    end Switch_Animation;

    --  -------------------------------------------------------------------------

    function Teleport_Countdown (Character : Barbarian_Character) return Float is
    begin
        return Character.Teleport_Countdown;
    end Teleport_Countdown;

    --  -------------------------------------------------------------------------

    procedure  Update_Attack (theCharacter : in out Barbarian_Character;
                              Seconds : Float) is
    begin
        Null;
    end Update_Attack;

    --  -------------------------------------------------------------------------

    procedure Update_Character (theCharacter : in out Barbarian_Character;
                                Seconds : Float) is
    begin
        Put_Line ("Character_Controller.Update_Character");
        if theCharacter.Update_Decay > 0.0 then
            theCharacter.Update_Decay := theCharacter.Update_Decay - Seconds;
        else
            theCharacter.Needs_Update := False;
        end if;
        if theCharacter.Alert_Cooldown_Sec > 0.0 then
            theCharacter.Alert_Cooldown_Sec :=
              theCharacter.Alert_Cooldown_Sec - Seconds;
        end if;

        if theCharacter.Is_Alive then
            Character_AI.Step_AI_For_Character (theCharacter);
        end if;

    exception
        when others =>
            Put_Line ("Character_Controller.Update_Character exception");
            raise;
    end Update_Character;

    --  -------------------------------------------------------------------------

    procedure Update_Characters (Window  : in out Input_Callback.Barbarian_Window;
                                 Seconds : Float) is
        use Maths;
        use Batch_Manager;
        use Character_Map.Character_Map_Package;
        aCharacter      : Barbarian_Character;
        Char_List       : Character_Map.Character_Map_List;
        Curs            : Cursor;
        Char_Index      : Positive;
        Update_Distance : constant Int := 5;
        Left            : Int;
        Right           : Int;
        Up              : Int;
        Down            : Int;
        Ok              : Boolean := True;
    begin
        Put_Line ("Character_Controller.Update_Characters");
        if not Characters.Is_Empty then
            aCharacter := Characters.First_Element;
            Characters_Updated := 0;
            --              if Character_Controller_Loaded then
            Update_Player (Window, 1, Seconds);
            aCharacter.Needs_Update := False;
            Characters_Updated := Characters_Updated + 1;
            Left := Max_Int (0, aCharacter.Map (GL.X) - Update_Distance);
            Right := Min_Int
              (Max_Cols - 1, aCharacter.Map (GL.X) + Update_Distance);
            Up := Max_Int (0, aCharacter.Map (GL.Y) - Update_Distance);
            Down := Min_Int
              (Max_Rows - 1, aCharacter.Map (GL.Y) + Update_Distance);
            --  Collect all characters around p1
            for v in Up .. Down loop
                for h in Left .. Right loop
                    Put_Line ("Character_Controller.Update_Characters v, h :" &
                             Int'Image (v) & ", " & Int'Image (h));
                    Char_List := Character_Map.Get_Characters_In (h, v);
                    Curs := Char_List.First;
                    Put_Line ("Character_Controller.Update_Characters v, h :" &
                             Int'Image (v) & ", " & Int'Image (h));
                    while Has_Element (Curs) loop
                        Char_Index := Element (Curs);
                        aCharacter := Characters.Element (Char_Index);
                        if aCharacter.Is_Alive  then
                            aCharacter.Needs_Update := True;
                            Characters.Replace_Element (Char_Index, aCharacter);
                        end if;
                        Next (Curs);
                    end loop;
                end loop;
            end loop;
            Put_Line ("Character_Controller.Update_Characters 2nd loop");

            Curs := Char_List.First;
            while Has_Element (Curs) loop
                Char_Index := Element (Curs);
                aCharacter := Characters.Element (Char_Index);
                if aCharacter.Needs_Update then
                    Update_Character (aCharacter, Seconds);
                    Characters.Replace_Element (Char_Index, aCharacter);
                end if;
                Next (Curs);
                --   Process_Characters
                --    (Self_ID, Exclude_ID, World_Pos, Char_List);
            end loop;
        end if;
        Put_Line ("Character_Controller.Update_Characters done");

    exception
        when others =>
            Put_Line ("Character_Controller.Update_Characters exception");
            raise;

    end Update_Characters;

    --  -------------------------------------------------------------------------

    procedure Update_Decay (Character : in out Barbarian_Character;
                            Seconds : Float) is
    begin
        Character.Update_Decay := Seconds;
    end Update_Decay;

    --  -------------------------------------------------------------------------

    procedure Update_Player (Window    : in out Input_Callback.Barbarian_Window;
                             Player_ID : Integer; Seconds : Float) is
        use Input_Handler;
        Character : Barbarian_Character := Characters.Element (Player_ID);
    begin
        if Character.Is_Alive then
            if Is_Action_Down (Up_Action) then
                Character.Desired_Direction (GL.Z) := -1.0;
                Character.Is_Walking := True;
            elsif Is_Action_Down (Down_Action) then
                Character.Desired_Direction (GL.Z) := 1.0;
                Character.Is_Walking := True;
            elsif Is_Action_Down (Left_Action) then
                Character.Desired_Direction (GL.X) := -1.0;
                Character.Is_Walking := True;
            elsif Is_Action_Down (Right_Action) then
                Character.Desired_Direction (GL.X) := 1.0;
                Character.Is_Walking := True;
            end if;

            if Was_Action_Pressed (Window, Sword_Action) then
                Change_Weapon (Character, Sword_Wt);
            elsif Was_Action_Pressed (Window, Javelin_Action) and
              Character.Javelin_Count > 0 then
                Change_Weapon (Character, Missile_Wt);
            elsif Was_Action_Pressed (Window, Hammer_Action) and
              Character.Has_Hammer then
                Change_Weapon (Character, Hammer_Wt);
            elsif Was_Action_Pressed (Window, Cycle_Weapons_Action) then
                Cycle_Weapons (Character);
                Change_Weapon (Character, Hammer_Wt);
            elsif Was_Action_Pressed (Window, Attack_Action) then
                Start_Attack (Character);
            elsif Was_Action_Pressed (Window, Open_Door_Action) then
                Null;
            end if;

            Update_Attack (Character, Seconds);
        end if;

    exception
        when others =>
            Put_Line ("Character_Controller.Update_Player exception");
            raise;
    end Update_Player;

    --  -------------------------------------------------------------------------

end Character_Controller;
