
with Ada.Characters;
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
with GL_Utils;
with GUI;
with GUI_Level_Chooser;
with Input_Callback;
with Input_Handler;
with Manifold;
with Particle_System;
with Particle_System_Manager;
with Properties_Manager;
with Prop_Renderer;
with Prop_Renderer_Support;
with Sprite_Renderer;
with Tiles_Manager;

package body Character_Controller is
   Max_Blood_Fountains                 : constant Integer := 5;
   Max_Blood_Damage_Emitters           : constant Integer := 5;
   Character_Sprite_Offset_Over_Ground : constant Single := 1.0;
   Torch_Light_Range                   : constant Single := 8.0;
   Hello_Friend_Sound_File             : constant String := "hello_my_frien.ogg";
   Fall_Damage_Sound_File              : constant String :=
                                           "BREAK_Crack_stereo.wav";
   Fall_Death_Sound_File               : constant String :=
                                           "GORE_Splat_Hit_Short_mono.wav";

   type Character_Data is record
      Script_File   : Unbounded_String := To_Unbounded_String ("");
      U             : GL.Types.Int := 0;
      V             : GL.Types.Int := 0;
      Heading       : Maths.Degree := 0.0;
   end record;

   Fall_Dmage_Start_Height   : constant Float := 5.0;   -- Distance of falling below which does no damage
   Fall_Damage_Death_Height  : constant Float := 7.0;  -- Distance of falling beyond which always kills instantly

   Current_Blood_Fountain               : Natural := 0;
   Current_Blood_Damage_Emitter         : Natural := 0;
   Teleport_From_Particles_Idx          : Natural := 0;
   Teleport_To_Particles_Idx            : Natural := 0;
   Bf_Parts_Last_Attached_To            : array (1 .. Max_Blood_Fountains) of Integer
     := (-1, -1, -1, -1, -1);
   Bd_Parts_Last_Attached_To            : array (1 .. Max_Blood_Damage_Emitters) of Integer
     := (-1, -1, -1, -1, -1);
   Most_Characters_Updated_In_One_Frame : Natural := 0;

   Hammer_Hit_Armour_Sound      : constant String :=
                                    "SWORD_Hit_Metal_Armor_RR3_mono.wav";
   Sword_Hit_Armour_Sound       : constant String :=
                                    "HAMMER_Hit_Metal_Armor_stereo.wav";
   Torch_Light_Index            : array (1 .. 2) of Positive := (1, 1);
   Torch_Particles_Index        : array (1 .. 2) of Positive := (1, 1);

   --      Portal_Fadeout_Started  : Boolean := False;
   Characters_To_Reserve           : constant Integer := 256;
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

   Current_Gold    : Integer := 0;
   Max_Gold        : Integer := 0;
   Treasure_Found  : Integer := 0;

   function Close_Enough (Character    : Barbarian_Character;
                          World_Pos    : Singles.Vector3;
                          Height, Dist : Single) return Boolean;
   function Damage_Character (Character       : in out Barbarian_Character;
                              Damage, Doer_ID : Natural;
                              Angle           : Maths.Degree; Weapon : Weapon_Type)
                              return Boolean;
   procedure Damage_Doer_1 (Character   : in out Barbarian_Character;
                            Character_1 : Barbarian_Character);
   procedure Decapitated_Head_Check (Character : Barbarian_Character;
                                     Weapon    : Weapon_Type);
   procedure Detach_Particle_System (Char_ID, Partsys_ID : Positive);
   procedure Detach_Particle_System_From_Character
     (Char_ID, Partsys_ID : Positive);
   procedure Knock_Back (Character         : in out Barbarian_Character;
                         Self_Id           : Positive;
                         Damage            : Natural; Weapon : Weapon_Type;
                         World_Pos         : Singles.Vector3;
                         Throw_Back_Mps    : Single);
   procedure Set_Character_Defaults (aCharacter : in out Barbarian_Character);
   procedure Spray_Screen_Check (Character : Barbarian_Character;
                                 World_Pos : Singles.Vector3);
   procedure Set_Idle_Animation (Character : in out Barbarian_Character);
   procedure Switch_Animation (Character : in out Barbarian_Character;
                               Anim_Num  : Natural);
   procedure Update_Character_Physics (Character : in out Barbarian_Character;
                                       Seconds   : Float);

   procedure Update_Character_Stairs (Character          : in out Barbarian_Character;
                                      Effective_Velocity : in out Singles.Vector3);
   procedure Update_Character_Position (Character          : in out Barbarian_Character;
                                        Effective_Velocity : in out Singles.Vector3;
                                        Seconds            : Float);
   procedure Update_Desired_Velocity (Character : in out Barbarian_Character);

   procedure Update_Player (Window    : in out Input_Callback.Barbarian_Window;
                            Player_ID : Integer; Seconds : Float);

   --  -------------------------------------------------------------------------

   procedure Add_Gold_Current (Amount : Integer := 1) is
   begin
      Current_Gold := Amount;
   end Add_Gold_Current;

   --  ------------------------------------------------------------------------

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
                            Weapon    : Weapon_Type) is
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

      if Tiles_Manager.Is_Tile_Valid ((Source.U, Source.V)) then
         Set_Character_Defaults (theCharacter);
         theCharacter.Heading_Deg := Source.Heading;
         theCharacter.Map := ((Source.U, Source.V));
         theCharacter.Specs_Index :=
           Specs_Manager.Get_Script_Index (To_String (Source.Script_File));
         Spec := Specs_Manager.Get_Spec (theCharacter.Specs_Index);
         Rows := Spec.Atlas_Rows;
         Cols := Spec.Atlas_Cols;
         if not Spec.Atlas_Diffuse_ID.Initialized then
            raise Character_Controller_Exception with "Character_Controller.Create_Character, " &
              "Diff_Map texture has not been initialized";
         end if;
         if not Spec.Atlas_Specular_ID.Initialized then
            raise Character_Controller_Exception with "Character_Controller.Create_Character, " &
              "Spec_Map texture has not been initialized";
         end if;
         theCharacter.Sprite_Index :=
           Sprite_Renderer.Add_Sprite
             (Spec.Atlas_Diffuse_ID, Spec.Atlas_Specular_ID, Rows, Cols);

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

         Character_Map.Add_New_Character_To_Map
           (Source.U, Source.V, Characters.Last_Index);
         if Spec.Team_ID = 1 then
            Kills_Max := Kills_Max + 1;
         end if;

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
      Damage_Range   : Single; Damage : Natural; Throw_Back_Mps : Single;
      Exclude_Id     : Positive; Weapon : Specs_Manager.Weapon_Type)
      return Natural is
      use Maths;
      use Character_Map;
      use Character_Map_Package;
      Map_U               : constant Int := Int (0.5 * (World_Pos (Gl.X) + 1.0));
      Map_V               : constant Int := Int (0.5 * (World_Pos (Gl.Z) + 1.0));
      Left                : constant Int := Maths.Max_Int (0, Map_U - 1);
      Right               : constant Int
          := Maths.Min_Int (Tiles_Manager.Max_Map_Cols - 1, Map_U + 1);
      Up                  : constant Int := Maths.Max_Int (0, Map_V - 1);
      Down                : constant Int
          := Maths.Min_Int (Tiles_Manager.Max_Map_Rows - 1, Map_V + 1);
      Last_Character_Hit  : Integer := -1;
      Character_IDs       : Character_Map_List;
      Curs                : Cursor;
      Char_ID             : Positive;
      Character           : Barbarian_Character;
      Spec_ID             : Positive;
      aSpec               : Spec_Data;
      Height              : Single;
   begin
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
                     aSpec := Specs_Manager.Get_Spec (Spec_ID);
                     if  aSpec.Initial_Health > 0 then
                        Height := Single (aSpec.Height_Metre);
                        if Close_Enough
                          (Character, World_Pos, Height, Damage_Range) then
                           Last_Character_Hit := Char_ID;
                           Knock_Back
                             (Character, Self_Id, Damage,
                              Weapon, World_Pos, Throw_Back_Mps);
                        end if;
                     end if;
                  end if;
               end if;
               Next (Curs);
            end loop;
         end loop;
      end loop;

      return 0;

   exception
      when others =>
         Put_Line ("Character_Controller.Damage_All_Near exception");
         raise;
         return 0;
   end Damage_All_Near;

   --  -------------------------------------------------------------------------

   function Damage_Character (Character       : in out Barbarian_Character;
                              Damage, Doer_ID : Natural;
                              Angle           : Maths.Degree;  Weapon : Weapon_Type)
                              return Boolean is
      use Singles;
      use Particle_System;
      use Projectile_Manager;
      Char_ID           : constant Positive :=
                            Characters.Find_Index (Character);
      Character_1       : constant Barbarian_Character :=
                            Characters.First_Element;
      Is_Sorcerer       : Boolean := False;
      S_I               : constant Positive := Character.Specs_Index;
      aSpec             : constant Spec_Data := Specs_Manager.Get_Spec (S_I);
      Max_Health        : Integer;
      Blood_Fountain_Id : Positive;
      Decap             : Boolean := False;
      H_Fac             : Single := 0.0;
      Tile_Height       : Single := 0.0;
      Result            : Boolean := False;
   begin
      Is_Sorcerer := aSpec.Projectile = Skull_Proj_Type;
      --  Sorcerer is invulnerable until all mirrors gone
      Result := not Is_Sorcerer or Prop_Renderer.Get_Num_Live_Mirrors <= 0;
      if Result then
         Character.Current_Health := Character.Current_Health - Damage;
         Max_Health := aSpec.Initial_Health;
         Result := Max_Health > 0;
         if Result then
            H_Fac := Single (Character.Current_Health / Max_Health);
            if Char_ID = 1 then
               GUI.Change_Health_Bar (0, H_Fac, To_String (aSpec.Name));
               GUI.Change_Crong_Head (H_Fac);
               FB_Effects.Set_Feedback_Effect (FB_Effects.FB_Red_Flash_Effect);
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
        ((Character.Map (GL.X), Character.Map (GL.Y))) then
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
      Current_Blood_Damage_Emitter :=
        (Current_Blood_Damage_Emitter + 1) mod Max_Blood_Damage_Emitters + 1;
      return Result;

   exception
      when others =>
         Put_Line ("Character_Controller.Damage_Character exception");
         raise;
         return False;
   end Damage_Character;

   --  -------------------------------------------------------------------------

   procedure Damage_Doer_1 (Character   : in out Barbarian_Character;
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
                                     Weapon    : Weapon_Type) is
      use Maths;
      use GUI_Level_Chooser;
      use Projectile_Manager;
      Chance    : constant Integer := Integer (100.0 * Abs (Random_Float));
      S_I       : constant Positive := Character.Specs_Index;
      aSpec     : constant Spec_Data := Specs_Manager.Get_Spec (S_I);
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

   function Get_Character_Height_Near (Excluded_Char   : Barbarian_Character;
                                       Also_Exclude_ID : Natural;
                                       Next_Pos        : Singles.Vector3)
                                       return Single is
      use Singles;
      use Maths;
      use Character_Map;
      use Character_Map_Package;
      Excluded_ID   : constant Positive := Characters.Find_Index (Excluded_Char);
      Self_S_I      : constant Positive := Excluded_Char.Specs_Index;
      Self_Spec     : constant Spec_Data := Specs_Manager.Get_Spec (Self_S_I);
      Other_Spec    : Spec_Data;
      aChar         : Barbarian_Character;
      Char_S_I      : Positive;
      Width_Radius  : constant Float := Self_Spec.Width_Radius;
      Next_U        : constant Positive :=
                        Positive (0.5 * (1.0 + Next_Pos (GL.X)));
      Next_V        : constant Positive :=
                        Positive (0.5 * (1.0 + Next_Pos (GL.Z)));
      Left          : constant Integer := Max_Integer (1, Next_U);
      Right         : constant Integer
        := Min_Integer (Integer (Tiles_Manager.Max_Map_Cols), Next_U);
      Up            : constant Integer := Max_Integer (1, Next_V);
      Down          : constant Integer
        := Min_Integer (Integer (Tiles_Manager.Max_Map_Rows), Next_V);
      Char_List     : Character_Map_List;
      Curs          : Cursor := Char_List.First;
      List_Index    : Natural;
      Distance      : Vector3;
      Self_Height   : Float;
      Threshold     : constant Float := 0.25;
      Self_Head     : Float;
      Other_Head    : Single;
      Sq_Dist       : Single;
      Continue      : Boolean := True;
      Height        : Single := -100.0;
   begin
      for h in Left .. Right loop
         for v in Up .. Down loop
            Char_List := Get_Characters_In (Int (h), Int (v));
            while Has_Element (Curs) loop
               List_Index := Element (Curs);
               if List_Index /= Excluded_ID and
                 List_Index /= Also_Exclude_ID then
                  aChar := Characters.Element (List_Index);
                  if aChar.Is_Alive then
                     Distance := aChar.World_Pos - Next_Pos;
                     Self_Height := 0.0;
                     if aChar.Is_Alive then
                        Self_Height := Self_Spec.Height_Metre;
                        Self_Head := Float (Next_Pos (GL.Y)) + Self_Height - Threshold;
                        Continue := Float (aChar.World_Pos (GL.Y)) <= Self_Head;
                     end if;
                  end if;
                  if Continue then
                     Sq_Dist := Distance (GL.X) ** 2 + Distance (GL.Z) ** 2;
                     if Sq_Dist <= Single (Width_Radius ** 2) then
                        Char_S_I := aChar.Specs_Index;
                        Other_Spec := Specs_Manager.Get_Spec (Char_S_I);
                        Other_Head := aChar.World_Pos (GL.Y) +
                          Single (Other_Spec.Height_Metre);
                        Height := Max (Height, Other_Head);
                     end if;
                  end if;
               end if;
               Next (Curs);
            end loop;
         end loop;
      end loop;
      return Height;

   end Get_Character_Height_Near;

   --  -------------------------------------------------------------------------

   function Get_Min_Height_For_Character
     (Character : Barbarian_Character; Also_Exclude : Natural;
      Pos : Singles.Vector3; Extra_Radius : Float) return Single is
      use Maths;
      use Tiles_Manager;
      S_I          : constant Positive := Character.Specs_Index;
      aSpec        : constant Spec_Data := Specs_Manager.Get_Spec (S_I);
      Radius       : constant Single :=
                       Single (aSpec.Width_Radius + Extra_Radius);
      Floor_Height : Single :=
                       Get_Tile_Height (Pos (GL.X), Pos (GL.Z), True, True);
      NW           : Singles.Vector3 := Pos;
      SE           : Singles.Vector3 := Pos;
      Self_Height  : constant Single := Single (aSpec.Height_Metre);
      P_Height     : constant Single :=
                       Prop_Renderer.Get_Prop_Height_Between (NW, SE);
      Char_Height  : Single;
   begin
      Floor_Height :=
        Max (Floor_Height, Get_Tile_Height (Pos (GL.X) + Radius,
             Pos (GL.Z), True, True));
      Floor_Height :=
        Max (Floor_Height, Get_Tile_Height (Pos (GL.X) - Radius,
             Pos (GL.Z), True, True));
      Floor_Height :=
        Max (Floor_Height, Get_Tile_Height (Pos (GL.X),
             Pos (GL.Z) + Single (Radius), True, True));
      Floor_Height :=
        Max (Floor_Height, Get_Tile_Height (Pos (GL.X),
             Pos (GL.Z) - Single (Radius), True, True));

      NW (GL.X) := NW (GL.X) - Radius;
      NW (GL.Y) := NW (GL.Y) - Self_Height;
      NW (GL.Z) := NW (GL.Z) - Radius;

      SE (GL.X) := NW (GL.X) + Radius;
      SE (GL.Z) := NW (GL.Z) + Radius;

      Char_Height :=
        Get_Character_Height_Near (Character, Also_Exclude, Pos);

      return Max (Floor_Height, Max (Char_Height, P_Height));

   end Get_Min_Height_For_Character;

   --  -------------------------------------------------------------------------

   function Gold_Current return Integer is
   begin
      return Current_Gold;
   end Gold_Current;

   --  -------------------------------------------------------------------------

   function Gold_Max return Integer is
   begin
      return Max_Gold;
   end Gold_Max;

   --  -------------------------------------------------------------------------

   function Heading (Character : Barbarian_Character) return Maths.Degree is
   begin
      return Character.Heading_Deg;
   end Heading;

   --  -------------------------------------------------------------------------

   procedure Init is
   begin
      Specs_Manager.Clear_Specs;
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
                         Self_Id           : Positive;
                         Damage            : Natural; Weapon : Weapon_Type;
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
      Did_Damage     : Boolean := Damage_Character
        (Character, Self_Id, Damage, Dir_Deg, Weapon);

   begin
      if Character.Is_On_Ground then
         Momentum := 0.2 * Momentum;
      end if;
      Character.Velocity := Character.Velocity + Momentum;
      Character.Velocity (GL.Y) := Y;
      Did_Damage := Damage_Character (Character, Damage, Self_Id, Dir_Deg, Weapon);
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
      --          Portal_Fadeout_Started := False;
      Specs_Manager.Clear_Specs;

      if Characters.Is_Empty then
         null;
         --              Characters_Allocd_Count := Characters_To_Reserve;
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
         GUI.Set_GUI_Gold (Current_Gold);
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

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Character_Controller.Load_Characters!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
         raise;
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

   procedure Open_Door (Window    : in out Input_Callback.Barbarian_Window;
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
            Properties_Manager.Prop_Activator_Player_State);
         Result := Prop_Renderer.Activate_Door_In_Tile
           (Character.Map (GL.X), Character.Map (GL.Y), 1.0 + Character.World_Pos (GL.Y),
            Properties_Manager.Prop_Activator_Player_State);
      end if;

   end Open_Door;

   --  -------------------------------------------------------------------------

   procedure Process_Characters_2 (Self_ID, Exclude_ID : Positive;
                                   World_Pos           : Singles.Vector3;
                                   Char_List           : Character_Map.Character_Map_List) is
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
                                 Value     : Float) is
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
                                State     : Boolean) is
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

   procedure Set_Gold_Current (Amount : Integer) is
   begin
      Current_Gold := Amount;
   end Set_Gold_Current;

   --  ------------------------------------------------------------------------

   procedure Set_Gold_Max (Amount : Integer) is
   begin
      Max_Gold := Amount;
   end Set_Gold_Max;

   --  ------------------------------------------------------------------------

   procedure Set_Has_Pathing_Destination
     (Character : in out Barbarian_Character; State : Boolean) is
   begin
      Character.Has_Pathing_Destination := State;
   end Set_Has_Pathing_Destination;

   --  ------------------------------------------------------------------------

   procedure Set_Fireball_Countdown (Character : in out Barbarian_Character;
                                     Seconds   : Float) is
   begin
      Character.Fireball_Countdown := Seconds;
   end Set_Fireball_Countdown;

   --  ------------------------------------------------------------------------

   procedure Set_Heading (Character : in out Barbarian_Character;
                          Heading   : Maths.Degree) is
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
                                  Seconds   : Float) is
   begin
      Character.Skull_Countdown := Seconds;
   end Set_Skull_Countdown;

   --  ------------------------------------------------------------------------

   procedure Set_Teleport_Countdown (Character : in out Barbarian_Character;
                                     Seconds   : Float) is
   begin
      Character.Teleport_Countdown := Seconds;
   end Set_Teleport_Countdown;

   --  ------------------------------------------------------------------------

   procedure Set_Total_Treasure_Found (Amount : Integer) is
   begin
      Treasure_Found := Amount;
   end Set_Total_Treasure_Found;

   --  -------------------------------------------------------------------------

   procedure Set_Walk_Animation (Character : in out Barbarian_Character) is
   begin
      Switch_Animation (Character,
                        3 *  Weapon_Type'Enum_Rep (Character.Current_Weapon) + 1);
   end Set_Walk_Animation;

   --  -------------------------------------------------------------------------

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
                          Specs_Manager.Get_Spec (Spec_Index);
      Weapon          : constant Weapon_Type := Character.Current_Weapon;
      Recharging      : Boolean := False;
      Projectile      : Projectile_Manager.Projectile_Type;
      Offset_Pos      : Singles.Vector4;
      Rotation_Matrix : Singles.Matrix4 := Singles.Identity4;
      Facing          : Singles.Vector3;
   begin
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
                 (Character, Spec_Index);
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
      Spec        : constant Spec_Data := Specs_Manager.Get_Spec (Spec_Index);
      Atlas_Index : constant Positive := Animation_Index (Spec_Index, Anim_Num, 1);
   begin
      if Character.Current_Animation /= Anim_Num then
         if Anim_Num > Natural (Max_Animations) then
            raise Character_Controller_Exception;
         end if;
         Character.Current_Animation := Anim_Num;
         Character.Current_Anim_Frame_Time := 0.0;
         Character.Current_Animation_Frame := 0;
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

   function Total_Treasure_Found return Integer is
   begin
      return Treasure_Found;
   end Total_Treasure_Found;

   --  -------------------------------------------------------------------------

   procedure  Update_Attack (theCharacter : in out Barbarian_Character;
                             Seconds      : Float) is
   begin
      Null;
   end Update_Attack;

   --  -------------------------------------------------------------------------

   procedure  Update_Animation_Frame
     (theCharacter  : in out Barbarian_Character;
      Curr_Frame_ID : Positive; theAnimation : Animation_Frame_List;
      Frame_Length  : Single; Anim_Size : Integer) is

      Atlas_Index : Positive;
   begin
      if theCharacter.Current_Anim_Frame_Time > Float (Frame_Length) then
         theCharacter.Current_Animation_Frame :=
           theCharacter.Current_Animation_Frame + 1;
         theCharacter.Current_Animation_Frame :=
           theCharacter.Current_Animation_Frame mod Anim_Size;
         --  don't loop attack animations when done early
         if not theCharacter.Is_Attacking or
           (theCharacter.Current_Anim_Frame_Time /= 0.0) then
            --  tell sprite renderer to change frame index
            Atlas_Index :=
              theAnimation.Element (Curr_Frame_ID).Atlas_Index;
            Sprite_Renderer.Set_Sprite_Current_Sprite
              (theCharacter.Sprite_Index, Atlas_Index);
         end if;
         theCharacter.Current_Anim_Frame_Time := 0.0;
      end if;
   end Update_Animation_Frame;

   --  -------------------------------------------------------------------------

   procedure Update_Character (theCharacter : in out Barbarian_Character;
                               Seconds      : Float) is
   begin
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

   procedure  Update_Character_Animation (Character_ID : Positive;
                                          Seconds      : Float) is
      Character     : Barbarian_Character :=
                        Characters.Element (Character_ID);
      Anim_Number   : constant Integer := Character.Current_Animation;
      S_I           : constant Positive := Character.Specs_Index;
      aSpec         : constant Spec_Data := Specs_Manager.Get_Spec (S_I);
      theAnimation  : Animation_Frame_List;
      Anim_Size     : Integer;
      Curr_Frame_ID : Integer;
      Curr_Frame    : Animation_Frame;
      Frame_Length  : Single;
   begin
      if Anim_Number > 0 then
         Anim_Size :=
           Integer (aSpec.Animation_Frame_Count (Int (Anim_Number)));
         if Anim_Size > 0 then
            Curr_Frame_ID := Character.Current_Animation_Frame;
            theAnimation := aSpec.Animations.Element (Anim_Number);
            Frame_Length := theAnimation (Curr_Frame_ID).Seconds;

            --  check if stopped moving
            if Character.Is_Alive and Character.Is_Walking and
              Character.Is_Attacking then
               Set_Idle_Animation (Character);
            end if;

            if Frame_Length > 0.0 then
               Update_Animation_Frame (Character, Curr_Frame_ID,
                                       theAnimation, Frame_Length, Anim_Size);
            end if;
         end if;
      end if;

      if Character.Is_On_Ground or Character.Is_Attacking then
         Character.Current_Anim_Frame_Time :=
           Character.Current_Anim_Frame_Time + Seconds;
      end if;

      Characters.Replace_Element (Character_ID, Character);

   end Update_Character_Animation;

   --  -------------------------------------------------------------------------

   procedure Update_Characters (Window  : in out Input_Callback.Barbarian_Window;
                                Seconds : Float) is
      use Maths;
      use Tiles_Manager;
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
      if not Characters.Is_Empty then
         aCharacter := Characters.First_Element;
         Characters_Updated := 0;

         Update_Player (Window, 1, Seconds);
         aCharacter.Needs_Update := False;
         Characters_Updated := Characters_Updated + 1;
         Left := Max_Int (0, aCharacter.Map (GL.X) - Update_Distance);
         Right := Min_Int
           (Max_Map_Cols - 1, aCharacter.Map (GL.X) + Update_Distance);
         Up := Max_Int (0, aCharacter.Map (GL.Y) - Update_Distance);
         Down := Min_Int
           (Max_Map_Rows - 1, aCharacter.Map (GL.Y) + Update_Distance);
         --  Collect all characters around p1
         for v in Up .. Down loop
            for h in Left .. Right loop
               Char_List := Character_Map.Get_Characters_In (h, v);
               Curs := Char_List.First;
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

         Curs := Char_List.First;
         while Has_Element (Curs) loop
            Char_Index := Element (Curs);
            aCharacter := Characters.Element (Char_Index);
            if aCharacter.Needs_Update then
               Update_Character (aCharacter, Seconds);
               Characters.Replace_Element (Char_Index, aCharacter);
            end if;
            Next (Curs);
         end loop;
      end if;

   exception
      when others =>
         Put_Line ("Character_Controller.Update_Characters exception");
         raise;
   end Update_Characters;

   --  -------------------------------------------------------------------------

   function Update_Character_Accel_Decel
     (Character : in out Barbarian_Character;
      Dimension : GL.Index_Homogeneous; Seconds   : Float) return Boolean is
      use Singles;
      use Maths;
      Accel_Mpsps : constant Float := 20.0;
      Decel_Mpsps : constant Float := 25.0;
      Diff        : constant Vector3 :=
                      Character.Desired_Velocity - Character.Velocity;
      Threshold   : constant Single := 0.01;
      Result      : Boolean := False;
   begin
      if Character.Is_On_Ground then
         if abs (Character.Desired_Velocity (Dimension)) > 0.0 then
            Character.Velocity (Dimension) := Character.Velocity (Dimension) +
              Min (Diff (Dimension), Single (Accel_Mpsps * Seconds));
         else
            Character.Velocity (Dimension) := Character.Velocity (Dimension) -
              Min (-Diff (Dimension), Single (Accel_Mpsps * Seconds));
         end if;
      else
         if Character.Desired_Velocity (Dimension) > 0.0 then
            Character.Velocity (Dimension) := Character.Velocity (Dimension) -
              Min (-Diff (Dimension), Single (Decel_Mpsps * Seconds));
         else
            Character.Velocity (Dimension) := Character.Velocity (Dimension) +
              Min (Diff (Dimension), Single (Decel_Mpsps * Seconds));
         end if;
      end if;  --  Character.Is_On_Ground

      Result := abs (Character.Desired_Velocity (Dimension)) > Threshold;
      if not Result then
         Character.Velocity (Dimension) := 0.0;
      end if;

      return Result;

   end Update_Character_Accel_Decel;

   --  -------------------------------------------------------------------------

   procedure  Update_Character_Gravity (Character : in out Barbarian_Character;
                                        Seconds   : Float) is
      use Singles;
   begin
      if not Character.Is_On_Ground then
         Character.Is_Walking := False;
         Character.Velocity (GL.Y) := Character.Velocity (GL.Y) +
           (-10.0) * Single (Seconds);  --  Gravity mpsps
      end if;
   end Update_Character_Gravity;

   --  -------------------------------------------------------------------------

   procedure  Update_Character_Motion (Character : in out Barbarian_Character;
                                       Seconds   : Float) is
      use Singles;
      S_I           :  Positive;
      aSpec         :  Spec_Data;
      Sprite_Pos    : Singles.Vector3;
   begin
      S_I := Character.Specs_Index;
      aSpec := Specs_Manager.Get_Spec (S_I);
      Character.Velocity := (0.0, 0.0, 0.0);
      if Character.Is_Walking or Character.Is_On_Ground then
         Update_Desired_Velocity (Character);
      end if;
      Update_Character_Physics (Character, Seconds);
      if Character.Is_Moving or Character.Is_On_Ground or
        Character.First_Update then
         Sprite_Pos := Character.World_Pos;
         Sprite_Pos (GL.Y) := Character_Sprite_Offset_Over_Ground +
           Single (aSpec.Sprite_Offset_Adjust);
      else
         Sprite_Pos := Character.World_Pos + (0.0, 0.15, 0.0);
      end if;
      Sprite_Renderer.Set_Sprite_Position (Character.Sprite_Index, Sprite_Pos);

   end Update_Character_Motion;

   --  -------------------------------------------------------------------------

   procedure Update_Character_Physics (Character : in out Barbarian_Character;
                                       Seconds   : Float) is
      use Singles;
      a             : constant Boolean :=
                        Update_Character_Accel_Decel  (Character, GL.X, Seconds);
      b             : constant Boolean :=
                        Update_Character_Accel_Decel  (Character, GL.Z, Seconds);
      Inert_Thresh  : constant Single := 0.01;
      Radius_Extra  : constant Float := -0.2;
      Effective_Vel : Vector3 := Character.Velocity;
      Water_Height  : Single;
      Prev_Height   : Single;
      Height        : Single;
      Tile_Height   : Single;
      Next_U        : Int;
      Next_V        : Int;
      Char_Index    : Positive;
      Num_D_Sixes   : Integer;
      Damage        : Integer;
      Max_Damage    : Integer;
      Damaged       : Boolean := False;
   begin
      Update_Character_Gravity (Character, Seconds);
      if (not a) and then (not b) then
         Character.Is_Walking := False;
      end if;

      if Manifold.Is_Water (Character.Map) then
         Water_Height :=
           Tiles_Manager.Get_Tile_Height (Character.World_Pos (GL.X),
                                          Character.World_Pos (GL.Z), True, True);
         if Water_Height + 0.1 > Character.World_Pos (GL.Y) then
            Effective_Vel := 0.7 * Effective_Vel;
         end if;

         --  work out stairs slow-down, speed-up
         Update_Character_Stairs (Character, Effective_Vel);
      end if;

      Update_Character_Position (Character, Effective_Vel, Seconds);

      --  attempt to reset state
      Character.Is_Moving := abs (Effective_Vel (GL.X)) >= Inert_Thresh or
      abs (Effective_Vel (GL.Y)) >= Inert_Thresh or
      abs (Effective_Vel (GL.Z)) >= Inert_Thresh;
      if Character.Is_Moving then
         Character.Needs_Update := True;
      end if;

      Prev_Height := Character.World_Pos (GL.Y);
      if Character.Is_Moving then
         Character.World_Pos := Character.World_Pos +
           Single (Seconds) * Effective_Vel;
         Character.Distance_Fallen := Character.Distance_Fallen +
           Maths.Max_Float (0.0, Float (Prev_Height - Character.World_Pos (GL.Y)));
         --  update character map entry
         Next_U := Int (0.5 * (1.0 + Character.World_Pos (GL.X)));
         Next_V := Int (0.5 * (1.0 + Character.World_Pos (GL.Z)));

         Char_Index := Characters.Find_Index (Character);
         if not Character_Map.Move_Character_In_Map
           (Character.Map, Next_U, Next_V, Char_Index) then
            raise Character_Controller_Exception with
              "ERROR: Update_Character_Physics moving character's map entry: "
              & Positive'Image (Char_Index);
         end if;
         Character_Controller.Support.Trigger_Tx (Character);
         Character.Map := ((Next_U, Next_V));
      end if;  --  Character.Is_Moving

      Height := Get_Min_Height_For_Character (Character, 0, Character.World_Pos,
                                              Radius_Extra);
      if Character.World_Pos (GL.Y) <= Height then
         --  put on ground but not if it's a huge difference or we'll fly up
         --  over tall things -- better to be 'stuck'
         if Character.World_Pos (GL.Y) + Char_Mount_Wall_Max_Height >= Height then
            Character.World_Pos (GL.Y) := Height;
         end if;
         Character.Velocity (GL.Y) := 0.0;
         Character.Is_On_Ground := True;
         if Character.Distance_Fallen > 0.1 and
           Manifold.Is_Water (Character.Map) then
            --  Make sure we've hit the ground and not a bridge type of thing
            Tile_Height :=
              Tiles_Manager.Get_Tile_Height (Character.World_Pos (GL.X),
                               Character.World_Pos (GL.Z), True, True);
            if Character.World_Pos (GL.Y) - Tile_Height <= 0.0 then
               Prop_Renderer.Splash_Particles_At (Character.World_Pos);
            end if;
         end if;

         --  check if fallen too far
         if Character.Distance_Fallen > Fall_Dmage_Start_Height then
            Num_D_Sixes := Integer (Character.Distance_Fallen / 4.0);
            Max_Damage := 50 * Num_D_Sixes;
            Damage := abs (Integer (Maths.Random_Float)) * Max_Damage;
            if Character.Distance_Fallen > Fall_Damage_Death_Height then
               Damage := 1000;
            end if;
            Audio.Play_Sound (Fall_Death_Sound_File, True);
            Damaged := Damage_Character (Character, Damage, 0, 0.0, Fall_Wt);
         end if;
         Character.Distance_Fallen := 0.0;
      else  --  allow to fall off ledges
         Character.Is_On_Ground := False;
      end if;

      if not Character.Is_On_Ground then
         Character.Needs_Update := True;
      end if;

   end Update_Character_Physics;

   --  -------------------------------------------------------------------------

   procedure Update_Character_Position (Character          : in out Barbarian_Character;
                                        Effective_Velocity : in out Singles.Vector3;
                                        Seconds            : Float) is
      use Singles;
      Next_Pos           : constant Vector3 := Character.World_Pos +
                             Effective_Velocity * Single (Seconds);
      Height             : constant Single := Get_Min_Height_For_Character
        (Character, 0,  Next_Pos, -0.2);
      Max_Climb          : constant Single :=
                             Character.World_Pos (GL.Y) + Char_Mount_Wall_Max_Height;
      Bounce_Factor      : constant Single := 0.4;
      Inert_Threshold    : constant Single := 0.01;
      Radius_Extra       : constant Float := -0.02;
      Next_Pos_X         : Vector3;
      Next_Pos_Z         : Vector3;
      Height_X           : Single;
      Height_Z           : Single;
      Bounced_XV         : Single;
      Bounced_ZV         : Single;
      Bounced_Pos        : Vector3;
      Max_Bounced_Height : Single;
      Bounced_Height     : Single;
   begin
      if Height > Max_Climb then
         Next_Pos_X := Character.World_Pos +
           Single (Seconds) * (Effective_Velocity (GL.X),
                               Effective_Velocity (GL.Y), 0.0);
         Next_Pos_Z := Character.World_Pos +
           Single (Seconds) * (0.0, Effective_Velocity (GL.Y),
                               Effective_Velocity (GL.Z));
         Height_X := Get_Min_Height_For_Character (Character, 0, Next_Pos_X, -0.2);
         Height_Z := Get_Min_Height_For_Character (Character, 0, Next_Pos_Z, -0.2);

         if Height_X > Max_Climb then
            Effective_Velocity (GL.X) := 0.0;
            Bounced_XV := -Bounce_Factor * Character.Velocity (GL.X);
         else
            Bounced_XV := Character.Velocity (GL.X);
         end if;
         if Height_Z > Max_Climb then
            Effective_Velocity (GL.Z) := 0.0;
            Bounced_ZV := -Bounce_Factor * Character.Velocity (GL.Z);
         else
            Bounced_ZV := Character.Velocity (GL.Z);
         end if;

         --  double check that we are not bouncing INTO a new wall
         Bounced_Pos := Character.World_Pos +
           Single (Seconds) * Effective_Velocity;
         Max_Bounced_Height := Bounced_Pos (GL.Y) +
           Char_Mount_Wall_Max_Height;
         Bounced_Height :=
           Get_Min_Height_For_Character (Character, 0, Bounced_Pos, -0.2);
         if Bounced_Height <= Max_Bounced_Height then
            Character.Velocity (GL.X) := Bounced_XV;
            Character.Velocity (GL.Z) := Bounced_ZV;
         end if;
      end if;

   end Update_Character_Position;

   --  -------------------------------------------------------------------------

   procedure Update_Character_Stairs (Character          : in out Barbarian_Character;
                                      Effective_Velocity : in out Singles.Vector3) is
      use Singles;
      Ramp_Glue_Threshold : constant Single := 0.6;  --  100 mm
      Ramp_Boost          : Vector3 := (1.0, 1.0, 1.0);
      Ramp_Height         : Single;
      Num_D_Sixes         : Integer;
      Damage              : Integer;
      Max_Damage          : Integer;
      Damaged             : Boolean;
      Ramp_Facing         : Standard.Character;
   begin
      if Manifold.Is_Ramp (Character.Map) then
         Ramp_Height := Tiles_Manager.Get_Tile_Height
           (Character.World_Pos (GL.X), Character.World_Pos (GL.Z), True, True);
         --  If character is just above the ramp then glue it to the ramp so
         --  that it can walk rather than fall down the stairs.
         --  This should be small enough that it's not noticeably magnetic.
         --  For example when falling a long way onto stairs.
         --  BUT this has to be quite high because character height is based
         --  on a radius which might be getting the height at the TOP of the
         --  stairs as base height.
         if Ramp_Height + Ramp_Glue_Threshold > Character.World_Pos (GL.Y) and
           Character.Desired_Velocity (GL.Y) <= 0.01 then
            Character.World_Pos (GL.Y) := Ramp_Height;
            --  check if fallen too far
            if Character.Distance_Fallen > Fall_Dmage_Start_Height then
               Num_D_Sixes := Integer (Character.Distance_Fallen / 4.0);
               Max_Damage := 50 * Num_D_Sixes;
               Damage := Natural (Maths.Random_Float) * Max_Damage +
                 Num_D_Sixes;
               if Character.Distance_Fallen > Fall_Damage_Death_Height then
                  Damage := 1000;
               end if;
               Audio.Play_Sound (Fall_Damage_Sound_File, True);
               Damaged :=
                 Damage_Character (Character, Damage, 0, 0.0, Fall_Wt);
            end if;
            Character.Distance_Fallen := 0.0;
            Character.Is_On_Ground := True;

            Ramp_Facing := Tiles_Manager.Get_Facing (Character.Map);
            case Ramp_Facing is
               when 'N' =>
                  if Effective_Velocity (GL.Z) > 0.0 then
                     Ramp_Boost (GL.Z) := 1.25;
                  else
                     Ramp_Boost (GL.Z) := 0.75;
                  end if;
               when 'S' =>
                  if Effective_Velocity (GL.Z) < 0.0 then
                     Ramp_Boost (GL.Z) := 1.25;
                  else
                     Ramp_Boost (GL.Z) := 0.75;
                  end if;
               when 'W' =>
                  if Effective_Velocity (GL.X) > 0.0 then
                     Ramp_Boost (GL.X) := 1.25;
                  else
                     Ramp_Boost (GL.X) := 0.75;
                  end if;
               when 'E' =>
                  if Effective_Velocity (GL.X) < 0.0 then
                     Ramp_Boost (GL.X) := 1.25;
                  else
                     Ramp_Boost (GL.X) := 0.75;
                  end if;
               when others => null;
            end case;
            for index in GL.Index_3D'range loop
               Effective_Velocity (index) :=
                 Effective_Velocity (index) * Ramp_Boost (index);
            end loop;
         end if; --  if Ramp_Height + Ramp_Glue_Threshold
      end if;  --  Manifold.Is_Ramp

   end Update_Character_Stairs;

   --  -------------------------------------------------------------------------

   procedure Update_Decay (Character : in out Barbarian_Character;
                           Seconds   : Float) is
   begin
      Character.Update_Decay := Seconds;
   end Update_Decay;

   --  -------------------------------------------------------------------------

   procedure  Update_Desired_Velocity (Character : in out Barbarian_Character) is
      use Singles;
      S_I   : constant Positive := Character.Specs_Index;
      aSpec : constant Spec_Data := Specs_Manager.Get_Spec (S_I);
      Speed : Single := Single (aSpec.Move_Speed_MPS);
   begin
      if abs (Character.Desired_Direction (GL.Z)) +
      abs (Character.Desired_Direction (GL.X)) > 0.0 then
         Speed := Speed + 0.707106781;
      end if;
      Character.Desired_Velocity := Speed * Character.Desired_Direction;
      if abs (Character.Desired_Direction (GL.Z))  > 0.0 or
      abs (Character.Desired_Direction (GL.X)) > 0.0 then
         if not Character.Is_Attacking then
            Set_Walk_Animation (Character);
         end if;
         Character.Heading_Deg :=
           Maths.Direction_To_Heading (Character.Desired_Direction);
         Sprite_Renderer.Set_Sprite_Heading
           (Character.Sprite_Index, Character.Heading_Deg);
      end if;

   end Update_Desired_Velocity;

   --  -------------------------------------------------------------------------

   procedure Update_Player (Window    : in out Input_Callback.Barbarian_Window;
                            Player_ID : Integer; Seconds : Float) is
      use GL.Types.Singles;
      use Maths;
      use Single_Math_Functions;
      use Input_Handler;
      use Character_Controller.Support;
      Character      : Barbarian_Character := Characters.Element (Player_ID);
      Lamp_Freq      : constant Single := 2.0;
      Lamp_Amplitude : constant Single := 0.25;
      Lamp_Position  : Vector3 := Character.World_Pos;
      Lamp_Time      : Single := 0.0;
      Rot            : constant Matrix4 :=
                         Rotate_Y_Degree (Identity4, Character.Heading_Deg);
      Torch_Pos      : constant Vector3 := Character.World_Pos +
                         To_Vector3 (Rot * (-0.25, 1.0, -0.4, 1.0));
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
         Check_End_Of_Level_Stairs  (Character, Seconds,
                                     GL_Utils.Level_Par_Time);
         Update_Character_Motion (Character, Seconds);
      end if;

      if Character.Is_Moving or Character.First_Update then
         Character_Controller.Support.Update_Camera_Position (Character);
         Character_Controller.Support.Grab_Nearby_Gold
           (Character, Player_ID);
         --  check if moved into tavern
         if  Prop_Renderer.Check_Tile_For_Property
           (Character.Map, Prop_Renderer_Support.Tavern_Prop) then
            Character.Is_Drinking_In_Tavern :=
              not Character.Is_Drinking_In_Tavern;
            if Character.Is_Drinking_In_Tavern then
               Audio.Play_Sound (Hello_Friend_Sound_File, False);
            end if;
         end if;
         --  check_tile_for_javelin_stall
         if  Prop_Renderer.Check_Tile_For_Property
           (Character.Map, Prop_Renderer_Support.Jav_Stand_Prop) then
            Character.Is_Buying_Javelins := not Character.Is_Buying_Javelins;
            if Character.Is_Buying_Javelins then
               Audio.Play_Sound (Hello_Friend_Sound_File, False);
            end if;
         end if;
      end if;

      Character.First_Update := False;
      Lamp_Time := Lamp_Time + Lamp_Freq * Single (Seconds);
      Lamp_Position (GL.X) :=
        Lamp_Position (GL.X) + Sin (Lamp_Time) * Lamp_Amplitude;
      Lamp_Position (GL.Z) :=
        Lamp_Position (GL.Z) + Sin (2.0 * Lamp_Time) * Lamp_Amplitude;
      Manifold.Update_Manifold_Dynamic_Light
        (Lamp_Position, (0.9, 0.7, 0.5), (1.0, 0.8, 0.0), Torch_Light_Range);
      Prop_Renderer.Update_Dynamic_Lights
        (Lamp_Position, (0.9, 0.7, 0.5), (1.0, 0.8, 0.0), Torch_Light_Range);
      Sprite_Renderer.Update_Dynamic_Light
        (Lamp_Position, (0.9, 0.7, 0.5), (1.0, 0.8, 0.0), Torch_Light_Range);

      -- rotate around player
      Particle_System.Set_Particle_System_Position
        (Torch_Particles_Index (Player_ID), Torch_Pos);
      Update_Character_Animation (1, Seconds);

   exception
      when others =>
         Put_Line ("Character_Controller.Update_Player exception");
         raise;
   end Update_Player;

   --  -------------------------------------------------------------------------

end Character_Controller;
