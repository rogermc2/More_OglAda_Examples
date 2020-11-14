
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Maths;

with Audio;
with Batch_Manager;
with Camera;
with Character_Map;
with Event_Controller;
with FB_Effects;
with Game_Utils;
with GUI;
with GUI_Level_Chooser;
with Particle_System;
with Particle_System_Manager;
with Prop_Renderer;
with Tiles_Manager;

package body Character_Controller is
   Max_Blood_Fountains       : constant Integer := 5;
   Max_Blood_Damage_Emitters : constant Integer := 5;

   type Weapon_Type is (Sword_Wt, Missile_Wt, --  used for javelin, arrow, green fireballs
                        Hammer_Wt, Skull_Wt, Teleport_Wt, Pillar_Wt, Boulder_Wt,
                        Fall_Wt, Na_Wt); --  everything else, n/a.
   pragma Ordered (Weapon_Type);

   --      type Integer_Array is array (Integer range <>) of Integer;
   type Character_Data is record
      Script_File   : Unbounded_String := To_Unbounded_String ("");
      U             : GL.Types.Int := 0;
      V             : GL.Types.Int := 0;
      H             : Float := 0.0;
   end record;
   Current_Blood_Fountain       : Integer;
   Current_Blood_Damage_Emitter : Integer;
   Hammer_Hit_Armour_Sound : constant String :=
                               "SWORD_Hit_Metal_Armor_RR3_mono.wav";
   Sword_Hit_Armour_Sound  : constant String :=
                               "HAMMER_Hit_Metal_Armor_stereo.wav";
   Characters              : Character_List;
   Character_Specs         : Specs_List;
   Torch_Light_Index       : array (1 .. 2) of Positive := (1, 1);
   Torch_Particles_Index   : array (1 .. 2) of Positive := (1, 1);

   --      Portal_Fadeout_Started  : Boolean := False;
   Characters_To_Reserve   : constant Integer := 256;
   Characters_Allocd_Count : Integer := Characters_To_Reserve;
   Character_Count         : Integer := Characters_To_Reserve;
   --      Specs_Allocd_Count      : Integer := 0;
   --      Specs_Count             : Integer := 0;
   --      Gold_Current            : constant Integer := 0;
   Kills_Current           : Integer := 0;
   --      Kills_Max               : Integer := 0;

   Teleport_From_Particles_Index  : Integer := 1;
   Teleport_To_Particles_Index    : Integer := 1;
   Blood_Fountain_Particles_Index : array (1 ..Max_Blood_Fountains) of Integer := (others => -1);
   Blood_Damage_Particles_Index   : array (1 ..Max_Blood_Damage_Emitters) of Integer := (others => -1);
   Bfparts_Last_Attached_To : array (1 .. Max_Blood_Fountains) of Integer := (others => -1);
   Bdparts_Last_Attached_To : array (1 .. Max_Blood_Damage_Emitters) of Integer := (others => -1);

   function Damage_Character (Char_ID           : Positive; Doer_ID  : Natural; Angle : Maths.Degree;
                              Damage            : Int; Weapon : Weapon_Type) return Boolean;
   procedure Detach_Particle_System_From_Character (Char_Idx, Partsys_Idx : Positive);
   --      function Is_Character_Valid (Char_Index : Integer) return Boolean;
   function Is_Close_Enough (Character    : Barbarian_Character;
                             World_Pos    : Singles.Vector3;
                             Height, Dist : Single) return Boolean;
   --      function Is_Spec_Valid (Spec_Index : Integer) return Boolean;
   procedure Launch_Decapitated_Head (Character : Barbarian_Character;
                                      WT        : Weapon_Type);
   procedure Set_Character_Defaults (aCharacter : in out Barbarian_Character);
   procedure Spray_Screen_Check (Character : Barbarian_Character;
                                 World_Pos : Singles.Vector3);

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
         Game_Utils.Game_Log (
                              "Character_Controller.Attach_Particle_System_To_Character " &
                                "WARNING: no free particle slot found in character - all in use. " &
                                "overwriting... Char_ID: " & Integer'Image (Char_ID));
      end if;

      Character.Particle_System_Ids (Looping_Index) := Particle_System_ID;
      Characters.Replace_Element (Char_ID, Character);

   end Attach_Particle_System_To_Character;

   --  -------------------------------------------------------------------------

   procedure Create_Character (Source       : Character_Data;
                               theCharacter : in out Barbarian_Character) is
   begin
      if Source.Script_File = "" then
         raise Character_Controller_Exception with
           "Character_Controller.Create_Character, no script file name.";
      end if;

      if Tiles_Manager.Is_Tile_Valid (Source.U, Source.V) then
         Game_Utils.Game_Log ("Character_Controller.Create_Character creating character from " &
                                To_String (Source.Script_File));
         Set_Character_Defaults (theCharacter);
         theCharacter.Heading_Deg := Source.H;
         theCharacter.Map_X := Source.U;
         theCharacter.Map_Y := Source.V;
         theCharacter.Specs_Index :=
           Specs_Manager.Get_Script_Index (To_String (Source.Script_File));
         Character_Count := Character_Count + 1;
         Game_Utils.Game_Log ("Character_Controller.Create_Character character created from " &
                                To_String (Source.Script_File));
      else
         raise Character_Controller_Exception with
           "Character_Controller.Create_Character, invalid tile siza" &
           Int'Image (Source.U) & "x" & Int'Image (Source.V);
      end if;
   end Create_Character;

   --  -------------------------------------------------------------------------

   function Damage_All_Near
     (Self_Id        : Positive; World_Pos : Singles.Vector3;
      Damage_Range   : Single; Damage         : Int;
      Throw_Back_Mps : Single;  Exclude_Id : Positive;
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
                        if Is_Close_Enough
                          (Character, World_Pos, Height, Damage_Range) then
                           Last_Character_Hit := Char_ID;
                        end if;
                     end if;
                  end if;
               end if;
               Next (Curs);
            end loop;
         end loop;
      end loop;

      return 0;
   end Damage_All_Near;

   --  -------------------------------------------------------------------------

   function Damage_Character (Char_ID   : Positive; Doer_ID  : Natural; Angle : Maths.Degree;
                              Damage    : Int; Weapon : Weapon_Type) return Boolean is
      use Projectile_Manager;
      Character   : Barbarian_Character := Characters.Element (Char_ID);
      Character_1 : constant Barbarian_Character := Characters.First_Element;
      Is_Sorcerer : Boolean := False;
      S_I         : constant Positive := Character.Specs_Index;
      aSpec       : constant Spec_Data := Character_Specs.Element (S_I);
      Max_Health  : Integer;
      H_Fac       : Single := 0.0;
      Result      : Boolean := False;
   begin
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
               if Character.Current_Weapon = Sword_Wt then
                  Camera.Screen_Shake (0.2, 0.5, 50.0);
                  Audio.Play_Sound (Sword_Hit_Armour_Sound, True);
               elsif Character_1.Current_Weapon = Hammer_Wt then
                  Camera.Screen_Shake (0.2, 0.5, 50.0);
                  Audio.Play_Sound (Hammer_Hit_Armour_Sound, True);
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
                     Particle_System.Stop_Particle_System
                       (Positive (Torch_Particles_Index (1)));
                     Audio.Stop_All_Boulder_Sounds;
                     GUI.Show_Defeated_Screen (True);
                  else  --  Char_ID > 1
                     if not Character.Death_Was_Counted then
                        Character.Death_Was_Counted := True;
                        if aSpec.Team_ID = 2 then
                           Kills_Current := Kills_Current + 1;
                           GUI.Set_GUI_Kills (Kills_Current);
                        end if;
                     end if;
                  end if;

                  --  stop zombified movement
                  Character.Desired_Direction := (0.0, 0.0, 0.0);
                  --  splatter_all_tiles_near (g_characters[char_idx].world_pos);
                  Detach_Particle_System_From_Character
                    (Bfparts_Last_Attached_To (Current_Blood_Fountain),
                     Blood_Fountain_Particles_Index (Current_Blood_Fountain));
                  Particle_System.Set_Particle_System_Position
                    (Blood_Fountain_Particles_Index (Current_Blood_Fountain),
                     Character.World_Pos);
                  Particle_System.Start_Particle_System
                    (Blood_Fountain_Particles_Index (Current_Blood_Fountain));
                  Attach_Particle_System_To_Character
                    (Char_ID, Blood_Fountain_Particles_Index (Current_Blood_Fountain));
                  Bfparts_Last_Attached_To (Current_Blood_Fountain) := Char_ID;
                  Current_Blood_Fountain := (Current_Blood_Fountain + 1) * Max_Blood_Fountains;
                  if Doer_ID > 0 then
                     Launch_Decapitated_Head (Character, Weapon);
                  end if;
               end if;  --  Current_Health <= 0
            end if;  --  Doer_ID = 1
         end if;
      end if;

      return Result;
   end Damage_Character;

   --  -------------------------------------------------------------------------

   procedure Detach_Particle_System_From_Character (Char_Idx, Partsys_Idx : Positive) is
   begin
      null;
   end Detach_Particle_System_From_Character;

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
   end Init;

   --  -------------------------------------------------------------------------
   -- derived from _damage_all_near part // work out if close enough
   function Is_Close_Enough (Character    : Barbarian_Character;
                             World_Pos    : Singles.Vector3;
                             Height, Dist : Single) return Boolean is
      use Singles;
      use Maths;
      C_P             : constant Singles.Vector3 := Character.World_Pos;
      Distance_Top    : constant Singles.Vector3 :=
                          World_Pos - (C_P (Gl.X), (C_P (Gl.Y) + Height),
                                       C_P (Gl.Z));
      Distance_Middle : constant Singles.Vector3 := World_Pos - (C_P (Gl.X), C_P (Gl.Y) + Height * 0.5, C_P (Gl.Z));
      Distance_Bottom : constant Singles.Vector3 := World_Pos - (C_P (Gl.X), C_P (Gl.Y), C_P (Gl.Z));
      Sq_Dist_Top     : constant Single := Length_Sq (Distance_Top);
      Sq_Dist_Middle  : constant Single := Length_Sq (Distance_Middle);
      Sq_Dist_Bottom  : constant Single := Length_Sq (Distance_Bottom);
   begin
      return Dist ** 2 >=  Min (Min (Sq_Dist_Top, Sq_Dist_Bottom),
                                Sq_Dist_Middle);
   end Is_Close_Enough;

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

   procedure Knock_Back (Character         : Barbarian_Character;
                         Self_Id, Char_ID  : Positive; Weapon : Weapon_Type;
                         World_Pos         : Singles.Vector3;
                         Throw_Back_Mps    : Single; Damage : Int) is
      use GL.Types.Singles;
      use Maths;
      Distance       : Vector3 := Character.World_Pos - World_Pos;
      Direction      : Vector3 := Maths.Normalized (Distance);
      Momentum       : Vector3 := Direction * Throw_Back_Mps;
      Y              : Single := Character.Velocity (GL.Y);
      Dir_Deg        : Degree := -Direction_To_Heading ((Direction (Gl.X), 0.0,
                                                        -Direction (Gl.Z)));
      Did_Damage     : Boolean := Damage_Character (Char_ID, Self_Id, Dir_Deg,
                                                    Damage, Weapon);

   begin
      null;
   end Knock_Back;

   --  -------------------------------------------------------------------------

   procedure Launch_Decapitated_Head (Character : Barbarian_Character;
                                      WT        : Weapon_Type) is
      use Maths;
      use GUI_Level_Chooser;
      use Projectile_Manager;
      Chance    : constant Integer := Integer (100.0 * Abs (Random_Float));
      S_I       : constant Positive := Character.Specs_Index;
      aSpec     : constant Spec_Data := Character_Specs.Element (S_I);
      DH_S_I    : constant Positive := aSpec.Decapitated_Head_Script;
      Proj_Type : Projectile_Type;
      Pos       : Singles.Vector3 := Character.World_Pos;
      Decap     : Boolean := False;
   begin
      case WT is
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
         when others => null;
      end case;

   end Launch_Decapitated_Head;

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
      --          Kills_Current := 0;
      --          Kills_Max := 0;

      if not Editor_Mode then
         Put_Line ("not Editor_Mode");
         --              GUI.Set_GUI_Gold (Gold_Current);
         --              GUI.Set_GUI_Kills (Kills_Current);
         --              GUI.Set_GUI_Javalin_Ammo (0);
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
            Field.H := Float (Int'Value (aLine (Pos1 + 1 .. L_Length)));
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

   procedure Set_Character_Defaults (aCharacter : in out Barbarian_Character) is
   begin
      aCharacter.Destination_Tile_X := -1;
      aCharacter.Destination_Tile_Y := -1;
      aCharacter.Is_Alive := True;
      aCharacter.Is_On_Ground := True;
   end Set_Character_Defaults;

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
      Num_Splats  : constant Int := 3 + Int (40.0 * Nearfac);
   begin

      if Y_Above >= 0.0 and Y_Above <= 15.0 and
        Sqxzdist <= Sqmaxxzdist then
         Add_Screen_Splats (Num_Splats);
      end if;
   end Spray_Screen_Check;

   --  -------------------------------------------------------------------------

   function Update_Characters (Seconds : Float) return Boolean is
   begin
      return False;
   end Update_Characters;

   --  -------------------------------------------------------------------------

end Character_Controller;
