
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with Projectile_Manager;
with Specs_Manager;

package Character_Controller is

   type Barbarian_Character is private;

   Char_Mount_Wall_Max_Height : constant GL.Types.Single := 1.0;  --  0.75F --  Was 1.0
   Max_Inventory_Javelins     : constant GL.Types.Int := 4;
   Gold_Current               : Integer := 0;
   Gold_Max                   : Integer := 0;
   Total_Treasure_Found       : Integer := 0;
   Kills_Current              : Integer := 0;
   Kills_Max                  : Integer := 0;

   Character_Controller_Exception : Exception;

   procedure Init;
   procedure Load_Characters (Input_File : File_Type; Editor_Mode : Boolean);
   function Update_Characters (Seconds : Float) return Boolean;

private
   use GL.Types;
   use Specs_Manager;
   type Attached_Particle_Systems_Array is array (GL.Types.Int range <>)
     of Integer;
   type Barbarian_Character is record
      Specs_Index                                                     : Natural := 0;
      World_Pos                                                       : Singles.Vector3 := (0.0, 0.0, 0.0);
      Velocity                                                        : Singles.Vector3 := (0.0, 0.0, 0.0);
      Desired_Velocity                                                : Singles.Vector3 := (0.0, 0.0, 0.0);
      Desired_Direction                                               : Singles.Vector3 := (0.0, 0.0, 0.0);
      Current_Anim_Frame_Time                                         : Integer := 0;
      Attack_Countdown                                                : Integer := 0;
      Update_Decay                                                    : Integer := 0;
      Alert_Cooldown_Sec                                              : Integer := 0;
      Skull_Countdown                                                 : Integer := 0;
      Fireball_Countdown                                              : Integer := 0;
      Teleport_Countdown                                              : Integer := 0;
      Heading_Deg                                                     : Float := 0.0;
      Distance_Fallen                                                 : Float := 0.0;
      Particle_System_Ids                                             : Attached_Particle_Systems_Array
        (1 .. Specs_Manager.Max_Particle_Systems_Attached_To_Character) :=
                                                                          (others => 0);
      Map_X                                                           : Int := 0;
      Map_Y                                                           : Int := 0;
      Destination_Tile_X                                              : Int := -1;
      Destination_Tile_Y                                              : Int := -1;
      Current_Weapon                                                  : Specs_Manager.Weapon_Type :=
                                                                          Specs_Manager.Na_Wt;
      Sprite_Index                                                    : Integer := 0;
      Current_Anim                                                    : Integer := 0;
      Current_Anim_Frame                                              : Integer := 0;
      Next_Attack_Event                                               : Integer := 0;
      Current_Health                                                  : Integer := 0;
      Number_Particle_Systems_Attached                                : Integer := 0;
      Javelin_Count                                                   : Integer := 0;
      --  States
      First_Update                                                    : Boolean := True;
      Is_Alive                                                        : Boolean := True;
      Death_Was_Counted                                               : Boolean := False;
      Is_On_Ground                                                    : Boolean := True;
      Is_Attacking                                                    : Boolean := False;
      Is_Chasing_Enemy                                                : Boolean := False;
      Has_Pathing_Destination                                         : Boolean := False;
      Is_Walking                                                      : Boolean := False;
      Is_Moving                                                       : Boolean := False;
      Is_Drinking_In_Tavern                                           : Boolean := False;
      Is_Buying_Javelins                                              : Boolean := False;
      Has_Hammer                                                      : Boolean := False;
      No_Save                                                         : Boolean := False;
      --  the character should be updated in a next simulation step
      --  e.g.. is close to player or is falling or something
      Needs_Update                                                    : Boolean := False;
   end record;

   package Character_Package is new Ada.Containers.Vectors
     (Positive, Barbarian_Character);
   type Character_List is new Character_Package.Vector with null Record;

   type Attack_Events_Array is array
     (1 .. Max_Weapons, 1 .. Max_Attack_Events) of Attack_Event;
   type Event_Count_Array is array (1 .. Max_Weapons) of Integer;
   type Animations_Array is array
     (1 .. Max_Animations, 1 .. Max_Anim_Frames) of
     Specs_Manager.Anim_Frame;
   type Anim_Count_Array is array (1 .. Max_Animations) of Integer;
   type Weapon_Param_Array is array (1 .. Max_Weapons) of Float;
   type Unbounded_String_Array is array (Integer range <>) of Unbounded_String;

   type Specs is record
      Attack_Events                : Attack_Events_Array;
      Attack_Event_Count           : Event_Count_Array := (others => 0);
      Animations                   : Animations_Array;
      Anim_Frame_Count             : Anim_Count_Array := (others => 0);
      Name                         : Unbounded_String := To_Unbounded_String ("");
      Weapon_Attack_Time           : Weapon_Param_Array;
      Attack_Range_M               : Weapon_Param_Array;
      Move_Speed_Mps               : Float := 0.0;
      Height                       : Float := 0.0;  -- Metres
      Width_Radius                 : Float := 1.0;
      Sprite_Offset_Adj            : Float := 0.0;
      Atlas_Rows                   : Int := 0;
      Atlas_Cols                   : Int := 0;
      Sight_Range_Tiles            : Integer := 0;
      Initial_Health               : Integer := 0;
      Default_Weapon               : Weapon_Type := Na_Wt;
      Projectile                   : Projectile_Manager.Projectile_Type :=
                                       Projectile_Manager.Na_Proj_Type;
      Decapitated_Head_Prop_Script : Positive := 1;
      Team_Id                      : Positive := 1;
      Tx_On_Death                  : Integer := -1;
      Land_Move                    : Boolean := False; --  False for Sewerman
      Uatlas_Diffuse_Id            : Positive := 1;
      Atlas_Specular_Id            : Positive := 1;
      --  Sounds
      Alert_Sound_File_Name        : Unbounded_String := To_Unbounded_String ("");
      Attack_Sound_File_Name           : Unbounded_String_Array
        (1 .. Integer (Max_Weapons)) := (others => To_Unbounded_String (""));
      Hurt_Sound_File_Name         : Unbounded_String := To_Unbounded_String ("");
      Death_Sound_File_Name        : Unbounded_String := To_Unbounded_String ("");
   end record;

end Character_Controller;
