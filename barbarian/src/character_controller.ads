
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with Maths;

with Input_Callback;
with Projectile_Manager;
with Specs_Manager;

package Character_Controller is
   use GL.Types;

   type Barbarian_Character is private;

   Char_Mount_Wall_Max_Height : constant GL.Types.Single := 1.0;  --  0.75F --  Was 1.0
   Max_Inventory_Javelins     : constant Natural := 4;
   Gold_Current               : Integer := 0;
   Gold_Max                   : Integer := 0;
   Total_Treasure_Found       : Integer := 0;

   Character_Controller_Exception : Exception;

   function Alert_Cooldown (Character : Barbarian_Character) return Float;
   function Alive (Character : Barbarian_Character) return Boolean;
   function Attacking (Character : Barbarian_Character) return Boolean;
   function Chasing_Enemy (Character : Barbarian_Character) return Boolean;
   function Current_Health (Character_ID : Positive) return Integer;
   function Current_Kills return Integer;
   function Current_Weapon (Character : Barbarian_Character)
                            return Specs_Manager.Weapon_Type;
   function Damage_All_Near
     (Self_Id        : Positive; World_Pos : Singles.Vector3;
      Damage_Range   : Single; Throw_Back_Mps : Single;  Exclude_Id : Positive;
      Weapon         : Specs_Manager.Weapon_Type) return Natural;
   function Fireball_Countdown (Character : Barbarian_Character) return Float;
   function Get_Character (Character_ID : Positive) return Barbarian_Character;
   function Get_Character_Position (Character_ID : Positive)
                                    return Singles.Vector3;
   function Heading (Character : Barbarian_Character) return Maths.Degree;
   procedure Init;
   function On_Ground (Character : Barbarian_Character) return Boolean;
   function Javelin_Count (Character_ID : Positive) return Natural;
   function Javelin_Count (Character : in out Barbarian_Character) return Natural;
   procedure Load_Characters (Input_File : File_Type; Editor_Mode : Boolean);
   function Map  (Character_ID : Positive) return Ints.Vector2;
   function Map  (Character : Barbarian_Character) return Ints.Vector2;
   function Max_Kills return Integer;
   function Position (Character_ID : Positive) return Singles.Vector3;
   function Position (Character : Barbarian_Character) return Singles.Vector3;
   function Skull_Countdown (Character : Barbarian_Character) return Float;
   procedure Set_Alert_Cooldown (Character : in out Barbarian_Character;
                                 Value : Float);
   procedure Set_Chasing_Enemy (Character : in out Barbarian_Character;
                                State : Boolean);
   procedure Set_Desired_Direction (Character : in out Barbarian_Character;
                                     Direction : Singles.Vector3);
   procedure Set_Fireball_Countdown (Character : in out Barbarian_Character;
                                     Seconds : Float);
   procedure Set_Has_Pathing_Destination
      (Character : in out Barbarian_Character; State : Boolean);
   procedure Set_Heading (Character : in out Barbarian_Character;
                           Heading : Maths.Degree);
   procedure Set_Skull_Countdown (Character : in out Barbarian_Character;
                                  Seconds : Float);
   procedure Set_Teleport_Countdown (Character : in out Barbarian_Character;
                                     Seconds : Float);
   function Spec_Index (Character : Barbarian_Character) return Positive;
   function Spec_Index (Character_ID : Positive) return Positive;
   function Sprite_Index (Character : Barbarian_Character) return Positive;
   procedure Start_Attack (Character : in out Barbarian_Character);
   function Teleport_Countdown (Character : Barbarian_Character) return Float;
   procedure Update_Characters (Window     : in out Input_Callback.Barbarian_Window;
                                Seconds : Float);
   procedure Update_Decay (Character : in out Barbarian_Character;
                           Seconds : Float);

private
   use GL.Types;
   use Specs_Manager;
   type Attached_Particle_Systems_Array is array (Integer range <>)
     of Integer;
   type Barbarian_Character is record
      Specs_Index              : Natural := 0;
      World_Pos                : Singles.Vector3 := (0.0, 0.0, 0.0);
      Velocity                 : Singles.Vector3 := (0.0, 0.0, 0.0);
      Desired_Velocity         : Singles.Vector3 := (0.0, 0.0, 0.0);
      Desired_Direction        : Singles.Vector3 := (0.0, 0.0, 0.0);
      Current_Anim_Frame_Time  : Float := 0.0;
      Attack_Countdown         : Float := 0.0;
      Update_Decay             : Float := 0.0;
      Alert_Cooldown_Sec       : Float := 0.0;
      Skull_Countdown          : Float := 0.0;
      Fireball_Countdown       : Float := 0.0;
      Teleport_Countdown       : Float := 0.0;
      Heading_Deg              : Maths.Degree := 0.0;
      Distance_Fallen          : Float := 0.0;
      Particle_System_Ids      : Attached_Particle_Systems_Array
        (1 .. Specs_Manager.Max_Particle_Systems_Attached_To_Character)
        := (others => 0);
      Map                      : Ints.Vector2 := (0, 0);
      Destination_Tile         : Ints.Vector2 := (-1, -1);
      Current_Weapon           : Specs_Manager.Weapon_Type :=
                                   Specs_Manager.Na_Wt;
      Sprite_Index             : Natural := 0;
      Current_Anim             : Integer := 0;
      Current_Anim_Frame       : Integer := 0;
      Next_Attack_Event        : Integer := 0;
      Current_Health           : Integer := 0;
      Number_Particle_Systems_Attached : Integer := 0;
      Javelin_Count            : Integer := 0;
      --  States
      First_Update             : Boolean := True;
      Is_Alive                 : Boolean := True;
      Death_Was_Counted        : Boolean := False;
      Is_On_Ground             : Boolean := True;
      Is_Attacking             : Boolean := False;
      Is_Chasing_Enemy         : Boolean := False;
      Has_Pathing_Destination  : Boolean := False;
      Is_Walking               : Boolean := False;
      Is_Moving                : Boolean := False;
      Is_Drinking_In_Tavern    : Boolean := False;
      Is_Buying_Javelins       : Boolean := False;
      Has_Hammer               : Boolean := False;
      No_Save                  : Boolean := False;
      --  the character should be updated in a next simulation step
      --  e.g.. is close to player or is falling or something
      Needs_Update             : Boolean := False;
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

end Character_Controller;
