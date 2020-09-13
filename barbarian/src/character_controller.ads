
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO; use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Types;

with Projectile_Manager;

package Character_Controller is

    type Anim_Frame is record
        Atlas_Index : GL.Types.Int := 0;
        Seconds     : GL.Types.Single := 0.0;
    end record;

    type Attack_Event is record
        Location        : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
        Time_Sec        : GL.Types.Single := 0.0;
        Radius          : GL.Types.Single := 0.0;
        Throw_Back_MPS  : GL.Types.Single := 0.0;
        Max_Damage      : GL.Types.Int := 0;
        Min_Damage      : GL.Types.Int := 0;
    end record;

    type Barbarian_Character is private;

    package Anim_Frame_Package is new Ada.Containers.Doubly_Linked_Lists
      (Anim_Frame);
    type Anim_Frame_List is private;
    package Attack_Events_Package is new Ada.Containers.Doubly_Linked_Lists
      (Attack_Event);
    type Attack_Events_List is private;

    Max_Weapons                : constant GL.Types.Int := 9;
    Max_Animations             : constant GL.Types.Int := 32;
    Max_Anim_Frames            : constant GL.Types.Int := 32;
    Max_Attack_Events          : constant GL.Types.Int := 32;
    Max_Particle_Systems_Attached_To_Character : constant GL.Types.Int :=  8;
    Char_Mount_Wall_Max_Height : constant GL.Types.Single := 1.0;  --  0.75F --  Was 1.0
    Max_Inventory_Javelins     : constant GL.Types.Int := 4;

    type Weapon_Type is (Sword_Wt,
                         Missile_Wt,  --  Used for javelin, arrow and green fireballs
                         Hammer_Wt,
                         Skull_Wt,    --  Was originally for deleted the crossbow trap
                         Teleport_Wt, Pillar_Wt, Boulder_Wt, Fall_Wt,
                         Na_Wt);      --  Everything else

    Character_Controller_Exception : Exception;

    procedure Init;
    procedure Load_Characters (Input_Stream : Stream_IO.Stream_Access;
                               Editor_Mode : Boolean);
    function Update_Characters (Seconds : Float) return Boolean;

private
    type Attack_Events_List is new Attack_Events_Package.List with null Record;
    type Anim_Frame_List is new Anim_Frame_Package.List with null Record;
    type Unbounded_String_Array is array (GL.Types.Int range <>)
      of Unbounded_String;
    type Attached_Particle_Systems_Array is array (GL.Types.Int range <>)
      of Integer;
    type Barbarian_Character is record
        Specs_Index             : Natural := 0;
        World_Pos               : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
        Velocity                : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
        Desired_Velocity        : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
        Desired_Direction       : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
        Current_Anim_Frame_Time : Integer := 0;
        Attack_Countdown        : Integer := 0;
        Update_Decay            : Integer := 0;
        Alert_Cooldown_Sec      : Integer := 0;
        Skull_Countdown         : Integer := 0;
        Fireball_Countdown      : Integer := 0;
        Teleport_Countdown      : Integer := 0;
        Heading_Deg             : Float := 0.0;
        Distance_Fallen         : Float := 0.0;
        Particle_System_Ids     : Attached_Particle_Systems_Array
          (1 .. Max_Particle_Systems_Attached_To_Character) := (others => 0);
        Map_X                   : Float := 0.0;
        Map_Y                   : Float := 0.0;
        Destination_Tile_X      : Integer := -1;
        Destination_Tile_Y      : Integer := -1;
        Current_Weapon          : Weapon_Type := Na_Wt;
        Sprite_Index            : Integer := 0;
        Current_Anim            : Integer := 0;
        Current_Anim_Frame      : Integer := 0;
        Next_Attack_Event       : Integer := 0;
        Current_Health          : Integer := 0;
        Number_Particle_Systems_Attached : Integer := 0;
        Javelin_Count           : Integer := 0;
        --  States
        First_Update            : Boolean := True;
        Is_Alive                : Boolean := True;
        Death_Was_Counted       : Boolean := False;
        Is_On_Ground            : Boolean := True;
        Is_Attacking            : Boolean := False;
        Is_Chasing_Enemy        : Boolean := False;
        Has_Pathing_Destination : Boolean := False;
        Is_Walking              : Boolean := False;
        Is_Moving               : Boolean := False;
        Is_Drinking_In_Tavern   : Boolean := False;
        Is_Buying_Javelins      : Boolean := False;
        Has_Hammer              : Boolean := False;
        No_Save                 : Boolean := False;
        --  the character should be updated in a next simulation step
	--  e.g.. is close to player or is falling or something
        Needs_Update            : Boolean := False;
    end record;

    package Character_Package is new Ada.Containers.Doubly_Linked_Lists
      (Barbarian_Character);
    type Character_List is new Character_Package.List with null Record;

    type Spec_Data is record
        Attack_Events           : Attack_Events_List;
        Animations              : Anim_Frame_List;
        File_Name               : Unbounded_String := To_Unbounded_String ("");
        Name                    : Unbounded_String := To_Unbounded_String ("");
        Weapon_Attack_Time      : GL.Types.Int_Array (1 .. Max_Weapons) :=
                                    (others => 0);
        Attack_Range_Metre      : GL.Types.Int_Array (1 .. Max_Weapons) :=
                                    (others => 0);
        Move_Speed_MPS          : Float := 0.0;
        Height_Metre            : Float := 0.0;
        Width_Radius            : Float := 0.0;
        Sprite_Offset_Adjust    : Float := 0.0;
        Atlas_Rows              : Integer := 0;
        Atlas_Cols              : Integer := 0;
        Sight_Range_Tiles       : Integer := 0;
        Initial_Health          : Integer := 0;
        Deafault_Weapon         : Weapon_Type := Na_Wt;
        Projectile              : Projectile_Manager.Projectile_Type :=
                                    Projectile_Manager.Na_Proj_Type;
        Decapitated_Head_Prop_Script : Integer := 0;
        Team_ID                 : Integer := 0;
        Tx_On_Death             : Integer := 0;
        Land_Move               : Boolean;  --  False for sewerman
        Atlas_Diffuse_ID        : GL.Types.UInt := 0;
        Atlas_Specular_ID       : GL.Types.UInt := 0;
        Alert_Sound_File_Name   : Unbounded_String := To_Unbounded_String ("");
        Attack_Sound_File_Name  : Unbounded_String_Array (1 .. Max_Weapons) :=
                                    (others => To_Unbounded_String (""));
        Hurt_Sound_File_Name    : Unbounded_String := To_Unbounded_String ("");
        Death_Sound_File_Name   : Unbounded_String := To_Unbounded_String ("");
        particle_system_ids     : GL.Types.Int_Array
          (1 .. Max_Particle_Systems_Attached_To_Character) := (others => 0);
        Map_X                   : Integer := 0;
        Map_Y                   : Integer := 0;
        Destination_Tile_X      : Integer := 0;
        Destination_Tile_Y      : Integer := 0;
        Current_Weapon          : Weapon_Type := Na_Wt;
        Sprite_Index            : Integer := 0;
        Current_Anim            : Integer := 0;
        Current_Anim_Frame      : Integer := 0;
        Next_Attack_Event       : Integer := 0;
        Current_Health          : Integer := 0;
        Number_Particle_Systems_Attached : Integer := 0;
        Javelin_Count           : Integer := 0;
        First_Update            : Boolean := True;
        Is_Alive                : Boolean := False;
        Death_Was_Counted       : Boolean := False;
        Is_On_Ground            : Boolean := False;
        Is_Attacking            : Boolean := False;
        Is_Chasing_Enemy        : Boolean := False;
        Has_Pathing_Destination : Boolean := False;
        Is_Walking              : Boolean := False;
    end record;

    package Specs_Package is new Ada.Containers.Vectors
      (Positive, Spec_Data);
    type Specs_List is  new Specs_Package.Vector with null Record;

end Character_Controller;
