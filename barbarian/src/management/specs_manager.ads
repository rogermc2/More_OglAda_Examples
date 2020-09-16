
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Objects.Textures;
with GL.Types; use GL.Types;

with Projectile_Manager;

package Specs_Manager is

    Max_Weapons       : constant GL.Types.Int := 9;
    Max_Animations    : constant GL.Types.Int := 32;
    Max_Attack_Events : constant GL.Types.Int := 32;
    Max_Particle_Systems_Attached_To_Character : constant GL.Types.Int :=  8;

    type Unbounded_String_Array is array (GL.Types.Int range <>)
      of Unbounded_String;

    type Weapon_Type is (Sword_Wt,
                         Missile_Wt,  --  Used for javelin, arrow and green fireballs
                         Hammer_Wt,
                         Skull_Wt,    --  Was originally for deleted the crossbow trap
                         Teleport_Wt, Pillar_Wt, Boulder_Wt, Fall_Wt,
                         Na_Wt);      --  Everything else

    type Anim_Frame is record
        Atlas_Index : Int := 0;
        Seconds     : Single := 0.0;
    end record;

    type Attack_Event is record
        Location        : Singles.Vector3 := (0.0, 0.0, 0.0);
        Time_Sec        : Single := 0.0;
        Radius          : Single := 0.0;
        Throw_Back_MPS  : Single := 0.0;
        Max_Damage      : Int := 0;
        Min_Damage      : Int := 0;
    end record;

    package Anim_Frame_Package_1D is new Ada.Containers.Vectors
      (Positive, Anim_Frame);
    type Anim_Frame_List is new Anim_Frame_Package_1D.Vector with null Record;

    package Anim_Frame_Package is new Ada.Containers.Vectors
      (Positive, Anim_Frame_List);
    type Anim_Frame_Array is new Anim_Frame_Package.Vector with null Record;

    package Attack_Events_Package is new Ada.Containers.Doubly_Linked_Lists
      (Attack_Event);
    type Attack_Events_List is new Attack_Events_Package.List with null Record;

    type Weapon_Array is array
      (Weapon_Type range Weapon_Type'First .. Weapon_Type'Last) of Float;

    type Attack_Events_Array is array
      (Weapon_Type range Weapon_Type'First .. Weapon_Type'Last) of Attack_Events_List;

    type Event_Count_Array is array
      (Weapon_Type range Weapon_Type'First .. Weapon_Type'Last) of Integer;

    type Spec_Data is record
        Attack_Events           : Attack_Events_Array;
        Attack_Event_Count      : Event_Count_Array := (others => 0);
        Animations              : Anim_Frame_Array;
        Animation_Frame_Count   : Int_Array (1 .. Max_Animations) :=
                                    (others => 0);
        File_Name               : Unbounded_String := To_Unbounded_String ("");
        Name                    : Unbounded_String := To_Unbounded_String ("");
        Weapon_Attack_Time      : Weapon_Array := (others => 0.0);
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
        Atlas_Diffuse_ID        : GL.Objects.Textures.Texture;
        Atlas_Specular_ID       : GL.Objects.Textures.Texture;
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
    type Specs_List is new Specs_Package.Vector with null Record;

    Specs_Exception : Exception;

    procedure Clear_Specs;
    function Get_Script_Index (File_Name : String) return Integer;
    procedure Load_Specs_File (File_Name : String);

end Specs_Manager;
