
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

with GL.Types; use GL.Types;

with Maths;

with Particle_System_Manager;

package Particle_System is
    use Singles;

    type Particle_System is private;
    type Particle_Systems_List is private;

    function Create_Particle_System
      (Script_Name : String; Start_Now, Always_Update, Always_Draw : Boolean)
       return Integer;
    procedure Init;

private
    package Ages_Package is new
      Ada.Containers.Doubly_Linked_Lists (Float);
    type Ages_List is new Ages_Package.List with null record;

    type Particle_System is record
    -- For transforms on cpu
	Particle_Positions : Particle_System_Manager.Vec3_List;
	Particle_Ages      : Ages_List;
	-- Used to rotate emitter
	Rot_Mat            : Singles.Vector4 := Maths.Vec4_0;
	Emitter_World_Pos  : Singles.Vector3 := Maths.Vec3_0;
	System_Age         : Integer := 0;
	Script_Index       : Integer := 0;
	Is_Running         : Boolean := False;
	-- If it should skip visibility test
	Always_Draw        : Boolean := False;
	-- If it should keep updating even when off-screen
	Always_Update      : Boolean := False;
    end record;

    package Systems_Package is new
      Ada.Containers.Vectors (Positive, Particle_System);
    type Particle_Systems_List is new Systems_Package.Vector with null record;

end Particle_System;
