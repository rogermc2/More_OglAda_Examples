
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

with GL.Types; use GL.Types;

with Maths;

with GL_Maths;
with Particle_System_Manager;

package Particle_System is
   use Singles;

   type Particle_System is private;
   type Particle_Systems_List is private;

   Particle_System_Exception : Exception;

   function Create_Particle_System
     (Script_Name : String; Start_Now, Always_Update, Always_Draw : Boolean)
      return Positive;
   function Get_Particle_Script (Script_Index : Positive)
                                  return Particle_System_Manager.Particle_Script;
   function Get_Particle_System
     (System_ID : Positive; theSystem : in out Particle_System) return Boolean;
   function Has_Particle_System (System_ID : Positive) return Boolean;
   procedure Init;
   function Is_Running (System_ID : Positive) return Boolean;
   function Length return Positive;

   procedure Render_Particle_Systems (Seconds : Single);
   function Script_Index (System_ID : Positive) return Positive;
   procedure Set_Particle_System_Heading (System_ID : Positive;
                                          Heading : Maths.Degree);
   procedure Set_Particle_System_Position (System_ID : Positive;
                                           Emitter_World_Pos : Singles.Vector3);
   procedure Start_Particle_System (System_ID : Positive);
   procedure Stop_Particle_System (System_ID : Positive);
   procedure Stop_Particle_Systems;
   procedure Update_Particle_System (System_ID : Positive;
                                     Seconds : Single);

private
   package Singles_Package is new
     Ada.Containers.Vectors (Positive, Single);
   type Ages_List is new Singles_Package.Vector with null record;

   package Singles_Vector3_Package is new
     Ada.Containers.Vectors (Positive, Singles.Vector3);
   type Positions_List is new Singles_Vector3_Package.Vector with null record;

   type Particle_System is record
      -- For transforms on cpu
      Particle_Positions : Positions_List;
      Particle_Ages      : Ages_List;
      -- Used to rotate emitter
      Rot_Mat            : Singles.Matrix4 := Singles.Identity4;
      Emitter_World_Pos  : Singles.Vector3 := Maths.Vec3_0;
      System_Age         : Float := 0.0;
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
