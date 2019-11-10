
with Ada.Containers.Generic_Array_Sort;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types.Colors;
with Maths;
--  with Utilities;

package body Particle_System is
   use Ada.Containers;

   type Particle is record
      Position        : Singles.Vector3;
      Speed           : Singles.Vector3;
      Diameter        : Single;
      Angle           : Single;
      Weight          : Single;
      Colour          : GL.Types.Colors.Color;
      Life            : Single := -1.0;
      Camera_Distance : Single := -1.0;
   end record;

   type Particle_Array is array (Int range <>) of Particle;

   function "<" (Left, Right : Particle) return Boolean is
   begin
      return Maths.Length (Left.Position) < Maths.Length (Right.Position);
   end "<";

   procedure Sort_Particles is new Generic_Array_Sort (Index_Type   => GL.Types.Int,
                                                       Element_Type => Particle,
                                                       Array_Type   => Particle_Array,
                                                       "<"          => "<");

   Particle_Container : Particle_Array (1 .. Max_Particles);
   Position_Size_Data : Position_Size_Data_Array (1 .. 4 * Max_Particles);
   Colour_Data        : Colour_Data_Array (1 .. 4 * Max_Particles);

   Single_Bytes       : constant Int := Single'Size / 8;
   Last_Used_Particle : Int := 1;

   --  -------------------------------------------------------------------------

   function Get_Colour_Data return Colour_Data_Array is
   begin
      return Colour_Data;
   end Get_Colour_Data;

   --  -------------------------------------------------------------------------

   function Get_Position_Size_Data return Position_Size_Data_Array is
   begin
      return Position_Size_Data;
   end Get_Position_Size_Data;

   --  -------------------------------------------------------------------------

   procedure Init (Position_Buffer, Colour_Buffer : in out GL.Objects.Buffers.Buffer) is
      use GL.Objects.Buffers;
   begin
      Position_Buffer.Initialize_Id;
      Array_Buffer.Bind (Position_Buffer);
      GL.Objects.Buffers.Allocate (Array_Buffer, Long (4 * Max_Particles * Single_Bytes), Stream_Draw);

      Colour_Buffer.Initialize_Id;
      Array_Buffer.Bind (Colour_Buffer);
      GL.Objects.Buffers.Allocate (Array_Buffer, Long (4 * Max_Particles * Single_Bytes), Stream_Draw);

    exception
        when others =>
            Put_Line ("An exception occurred in Particle_System.Init.");
            raise;
   end Init;

   --  -------------------------------------------------------------------------

   function Find_Unused_Particle return Int is
      Unused : Int := 0; -- If all particles are taken override the first one
   begin
      for index in Last_Used_Particle .. Max_Particles loop
         if Particle_Container (index).Life < 0.0 then
            Last_Used_Particle := index;
            Unused  := index;
         end if;
      end loop;

      for index in 1 .. Last_Used_Particle loop
         if Particle_Container (index).Life < 0.0 then
            Last_Used_Particle := index;
            Unused  := index;
         end if;
      end loop;

      return Unused;

    exception
        when others =>
            Put_Line ("An exception occurred in Particle_System.Find_Unused_Particle.");
            raise;
   end Find_Unused_Particle;

   --  -------------------------------------------------------------------------

   procedure Update_Particles (Delta_Time : Single) is
      use GL.Types.Colors;
      use GL.Types.Singles;
      New_Particles    : Int := 10000 * Int (Delta_Time);
      Particle_Index   : Int;
      Particle_Count   : Int := 0;
      aParticle        : Particle;
      Spread           : constant Single := 1.5;
      Main_Direction   : constant Vector3 := (0.0, 10.0, 0.0);
      Random_Direction : Vector3;
   begin
      if New_Particles > 160 then
         New_Particles := 160;
      end if;
      for index in 1 .. New_Particles loop
         Particle_Index := Find_Unused_Particle;
         Random_Direction := (Maths.Random_Float,
                              Maths.Random_Float,
                              Maths.Random_Float);
         Particle_Container (Particle_Index).Life := 5.0;
         Particle_Container (Particle_Index).Position := (0.0, 0.0, -20.0);
         Particle_Container (Particle_Index).Speed :=
           Main_Direction + Spread * Random_Direction;
         Particle_Container (Particle_Index).Colour :=
           (Maths.Random_Float / 3.0,
            Maths.Random_Float / 3.0,
            Maths.Random_Float / 3.0,
            Maths.Random_Float / 3.0);
         Particle_Container (Particle_Index).Diameter :=
           Maths.Random_Float / 2.0 + 0.1;

         for index in 1 .. Max_Particles loop
            aParticle := Particle_Container (index);
            if aParticle.Life > 0.0 then
               Particle_Count := Particle_Count + 1;
               aParticle.Life := aParticle.Life - Delta_Time;
               if aParticle.Life > 0.0 then
                  aParticle.Speed :=
                    aParticle.Speed + 0.5 * Delta_Time * (0.0, -9.81, 0.0);
                  aParticle.Position :=
                    aParticle.Position + Delta_Time * aParticle.Speed;
                  aParticle.Camera_Distance :=
                    Maths.Length (aParticle.Position) - aParticle.Camera_Distance;

                  Position_Size_Data (4 * Particle_Count) := aParticle.Position (GL.X);
                  Position_Size_Data (4 * Particle_Count + 1) := aParticle.Position (GL.Y);
                  Position_Size_Data (4 * Particle_Count + 2) := aParticle.Position (GL.Z);
                  Position_Size_Data (4 * Particle_Count + 3) := aParticle.Diameter;

                  Colour_Data (4 * Particle_Count) := aParticle.Colour (R);
                  Colour_Data (4 * Particle_Count + 1) := aParticle.Colour (G);
                  Colour_Data (4 * Particle_Count + 2) := aParticle.Colour (B);
                  Colour_Data (4 * Particle_Count + 3) := aParticle.Colour (A);
               else
                  --  Particles that just died will be put
                  --  at the end of the buffer in Sort_Particles
                  aParticle.Camera_Distance := -1.0;
               end if;
            end if;
         end loop;
      end loop;

      Sort_Particles (Particle_Container);

    exception
        when others =>
            Put_Line ("An exception occurred in Particle_System.Update_Particles.");
            raise;
   end Update_Particles;

   --  -------------------------------------------------------------------------

end Particle_System;
