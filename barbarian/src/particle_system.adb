
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Programs;

with Game_Utils;
with Particle_System_Shader_Manager;

package body Particle_System is

   Scripts               : Particle_System_Manager.Particle_Script_List;
   Particle_Systems      : Particle_Systems_List;

   Basic_Particles_SP    : GL.Objects.Programs.Program;
   Particles_Initialised : Boolean := False;

   function Get_Particle_Script_Number
     (Name : String; Script_Number : out Positive) return Boolean;

   --  ------------------------------------------------------------------------

   function Create_Particle_System
     (Script_Name : String; Start_Now, Always_Update, Always_Draw : Boolean)
       return Integer is
      Particle_Count : Integer;
      Script_Num     : Positive;
      Script         : Particle_System_Manager.Particle_Script;
      P_System       : Particle_System;
      Result         : Integer := -1;
   begin
      if Particles_Initialised then
         if Get_Particle_Script_Number (Script_Name, Script_Num) then
            Script := Scripts.Element (Script_Num);
            Particle_Count := Integer (Script.Particle_Count);
            for index in 1 .. Particle_Count loop
               P_System.Particle_Positions.Append (Maths.Vec3_0);
               P_System.Particle_Ages.Append
                 (Float (index) * Float (Script.Seconds_Between));
            end loop;
            P_System.Script_Index := Script_Num;
            P_System.Is_Running := Start_Now;
            P_System.Always_Update := Always_Update;
            P_System.Always_Draw := Always_Draw;
            Particle_Systems.Append (P_System);
            Result := Integer (Particle_Systems.Length);
         end if;
      end if;
      return Result;

   exception
      when anError : others =>
         Put_Line
           ("An exception occurred in Particle_System.Create_Particle_System!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
         return Result;
   end Create_Particle_System;

   --  ------------------------------------------------------------------------

   function Get_Particle_Script_Number
     (Name : String; Script_Number : out Positive) return Boolean is
      use Ada.Strings.Unbounded;
      use Particle_System_Manager.Script_Package;
      Curs          : Cursor := Scripts.First;
      Found         : Boolean := False;
   begin
      if Particles_Initialised then
         while Has_Element (Curs) and not Found loop
            Found := To_String (Element (Curs).Script_Name) = Name;
            if Found then
               Script_Number := To_Index (Curs);
            else
               Next (Curs);
            end if;
         end loop;
      end if;

      return Found;
   end Get_Particle_Script_Number;

   --  ------------------------------------------------------------------------

   procedure Init is
      use Particle_System_Manager;
   begin
      Game_Utils.Game_Log ("---- Initialising Particle Systems ----");
      Put_Line ("---- Initialising Particle Systems ----");
      Particle_System_Shader_Manager.Init (Basic_Particles_SP);
      Scripts.Clear;
      Particle_Systems.Clear;
      Put_Line ("Particle Systems cleared");
      Load_Particle_Script ("torch_smoke.particles", Scripts);
      Put_Line ("Particle Systems torch_smoke loaded");
      Load_Particle_Script ("blood_fountain.particles", Scripts);
      Load_Particle_Script ("blood_damage.particles", Scripts);
      Load_Particle_Script ("blood_artery_jet.particles", Scripts);
      Load_Particle_Script ("dust.particles", Scripts);
      Load_Particle_Script ("splash.particles", Scripts);
      Game_Utils.Game_Log ("---- Particle Systems Initialized ----");
      Put_Line ("---- Particle Systems Initialized ----");
      Particles_Initialised := True;
   end Init;

   --  ------------------------------------------------------------------------

end Particle_System;
