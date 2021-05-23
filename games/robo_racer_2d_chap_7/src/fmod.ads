
with System;

with GL.Types; use GL.Types;

with Fmod_Common; use Fmod_Common;

package Fmod is

   type F_System is new System.Address;
   type GLvoid is null record;

   function System_Create (aSystem : in out F_System) return Fmod_Result;
   pragma Import (C, System_Create, "FMOD_System_Create");


   function System_Init (aSystem : in out F_System;
                         maxchannels : Int; flags : Fmod_Init_Flags;
                         extradriverdata : access GLvoid) return Fmod_Result;
   pragma Import (C, System_Init, "FMOD_System_Init");

end Fmod;
