
with System;

with GL.Types; use GL.Types;

with Fmod_Common; use Fmod_Common;

package Fmod is

   type F_System is access System.Address;
   pragma Convention (C, F_System);

   function System_Close (aSystem : in out F_System) return Fmod_Result;
   pragma Import (C, System_Close, "FMOD_System_Close");

   function System_Create (aSystem : in out F_System) return Fmod_Result;
   pragma Import (C, System_Create, "FMOD_System_Create");

   function System_Init (aSystem : in out F_System;
                         maxchannels : Int; flags : Fmod_Init_Flags;
                         extradriverdata : System.Address) return Fmod_Result;
   pragma Import (C, System_Init, "FMOD_System_Init");

   function Close_System return Fmod_Result;
   function Create_System return Fmod_Result;
   function Init_System (maxchannels : Int; flags : Fmod_Init_Flags;
                         extradriverdata : System.Address) return Fmod_Result;

end Fmod;
