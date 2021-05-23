
with System;

package Fmod.API is

   type F_System is new System.Address;
   type F_System_Handle is access F_System;
   pragma Convention (C, F_System_Handle);

   function System_Close (aSystem : in out F_System_Handle) return Fmod_Result;
   pragma Import (C, System_Close, "FMOD_System_Close");

   function System_Create (aSystem : in out F_System_Handle) return Fmod_Result;
   pragma Import (C, System_Create, "FMOD_System_Create");

   function System_Init (aSystem : in out F_System_Handle;
                         maxchannels : Int; flags : Fmod_Init_Flags;
                         extradriverdata : System.Address) return Fmod_Result;
   pragma Import (C, System_Init, "FMOD_System_Init");

end Fmod.API;
