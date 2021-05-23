
with Interfaces.C;
with System;

with Fmod_Common;

package Fmod.API is

   function Create_Sound (aSystem     : in out Fmod_Common.GLvoid_Ptr;
                         name_or_data : Interfaces.C.char_array;
                         mode : Fmod_Mode;
                         extradriverdata : System.Address) return Fmod_Result;
   pragma Import (StdCall, Create_Sound, "FMOD_System_CreateSound");

   function System_Close (aSystem : in out Fmod_Common.GLvoid_Ptr) return Fmod_Result;
   pragma Import (StdCall, System_Close, "FMOD_System_Close");

   function System_Create (aSystem : in out GLvoid_Handle) return Fmod_Result;
   pragma Import (StdCall, System_Create, "FMOD_System_Create");

   function System_Init (aSystem         : in out Fmod_Common.GLvoid_Ptr;
                         maxchannels     : Int; flags : Fmod_Init_Flags;
                         extradriverdata : System.Address) return Fmod_Result;
   pragma Import (StdCall, System_Init, "FMOD_System_Init");

end Fmod.API;
