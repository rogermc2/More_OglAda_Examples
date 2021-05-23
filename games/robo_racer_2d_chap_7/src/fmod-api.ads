
with Interfaces.C.Strings;
with System;

package Fmod.API is

   type GLvoid is null record;
   pragma Convention (C_Pass_By_Copy, GLvoid);

   type GLvoid_Ptr is access GLvoid;
   pragma Convention (C, GLvoid_Ptr);
   type GLvoid_Handle is access GLvoid_Ptr;
   pragma Convention (C, GLvoid_Handle);


   function Create_Sound (aSystem     : in out  GLvoid_Ptr;
                         name_or_data : Interfaces.C.Strings.chars_ptr;
                         mode : Fmod_Mode;
                         extradriverdata : System.Address) return Fmod_Result;
   pragma Import (StdCall, Create_Sound, "FMOD_System_CreateSound");

   function System_Close (aSystem : in out GLvoid_Ptr) return Fmod_Result;
   pragma Import (StdCall, System_Close, "FMOD_System_Close");

   function System_Create (aSystem : in out GLvoid_Handle) return Fmod_Result;
   pragma Import (StdCall, System_Create, "FMOD_System_Create");

   function System_Init (aSystem         : in out  GLvoid_Ptr;
                         maxchannels     : Int; flags : Fmod_Init_Flags;
                         extradriverdata : System.Address) return Fmod_Result;
   pragma Import (StdCall, System_Init, "FMOD_System_Init");

end Fmod.API;
