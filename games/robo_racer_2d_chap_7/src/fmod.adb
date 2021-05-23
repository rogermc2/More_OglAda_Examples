
with Fmod.API;

package body Fmod is

   Audio    : Fmod.API.F_System_Handle;
   pragma Convention (C, Audio);

   function Close_System return Fmod_Result is
   begin
      return Fmod.API.System_Close (Audio);
   end Close_System;

   function Create_System return Fmod_Result is
   begin
      return Fmod.API.System_Create (Audio);
   end Create_System;

   function Init_System (maxchannels     : Int; flags : Fmod_Init_Flags;
                         extradriverdata : System.Address) return Fmod_Result is
   begin
      return Fmod.API.System_Init (Audio, maxchannels, flags, extradriverdata);
   end Init_System;

end Fmod;
