
with Fmod.API;

package body Fmod is

   Audio_Handle : Fmod.API.GLvoid_Handle := null;
   pragma Convention (C, Audio_Handle);

   --  -------------------------------------------------------------------------

   function Close_System return Fmod_Result is
   begin
      return Fmod.API.System_Close (Audio_Handle.all);
   end Close_System;

   --  -------------------------------------------------------------------------

   function Create_System return Fmod_Result is
   begin
      return Fmod.API.System_Create (Audio_Handle);
   end Create_System;

   --  -------------------------------------------------------------------------

   function Init_System (maxchannels     : Int; flags : Fmod_Init_Flags;
                         extradriverdata : System.Address) return Fmod_Result is
   begin
      return Fmod.API.System_Init (Audio_Handle.all, maxchannels, flags,
                                   extradriverdata);
   end Init_System;

   --  -------------------------------------------------------------------------

end Fmod;
