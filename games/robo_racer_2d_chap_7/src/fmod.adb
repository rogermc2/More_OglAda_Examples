
with Interfaces.C;

with Fmod.API;

package body Fmod is

   Audio_Handle : Fmod_Common.GLvoid_Handle := null;
   pragma Convention (C, Audio_Handle);

   --  -------------------------------------------------------------------------

   function Close_System return Fmod_Result is
   begin
      return Fmod.API.System_Close (Audio_Handle.all);
   end Close_System;

   --  -------------------------------------------------------------------------

   function Create_Sound (name_or_data : String;
                         mode : Fmod_Mode;
                         extradriverdata : System.Address) return Fmod_Result is
      use Interfaces.C;
   begin
      return Fmod.API.Create_Sound
        (Audio_Handle.all, Interfaces.C.To_C (name_or_data), mode, extradriverdata);
   end Create_Sound;

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
