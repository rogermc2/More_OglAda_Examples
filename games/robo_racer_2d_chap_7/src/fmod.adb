
with Fmod.API;

package body Fmod is

   Audio     : aliased Fmod.API.Fmod_System;
   Audio_Ptr : Fmod.API.F_System_Handle;
   pragma Convention (C, Audio_Ptr);

   --  -------------------------------------------------------------------------

   function Close_System return Fmod_Result is
   begin
      Audio_Ptr.all := Audio;
      return Fmod.API.System_Close (Audio_Ptr);
   end Close_System;

   --  -------------------------------------------------------------------------

   function Create_System return Fmod_Result is
   begin
      Audio_Ptr.all := Audio;
      return Fmod.API.System_Create (Audio_Ptr);
   end Create_System;

   --  -------------------------------------------------------------------------

   function Init_System (maxchannels     : Int; flags : Fmod_Init_Flags;
                         extradriverdata : System.Address) return Fmod_Result is
   begin
      Audio_Ptr.all := Audio;
      return Fmod.API.System_Init (Audio_Ptr, maxchannels, flags,
                                   extradriverdata);
   end Init_System;

   --  -------------------------------------------------------------------------

end Fmod;
