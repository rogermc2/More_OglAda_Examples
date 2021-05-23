
with System;

with GL.Types; use GL.Types;

with Fmod_Common; use Fmod_Common;

package Fmod is

   function Close_System return Fmod_Result;
   function Create_System return Fmod_Result;
   function Init_System (maxchannels : Int; flags : Fmod_Init_Flags;
                         extradriverdata : System.Address) return Fmod_Result;

end Fmod;
