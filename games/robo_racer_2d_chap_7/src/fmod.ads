
with System;

with Fmod_Common;

package Fmod is

   type F_System is new System.Address;

   function System_Create (aSystem : in out F_System)
                           return Fmod_Common.Fmod_Result;
   pragma Import (C, System_Create, "FMOD_System_Create");

end Fmod;
