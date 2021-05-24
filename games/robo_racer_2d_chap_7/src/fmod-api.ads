
with Interfaces.C;
with System;

with Fmod_Common;

package Fmod.API is

    function Create_Sound (aSystem     : in out Fmod_Common.GLvoid_Ptr;
                           name_or_data : Interfaces.C.char_array;
                           mode : Fmod_Mode;
                           exinfo : access Fmod_Create_Sound_Exinfo;
                           sound : Fmod_Sound) return Fmod_Result;
    pragma Import (StdCall, Create_Sound, "FMOD_System_CreateSound");

    function Play_Sound (aSystem : in out Fmod_Common.GLvoid_Ptr;
                         sound : access Fmod_Sound;
                         channelgroup : access Fmod_Channelgroup;
                         paused : Fmod_Bool;
                         channel : System.Address) return Fmod_Result;
    pragma Import (StdCall, Play_Sound, "FMOD_System_PlaySound");

    function System_Close (aSystem : in out Fmod_Common.GLvoid_Ptr) return Fmod_Result;
    pragma Import (StdCall, System_Close, "FMOD_System_Close");

    function System_Create (aSystem : in out GLvoid_Handle) return Fmod_Result;
    pragma Import (StdCall, System_Create, "FMOD_System_Create");

    function System_Init (aSystem         : in out Fmod_Common.GLvoid_Ptr;
                          maxchannels     : Int; flags : Fmod_Init_Flags;
                          extradriverdata : System.Address) return Fmod_Result;
    pragma Import (StdCall, System_Init, "FMOD_System_Init");

end Fmod.API;
