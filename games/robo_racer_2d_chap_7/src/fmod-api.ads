
with Interfaces.C;
with System;

with Fmod_Common;

package Fmod.API is

    function Create_Sound (aSystem     : in out Fmod_Common.Fmod_System_Ptr;
                           name_or_data : Interfaces.C.char_array;
                           mode : Fmod_Mode;
                           exinfo : access Fmod_Create_Sound_Exinfo;
                           sound : in out Fmod_Sound_Handle) return Fmod_Result;
    pragma Import (C, Create_Sound, "FMOD_System_CreateSound");

    function Get_Open_State (sound : Fmod_Sound_Ptr;
                             openstate : out Fmod_Open_State_Ptr;
                             percentbuffered : out UInt_Pointers.Pointer;
                             starving, diskbusy : out Fmod_Bool_Ptr)
                             return Fmod_Result;
    pragma Import (C, Get_Open_State, "FMOD_Sound_GetOpenState");

    function Play_Sound (aSystem : access Fmod_System;
                         sound : access Fmod_Sound;
                         channelgroup : access Fmod_Channelgroup;
                         paused : Fmod_Bool;
                         channel : out Fmod_Channel_Handle) return Fmod_Result;
    pragma Import (C, Play_Sound, "FMOD_System_PlaySound");

    function System_Close (aSystem : in out Fmod_System_Ptr) return Fmod_Result;
    pragma Import (C, System_Close, "FMOD_System_Close");

    function System_Create (aSystem : in out Fmod_System_Handle) return Fmod_Result;
    pragma Import (C, System_Create, "FMOD_System_Create");

    function System_Init (aSystem         : in out Fmod_System_Ptr;
                          maxchannels     : Int; flags : Fmod_Init_Flags;
                          extradriverdata : System.Address) return Fmod_Result;
    pragma Import (C, System_Init, "FMOD_System_Init");

end Fmod.API;
