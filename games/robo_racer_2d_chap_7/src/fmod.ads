
with System;

with GL.Types; use GL.Types;

with Fmod_Common; use Fmod_Common;

package Fmod is

    function Close_System return Fmod_Result;
    function Create_Sound (name_or_data : String; mode : Fmod_Mode;
                           exinfo : access Fmod_Create_Sound_Exinfo;
                           sound : in out Fmod_Sound_Handle) return Fmod_Result;
    function Create_System return Fmod_Result;
    function Get_Open_State (sound : in out Fmod_Sound_Ptr;
                             openstate : in out Fmod_Open_State_Ptr;
                             percentbuffered : in out UInt_Pointers.Pointer;
                             starving, diskbusy : in out Fmod_Bool_Ptr)
                             return Fmod_Result;
    function Init_System (maxchannels : Int; flags : Fmod_Init_Flags;
                          extradriverdata : System.Address) return Fmod_Result;

    function Play_Sound (sound : Fmod_Sound_Handle;
                         channelgroup : in out Fmod_Channelgroup_Ptr;
                         paused : Boolean;
                         channel : in out Fmod_Channel_Handle) return Fmod_Result;

end Fmod;
