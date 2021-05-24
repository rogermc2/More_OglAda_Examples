
with System;

with GL.Types; use GL.Types;

with Fmod_Common; use Fmod_Common;

package Fmod is

    function Close_System return Fmod_Result;
    function Create_Sound (name_or_data : String; mode : Fmod_Mode;
                           exinfo : access Fmod_Create_Sound_Exinfo;
                           sound : Fmod_Sound) return Fmod_Result;
    function Create_System return Fmod_Result;
    function Init_System (maxchannels : Int; flags : Fmod_Init_Flags;
                          extradriverdata : System.Address) return Fmod_Result;

    function Play_Sound (sound : in out Fmod_Sound;
                         channelgroup : in out Fmod_Channelgroup;
                         paused : Boolean;
                         channel : in out System.Address) return Fmod_Result;

end Fmod;
