
with System;

with GL.Types; use GL.Types;

with Fmod_Common; use Fmod_Common;

package Fmod is

    Fmod_Exception : Exception;

    function Close_System return Fmod_Result;
    procedure Create_Sound (name_or_data : String; mode : Fmod_Mode;
                            exinfo       : Fmod_Create_Sound_Exinfo_Ptr;
                            sound        : out Fmod_Sound_Handle);
    function Create_System return Fmod_Result;
    function Create_And_Initialize_System
      (maxchannels     : Int; flags : Fmod_Init_Flags;
       extradriverdata : System.Address) return Fmod_Result;
    function Get_Open_State (sound              : Fmod_Sound_Handle;
                             openstate          : out Fmod_Open_State;
                             percentbuffered    : out UInt;
                             starving, diskbusy : out Boolean)
                            return Fmod_Result;
    function Init_System (maxchannels     : Int; flags : Fmod_Init_Flags;
                          extradriverdata : System.Address) return Fmod_Result;
    procedure Pause_Channel (Channel : Fmod_Channel_Handle; Pause : Boolean);
    procedure Play_Sound (sound        : Fmod_Sound_Handle;
                          channelgroup : Fmod_Channelgroup_Ptr;
                          paused       : Boolean;
                          channel      : out Fmod_Channel_Handle);
    procedure Print_Open_State (Message : String;
                                Sound : Fmod_Common.Fmod_Sound_Handle);
    procedure Release_Channel_Group (Group : Fmod_Channelgroup_Handle);
    procedure Release_Sound (Sound : Fmod_Sound_Handle);
    procedure Release_System;

end Fmod;
