
with System;

with GL.Types; use GL.Types;

with Fmod_Common; use Fmod_Common;

package Fmod is

   function Close_System return Fmod_Result;
   function Create_Sound (name_or_data : String; mode : Fmod_Mode;
                          exinfo       : access Fmod_Create_Sound_Exinfo;
                          sound        : in out Fmod_Sound_Ptr) return Fmod_Result;
   function Create_System return Fmod_Result;
   function Get_Open_State (sound              : out Fmod_Sound;
                            openstate          : out Fmod_Open_State;
                            percentbuffered    : out UInt;
                            starving, diskbusy : out Boolean)
                             return Fmod_Result;
   function Init_System (maxchannels     : Int; flags : Fmod_Init_Flags;
                         extradriverdata : System.Address) return Fmod_Result;

   function Play_Sound (sound        : in out Fmod_Sound_Ptr;
                        channelgroup : in out Fmod_Channelgroup_Ptr;
                        paused       : Boolean;
                        channel      : in out Fmod_Channel_Handle)
                         return Fmod_Result;
   procedure Print_Open_State (Message : String);
end Fmod;
