
with Interfaces.C;

--  with Ada.Text_IO; use Ada.Text_IO;

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

    function Create_Sound (name_or_data : String; mode : Fmod_Mode;
                           exinfo : access Fmod_Create_Sound_Exinfo;
                           sound : in out Fmod_Sound_Handle) return Fmod_Result is
    begin
        return Fmod.API.Create_Sound
          (Audio_Handle.all, Interfaces.C.To_C (name_or_data), mode,
           exinfo, sound);
    end Create_Sound;

    --  -------------------------------------------------------------------------

    function Create_System return Fmod_Result is
    begin
        return Fmod.API.System_Create (Audio_Handle);
    end Create_System;

    --  -------------------------------------------------------------------------
--  Get_Open_State parameters
--  openstate: address of a variable that receives the open state of a sound.
--  Optional. Specify 0 or NULL to ignore.
--  percentbuffered: address of a variable that receives the percentage of the
--  file buffer filled progress of a stream.
--  Optional. Specify 0 or NULL to ignore.
--  starving: address of a variable that receives the starving state of a sound.
--  If a stream has decoded more than the stream file buffer has ready for it,
--  it will return TRUE.
--  Optional. Specify 0 or NULL to ignore.
--  diskbusy: address of a variable that receives the disk busy state of a sound.
--  That is, whether or not the disk is currently being accessed for the sound.
    function Get_Open_State (sound : in out Fmod_Sound_Ptr;
                             openstate : in out Fmod_Open_State_Ptr;
                             percentbuffered : in out UInt_Pointers.Pointer;
                             starving, diskbusy : in out Fmod_Bool_Ptr)
                             return Fmod_Result is
    begin
        return Fmod.API.Get_Open_State (sound, openstate, percentbuffered,
                                        starving, diskbusy);
    end Get_Open_State;

    --  ------------------------------------------------------------------------

    function Init_System (maxchannels     : Int; flags : Fmod_Init_Flags;
                          extradriverdata : System.Address) return Fmod_Result is
    begin
        return Fmod.API.System_Init (Audio_Handle.all, maxchannels, flags,
                                     extradriverdata);
    end Init_System;

    --  -------------------------------------------------------------------------

    function Play_Sound (sound : Fmod_Sound_Handle;
                         channelgroup : in out Fmod_Channelgroup_Ptr;
                         paused : Boolean; channel : in out Fmod_Channel_Handle)
                         return Fmod_Result is
        Pause  : Fmod_Bool := 0;
    begin
        if paused then
            Pause := 1;
        end if;
--          Put_Line ("Fmod.Play_Sound");
        return Fmod.API.Play_Sound (Audio_Handle.all, sound.all,
                                    channelgroup, Pause, channel);
    end Play_Sound;

    --  -------------------------------------------------------------------------

end Fmod;
