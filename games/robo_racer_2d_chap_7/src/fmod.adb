
with Interfaces.C;

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
                           sound : Fmod_Sound) return Fmod_Result is
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

    function Init_System (maxchannels     : Int; flags : Fmod_Init_Flags;
                          extradriverdata : System.Address) return Fmod_Result is
    begin
        return Fmod.API.System_Init (Audio_Handle.all, maxchannels, flags,
                                     extradriverdata);
    end Init_System;

    --  -------------------------------------------------------------------------

    function Play_Sound (sound : in out Fmod_Sound;
                         channelgroup : in out Fmod_Channelgroup;
                         paused : Boolean; channel : in out System.Address)
                         return Fmod_Result is
        Sound_Alias   : aliased Fmod_Sound := sound;
        CG_Alias      : aliased Fmod_Channelgroup := channelgroup;
        Pause : Fmod_Bool := 0;
    begin
        if paused then
            Pause := 1;
        end if;
        return Fmod.API.Play_Sound (Audio_Handle.all, Sound_Alias'Access,
                                    CG_Alias'Access, Pause, channel);
    end Play_Sound;

    --  -------------------------------------------------------------------------

end Fmod;
