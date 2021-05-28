
with System;

with Ada.Text_IO; use Ada.Text_IO;

with Fmod_Common;
with Fmod;

package body Audio_Manager is

    Background_Music : Fmod_Common.Fmod_Sound_Handle;
    sfx_Button       : Fmod_Common.Fmod_Sound_Handle := null;
    sfx_Jump         : Fmod_Common.Fmod_Sound_Handle := null;
    sfx_Movement     : Fmod_Common.Fmod_Sound_Handle := null;
    sfx_Oilcan       : Fmod_Common.Fmod_Sound_Handle := null;
    sfx_Water        : Fmod_Common.Fmod_Sound_Handle := null;
    Movement_Channel : Fmod_Common.Fmod_Channel_Handle := null;

    --  -------------------------------------------------------------------------

    procedure Close is
        use Fmod_Common;
        F_Result : Fmod_Result;
    begin
        F_Result := Fmod.Close_System;
        if F_Result = Fmod_Ok then
            Put_Line ("Audio_Manager.Close audio system closed");
        else
            raise Audio_Exception with
              "Audio_Manager.Close audio system closure failed"
              & " with failure code " & Fmod_Result'Image (F_Result);
        end if;

    end Close;

    --  -------------------------------------------------------------------------

    procedure Init_Fmod is
        use Fmod_Common;
        F_Result : Fmod_Result;
    begin
        F_Result := Fmod.Create_And_Initialize_System
          (50, Fmod_Init_Normal, System.Null_Address);
        if F_Result /= Fmod_Ok then
            raise Audio_Exception with
              "Audio_Manager.Init_Fmod audio system initialization failed"
              & " with failure code " & Fmod_Result'Image (F_Result);
        end if;

    end Init_Fmod;

    --  -------------------------------------------------------------------------

    procedure Load_Audio is
        use Fmod_Common;
        Channel : Fmod_Common.Fmod_Channel_Handle;
        pragma Unreferenced (Channel);
    begin
        --  Create_Sound returns a Sound handle which is a handle to the
        --  loaded sound.
        Fmod.Create_Sound ("src/audio/oil.wav", Fmod_Default, null, sfx_Oilcan);
        Fmod.Create_Sound ("src/audio/water.wav", Fmod_Default,
                           null, sfx_Water);
        Fmod.Create_Sound ("src/audio/jump.wav", Fmod_Default, null, sfx_Jump);
        Fmod.Create_Sound
          ("src/audio/movement.wav", Fmod_Loop_Normal_Or_2D, null, sfx_Movement);
        Fmod.Create_Sound ("src/audio/button.wav", Fmod_Default,
                           null, sfx_Button);
        Fmod.Create_Sound ("src/audio/jollybot.mp3", Fmod_Loop_Normal_Or_2D,
                           null, Background_Music);

        --  Play_Sound uses the Sound handle returned by Create_Sound
        --  and returns a Channel handle.
        --  The default behavior is always FMOD_CHANNEL_FREE.

        Fmod.Play_Sound (sfx_Movement, null, False, Movement_Channel);
        Fmod.Play_Sound (Background_Music, null, False, Channel);

    end Load_Audio;

    --  -------------------------------------------------------------------------

    procedure Pause_Movement_Channel (Pause : Boolean) is
    begin
        Fmod.Pause_Channel (Movement_Channel, Pause);
    end Pause_Movement_Channel;

    --  -------------------------------------------------------------------------

    procedure Play_Sound (aSound : Sound) is
        Sound_Handle : Fmod_Common.Fmod_Sound_Handle;
        Channel      : Fmod_Common.Fmod_Channel_Handle;
        pragma Unreferenced (Channel);
    begin
        case aSound is
            when Button_Sound => Sound_Handle := sfx_Button;
            when Jump_Sound => Sound_Handle := sfx_Jump;
            when Movement_Sound => Sound_Handle := sfx_Movement;
            when Oilcan_Sound => Sound_Handle := sfx_Oilcan;
            when Water_Sound => Sound_Handle := sfx_Water;
        end case;

        Fmod.Play_Sound (Sound_Handle, null, False, Channel);

    end Play_Sound;

    --  -------------------------------------------------------------------------

    procedure Close_Audio is
        use Fmod;
    begin
        Release_Sound (Background_Music);
        Release_Sound (sfx_Button);
        Release_Sound (sfx_Jump);
        Release_Sound (sfx_Movement);
        Release_Sound (sfx_Oilcan);
        Release_Sound (sfx_Water);

    end Close_Audio;

    --  -------------------------------------------------------------------------

end Audio_Manager;
