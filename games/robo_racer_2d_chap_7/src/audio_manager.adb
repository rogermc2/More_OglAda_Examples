
with System;

with Ada.Text_IO; use Ada.Text_IO;

with Fmod_Common;
with Fmod;

package body Audio_Manager is

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
        F_Result         : Fmod_Result;
    begin
        --  Create_Sound returns a Sound handle which is a handle to the
        --  loaded sound.
        F_Result := Fmod.Create_Sound ("src/audio/oil.wav", Fmod_Default,
                                       null, sfx_Oilcan);
        if F_Result = Fmod_Ok then
            F_Result := Fmod.Create_Sound ("src/audio/water.wav", Fmod_Default,
                                           null, sfx_Water);
            if F_Result = Fmod_Ok then
                F_Result := Fmod.Create_Sound ("src/audio/jump.wav", Fmod_Default,
                                               null, sfx_Jump);
                if F_Result = Fmod_Ok then
                    F_Result := Fmod.Create_Sound
                      ("src/audio/movement.wav", Fmod_Loop_Normal_Or_2D,
                       null, sfx_Movement);
                end if;
            end if;
        end if;

        F_Result := Fmod.Create_Sound ("src/audio/button.wav", Fmod_Default,
                                        null, sfx_Button);

        if F_Result = Fmod_Ok then
            --  Play_Sound uses the Sound handle returned by Create_Sound
            --  and returns a Channel handle.
            --  The default behavior is always FMOD_CHANNEL_FREE.

            F_Result := Fmod.Play_Sound (sfx_Movement, null, False,
                                         Movement_Channel);
            if F_Result /= Fmod_Ok then
                Put_Line ("Audio_Manager.Load_Audio play sound failed " &
                            "with error:" &
                            Integer'Image (Fmod_Result'Enum_Rep (F_Result)) &
                            " " & Fmod_Result'Image (F_Result));
                Fmod.Print_Open_State ("Audio_Manager.Load_Audio play audio result",
                                       sfx_Oilcan);
            end if;
        else
            raise Audio_Exception with
              "Audio_Manager.Load_Audio audio loading failed"
              & " with failure Load_Audio " & Fmod_Result'Image (F_Result);
        end if;

    end Load_Audio;

    --  -------------------------------------------------------------------------

    procedure Pause_Movement_Channel (Pause : Boolean) is
    begin
        Fmod.Pause_Channel (Movement_Channel, Pause);
    end Pause_Movement_Channel;

    --  -------------------------------------------------------------------------

    procedure Play_Sound (aSound : Sound) is
        use Fmod_Common;
        Sound_Handle : Fmod_Common.Fmod_Sound_Handle;
        Channel      : Fmod_Common.Fmod_Channel_Handle;
        Result       : Fmod_Result;
    begin
      case aSound is
            when Button_Sound => Sound_Handle := sfx_Button;
            when Jump_Sound => Sound_Handle := sfx_Jump;
            when Movement_Sound => Sound_Handle := sfx_Movement;
            when Oilcan_Sound => Sound_Handle := sfx_Oilcan;
            when Water_Sound => Sound_Handle := sfx_Water;
      end case;

      Result := Fmod.Play_Sound (Sound_Handle, null, False, Channel);
      if Result /= Fmod_Ok then
         raise Audio_Exception with
           "Audio_Manager.Play_Sound failed with failure code " &
           Fmod_Result'Image (Result);
      end if;
    end Play_Sound;

    --  -------------------------------------------------------------------------

end Audio_Manager;
