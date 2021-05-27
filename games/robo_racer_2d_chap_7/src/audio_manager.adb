
with System;

with Ada.Text_IO; use Ada.Text_IO;

with Fmod_Common;
with Fmod;

package body Audio_Manager is

   sfx_Jump         : Fmod_Common.Fmod_Sound_Handle := null;
   sfx_Movement     : Fmod_Common.Fmod_Sound_Handle := null;
   sfx_Oilcan       : Fmod_Common.Fmod_Sound_Handle := null;
   sfx_Water        : Fmod_Common.Fmod_Sound_Handle := null;
   Channel_Oil      : Fmod_Common.Fmod_Channel_Handle := null;

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
      F_Result := Fmod.Create_System;
      if F_Result = Fmod_Ok then
         F_Result := Fmod.Init_System (50, Fmod_Init_Normal, System.Null_Address);
         if F_Result /= Fmod_Ok then
            raise Audio_Exception with
              "Audio_Manager.Init_Fmod audio system initialization failed"
              & " with failure code " & Fmod_Result'Image (F_Result);
         end if;
      else
         raise Audio_Exception with
           "Audio_Manager.Init_Fmod audio system creation failed"
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
               F_Result := Fmod.Create_Sound ("src/audio/movement.wav", Fmod_Default,
                                              null, sfx_Movement);
            end if;
         end if;
      end if;

      if F_Result = Fmod_Ok then
--           Fmod.Print_Open_State ("Audio_Manager.Load_Audio audio loaded",
--                                  sfx_Oilcan);
         --  Play_Sound uses the Sound handle returned by Create_Sound
         --  and returns a Channel handle.
         --  The default behavior is always FMOD_CHANNEL_FREE.

         F_Result := Fmod.Play_Sound (sfx_Oilcan, null, False,
                                      Channel_Oil);
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

end Audio_Manager;
