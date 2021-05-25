
with System;

with Ada.Text_IO; use Ada.Text_IO;

with Fmod_Common;
with Fmod;
with System.Address_Image;

package body Audio_Manager is

   sfx_Jump         : Fmod_Common.Fmod_Sound_Handle;
   sfx_Movement     : Fmod_Common.Fmod_Sound_Handle;
   sfx_Oilcan       : Fmod_Common.Fmod_Sound_Handle;
   sfx_Water        : Fmod_Common.Fmod_Sound_Handle;
   Channel_Movement : Fmod_Common.Fmod_Channel_Handle;
   Channel_Group    : Fmod_Common.Fmod_Channelgroup_Ptr;


   procedure Print_Handle (n : Fmod_Common.Fmod_Sound_Handle);

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
         Put_Line ("Audio_Manager.Init audio system created");
         F_Result := Fmod.Init_System (50, Fmod_Init_Normal, System.Null_Address);
         if F_Result = Fmod_Ok then
            Put_Line ("Audio_Manager.Init audio system initialized");
         else
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
      Print_Handle (sfx_Oilcan);

      if F_Result = Fmod_Ok then
         Put_Line ("Audio_Manager.Load_Audio audio loaded");
         --              Fmod.Print_Open_State;
         --  Play_Sound uses the Sound handle returned by Create_Sound
         --  and returns a Channel handle.
         F_Result := Fmod.Play_Sound (sfx_Movement, Channel_Group, False,
                                      Channel_Movement);
         if F_Result = Fmod_Ok then
            Put_Line ("Audio_Manager.Load_Audio sound played");
         else
            Put_Line ("Audio_Manager.Load_Audio play sound failed " &
                        "with error: " & Fmod_Result'Image (F_Result));
            end if;
      else
         raise Audio_Exception with
           "Audio_Manager.Load_Audio audio loading failed"
           & " with failure Load_Audio " & Fmod_Result'Image (F_Result);
      end if;

   end Load_Audio;

   --  -------------------------------------------------------------------------

   procedure Print_Handle (n : Fmod_Common.Fmod_Sound_Handle) is
begin
   Put_Line ("sfx_Oilcan at address " & System.Address_Image (n.all'address)); --'
end Print_Handle;

   --  ------------------------------------------------------------------------

end Audio_Manager;
