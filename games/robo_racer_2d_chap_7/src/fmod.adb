
with Interfaces.C;

with Ada.Text_IO; use Ada.Text_IO;

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
                          exinfo       : access Fmod_Create_Sound_Exinfo;
                          sound        : in out Fmod_Sound_Handle) return Fmod_Result is
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
   function Get_Open_State (sound              : out Fmod_Sound;
                            openstate          : out Fmod_Open_State;
                            percentbuffered    : out UInt;
                            starving, diskbusy : out Boolean)
                            return Fmod_Result is
      Sound_Ptr     : Fmod_Sound_Ptr;
      Openstate_Ptr : Fmod_Open_State_Ptr;
      PB_Ptr        : UInt_Pointers.Pointer;
      Starving_Ptr  : Fmod_Bool_Ptr;
      Disk_Busy_Ptr : Fmod_Bool_Ptr;
      Result        : constant Fmod_Result
        := Fmod.API.Get_Open_State (Sound_Ptr, Openstate_Ptr, PB_Ptr,
                                    Starving_Ptr, Disk_Busy_Ptr);
   begin
      if Sound_Ptr /= null then
         sound := Sound_Ptr.all;
      else
         Put_Line ("Fmod.Get_Open_State Sound_Ptr is null.");
      end if;
      if Openstate_Ptr /= null then
         openstate := Openstate_Ptr.all;
      else
         Put_Line ("Fmod.Get_Open_State Openstate_Ptr is null.");
      end if;
      --        percentbuffered := PB_Ptr.all;
      percentbuffered := 0;

      if Starving_Ptr /= null then
         starving := Starving_Ptr.all /= 0;
      else
         Put_Line ("Fmod.Get_Open_State Starving_Ptr is null.");
      end if;

      if Disk_Busy_Ptr /= null then
         diskbusy := Disk_Busy_Ptr.all /= 0;
      else
         Put_Line ("Fmod.Get_Open_State Disk_Busy_Ptr is null.");
      end if;

      return Result;
   end Get_Open_State;

   --  ------------------------------------------------------------------------

   function Init_System (maxchannels     : Int; flags : Fmod_Init_Flags;
                         extradriverdata : System.Address) return Fmod_Result is
   begin
      return Fmod.API.System_Init (Audio_Handle.all, maxchannels, flags,
                                   extradriverdata);
   end Init_System;

   --  -------------------------------------------------------------------------

   function Play_Sound (sound        : Fmod_Sound_Handle;
                        channelgroup : in out Fmod_Channelgroup_Ptr;
                        paused       : Boolean; channel : in out Fmod_Channel_Handle)
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

   procedure Print_Open_State is
      Sound            : Fmod_Sound;
      Open_State       : Fmod_Open_State;
      Percent_Buffered : UInt;
      Starving         : Boolean;
      Disk_Busy        : Boolean;
      Result           : constant Fmod_Result :=
                           Get_Open_State (Sound, Open_State, Percent_Buffered,
                                           Starving, Disk_Busy);
   begin
      Put_Line ("Open State Status:");
      Put_Line ("Fmod Result: " & Fmod_Result'Image (Result));
      Put_Line ("Open State: " & Fmod_Open_State'Image (Open_State));
      Put_Line ("Percent Buffered: " & UInt'Image (Percent_Buffered));
      Put_Line ("Starving: " & Boolean'Image (Starving));
      Put_Line ("Disk Busy: " & Boolean'Image (Disk_Busy));
   end Print_Open_State;


   --  -------------------------------------------------------------------------

end Fmod;
