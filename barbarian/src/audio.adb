
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Types;

with Settings;

package body Audio is

   Min_Sound_D        : constant Float := 8.0;
   Max_Ambient_Sounds : constant Integer := 128;
   Max_Boulder_Sounds : constant Integer := 64;
   Max_Loaded_Sounds  : constant Integer := 512;
   Max_Music_Tracks   : constant Integer := 128;

   type Ambient_Sound_Data is record
      Position       : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
      Cut_Off_Radius : Float := 1.0;
      Map_S          : Integer := 0;
      Map_T          : Integer := 0;
      H              : Integer := 0;
      File_Name      : Unbounded_String := To_Unbounded_String ("");
      --        Sound_Source : Irrklang::ISoundSource*;
   end record;

   type Boulder_Sound_Data is record
      --        Sound : Irrklang::ISound;
      Position : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
      Playing  : Boolean := False;
   end record;

   type Loaded_Sound_Data is record
      File_Name    : Unbounded_String := To_Unbounded_String ("");
      --        Sound_Source : Irrklang::ISoundSource*;
   end record;

   type Unbounded_String_Array is array (Integer range <>) of Unbounded_String;
   type Data_Array is array (Integer range <>) of GL.Types.UByte;

   type Music_Vom_Data is record
      Data          : Data_Array (1 .. Max_Music_Tracks) := (others => 0);
      File_Names    : Unbounded_String_Array (1 ..Max_Music_Tracks) :=
                        (others => To_Unbounded_String (""));
      --        Sound_Source : Irrklang::ISoundSource*;
   end record;

   type Ambient_Sound_Array is array (1 .. Max_Ambient_Sounds) of Ambient_Sound_Data;
   type Boulder_Sound_Array is array (1 .. Max_Boulder_Sounds) of Ambient_Sound_Data;
   type Loaded_Sound_Array is array (1 .. Max_Loaded_Sounds) of Ambient_Sound_Data;

   type Audio_Data is record
      --        Device         : Irrklang::Isoundengine*;
      --        *Credits_Snd   : Irrklang::Isound *Curr_Music_Snd;
      Ambient_Sounds      : Ambient_Sound_Array;
      Boulder_Sounds      : Boulder_Sound_Array;
      Loaded_Sounds       : Loaded_Sound_Array;
      Sound_Vom           :  Music_Vom_Data;
      Mus_Vom             :  Music_Vom_Data;
      Boulder_Sound_Count : Integer := 0;
      Ambient_Sound_Count : Integer := 0;
      Loaded_Sound_Count  : Integer := 0;
      Device_Name         : Unbounded_String := To_Unbounded_String ("");
      Was_Init            : Boolean := False;
   end record;

   G_Audio : Audio_Data;

   --  -------------------------------------------------------------------------

   procedure Pause_Music (Pause : Boolean) is
   begin
      if G_Audio.Was_Init then
         null;
--           if G_Audio.Curr_Music_Snd then
--              G_Audio.Curr_Music_Snd.Set_Is_Paused (Pause);
--           end if;
      end if;
   end Pause_Music;

   --  -------------------------------------------------------------------------

   procedure Play_Sound (File_Name : String; Random_Pitch : Boolean) is
   begin
      null;
   end Play_Sound;

   --  -------------------------------------------------------------------------

   procedure Set_Audio_Volume (Volume : Integer) is
   begin
      Settings.Set_Audio_Volume (Volume);
      if G_Audio.Was_Init then
         --           G_Audio.Device.Set_Sound_Volume (Float (Volume) / 10.0)
         null;
      end if;
   end Set_Audio_Volume;

   --  -------------------------------------------------------------------------

   procedure Stop_Credits_Music is
   begin
      if G_Audio.Was_Init then
         null;
      end if;
   end Stop_Credits_Music;

   --  -------------------------------------------------------------------------

end Audio;
