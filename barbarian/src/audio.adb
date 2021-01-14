
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
        File_Names    : Unbounded_String_Array (1 .. Max_Music_Tracks) :=
                          (others => To_Unbounded_String (""));
        --        Sound_Source : Irrklang::ISoundSource*;
    end record;

    type Ambient_Sound_Array is array (1 .. Max_Ambient_Sounds) of Ambient_Sound_Data;
    type Boulder_Sound_Array is array (1 .. Max_Boulder_Sounds) of Boulder_Sound_Data;
    type Loaded_Sound_Array is array (1 .. Max_Loaded_Sounds) of Loaded_Sound_Data;

    type Audio_Data is record
    --        Device         : Irrklang::Isoundengine*;
    --        Curr_Music_Snd : Irrklang::Isound;
    --        Credits_Snd    : Irrklang::Isound;
        Ambient_Sounds      : Ambient_Sound_Array;
        Boulder_Sounds      : Boulder_Sound_Array;
        Loaded_Sounds       : Loaded_Sound_Array;
        Sound_Vom           : Music_Vom_Data;
        Mus_Vom             : Music_Vom_Data;
        Boulder_Sound_Count : Natural := 0;
        Ambient_Sound_Count : Natural := 0;
        Loaded_Sound_Count  : Natural := 0;
        Device_Name         : Unbounded_String := To_Unbounded_String ("");
        Was_Init            : Boolean := False;
    end record;

    G_Audio : Audio_Data;

    --  -------------------------------------------------------------------------

    procedure Create_Boulder_Sound (Pos : GL.Types.Singles.Vector3) is
        Index        : constant Natural := G_Audio.Boulder_Sound_Count;
        Radius       : Float := 12.0;
        Play_Looped  : Boolean := True;
        Start_Paused : Boolean := True;
        Track        : Boolean := True;
    begin
        if G_Audio.Boulder_Sound_Count >= Max_Boulder_Sounds then
            raise Audio_Exception with
              "Audio.Create_Boulder_Sound trying to exceed Max_Boulder_Sounds";
        end if;

        G_Audio.Boulder_Sounds (Index).Playing := False;
        G_Audio.Boulder_Sounds (Index).Position := Pos;
        --          G_Audio.Boulder_Sounds (Index).Snd :=
        --            G_Audio.Device -> Play2D (Boulder_Sound_File, Play_Looped,
        --          Start_Paused, Track);
        --    G_Audio.Boulder_Sounds[Idx].Snd->Setmindistance (Radius / 2.0);
        --    G_Audio.Boulder_Sounds[Idx].Snd->Setmaxdistance (Radius / 10.0);
        --    -- prevent starting at full vol
        --    G_Audio.Boulder_Sounds[Idx].Snd->Setvolume (0.0);
        G_Audio.Boulder_Sound_Count := G_Audio.Boulder_Sound_Count + 1;
    end Create_Boulder_Sound;

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

    procedure Play_Credits_Music (File_Name : String) is
    begin
        null;
    end Play_Credits_Music;

    --  -------------------------------------------------------------------------

    procedure Play_Music (File_Name : String) is
    begin
        null;
    end Play_Music;

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

    function Start_Boulder_Sound (Index : Positive) return Boolean is
    begin
        return False;
    end Start_Boulder_Sound;

    --  -------------------------------------------------------------------------

    procedure Stop_All_Boulder_Sounds is
    begin
        for index in 1 .. G_Audio.Boulder_Sound_Count loop
            Stop_Boulder_Sound (index);
        end loop;
    end Stop_All_Boulder_Sounds;

    --  -------------------------------------------------------------------------

    procedure Stop_All_Sounds is
    begin
        if G_Audio.Was_Init then
            null;
        end if;
    end Stop_All_Sounds;

    --  -------------------------------------------------------------------------

    procedure Stop_Boulder_Sound (Index : Integer) is
    begin
        null;
    end Stop_Boulder_Sound;

    --  -------------------------------------------------------------------------

    procedure Stop_Credits_Music is
    begin
        if G_Audio.Was_Init then
            null;
        end if;
    end Stop_Credits_Music;

    --  -------------------------------------------------------------------------

    procedure Update_Ambient_Sounds is
    begin
        if G_Audio.Was_Init then
            null;
        end if;
    end Update_Ambient_Sounds;

    --  -------------------------------------------------------------------------

    procedure Update_Boulder_Sounds is
    begin
        null;
    end Update_Boulder_Sounds;

    --  -------------------------------------------------------------------------

end Audio;
