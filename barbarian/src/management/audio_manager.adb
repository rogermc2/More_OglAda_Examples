
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Audio_Manager is

    Min_Sound_D        : constant Float := 8.0;
    Max_Ambient_Sounds : constant Integer := 128;
    Max_Boulder_Sounds : constant Integer := 64;
    Max_Loaded_Sounds  : constant Integer := 512;
    Max_Music_Tracks   : constant Integer := 128;

    type Loaded_Sound_Data is record
        File_Name : Unbounded_String := To_Unbounded_String ("");

    end record;

    --  ------------------------------------------------------------------------

    function Init_Audio return Boolean is
    begin
        return False;
    end Init_Audio;

    --  ------------------------------------------------------------------------

    procedure Load_Ambient_Sounds
      (Input_Stream : Ada.Streams.Stream_IO.Stream_Access) is
    begin
        null;
    end Load_Ambient_Sounds;

    --  ------------------------------------------------------------------------

end Audio_Manager;
