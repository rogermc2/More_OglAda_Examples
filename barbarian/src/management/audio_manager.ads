
with Ada.Streams.Stream_IO;

package Audio_Manager is

    function Init_Audio return Boolean;
    procedure Load_Ambient_Sounds
      (Input_Stream : Ada.Streams.Stream_IO.Stream_Access);

end Audio_Manager;
