
with Ada.Streams.Stream_IO;

package Text_Manager is

    Text_Manager_Exception : Exception;

    procedure Preload_Comic_Texts
      (Input_Stream : Ada.Streams.Stream_IO.Stream_Access);

end Text_Manager;
