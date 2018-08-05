
with Interfaces.C;
with Interfaces.C.Pointers;

with GL.Types; use  GL.Types;

package IO_Stream is

   type Byte_Data is array (UInt range <>) of aliased UByte;
   package Image_Data_Pointers is new
     Interfaces.C.Pointers (UInt, UByte, Byte_Data, UByte'Last);

   function Read (Buffer : Byte_Data; Size, Count : UInt) return UInt;

end IO_Stream;
