
--  with Interfaces.C.Pointers;

--  with GL.Types; use GL.Types;

with Image;

package Ogldev_IO is

--     type Byte_Data is array (UInt range <>) of aliased UByte;
--     package Image_Data_Pointers is new
--       Interfaces.C.Pointers (UInt, UByte, Byte_Data, UByte'Last);

 procedure Read (File_Name : String; Data : out Image.Image);
 procedure Write (File_Name : String; Data : String);

end Ogldev_IO;
