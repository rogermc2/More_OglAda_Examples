
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body Ogldev_IO is

   procedure Read (File_Name : String; Data : out Image.Image) is
      File_ID      : Ada.Streams.Stream_IO.File_Type;
      Data_Stream  : Stream_Access;
   begin
      Open (File_ID, In_File, File_Name);
      Data_Stream := Stream (File_ID);
      Image.Image'Read (Data_Stream, Data);
      Close (File_ID);

   exception
      when others =>
         Put_Line ("An exception occurred in Ogldev_IO.Read.");
         raise;
   end Read;

   --  -------------------------------------------------------------------------

   procedure Write (File_Name : String; Data : String) is
      File_ID             : Ada.Streams.Stream_IO.File_Type;
      Data_Stream         : Stream_Access;
   begin
      Open (File_ID, Out_File, File_Name);
      Data_Stream := Stream (File_ID);
      String'Write (Data_Stream, Data);
      Close (File_ID);

   exception
      when others =>
         Put_Line ("An exception occurred in Ogldev_IO.Write.");
         raise;
   end Write;

   --  -------------------------------------------------------------------------

end Ogldev_IO;
