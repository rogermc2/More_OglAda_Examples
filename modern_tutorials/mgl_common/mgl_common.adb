
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

package body MGL_Common is

   procedure Read_SDL_File (File_Name : String; Data : in out SDL_Data) is
      use Ada.Strings;
      Input_File  : File_Type;
      aLine       : Unbounded_String := To_Unbounded_String (" ");
      Pos1        : Natural := 0;
      Pos2        : Natural := 0;
      Line_Length : Integer;
   begin
      Open (Input_File, In_File, File_Name);
      while Length (aline) < 2 and then Slice (aLine, 1, 1) /= " "
        and then Slice (aLine, 1, 2) /= "/*"
        and then Slice (aLine, 1, 2) /= "//"
        and then Slice (aLine, 1, 2) /= "--" loop
         aLine := To_Unbounded_String (Get_Line (Input_File));
      end loop;

      declare
         aString : String := To_String (aLine);
      begin
      Line_Length := aString'Length;
      Pos1 := Fixed.Index (aString, ",");
      Data.Width := GL.Types.Int'Value (aString (1 .. Pos1 - 1));
      Pos2 := Fixed.Index (aString (Pos1 + 1 .. Line_Length), ",");
      Data.Height := GL.Types.Int'Value (aString (Pos1 + 1 .. Pos2 - 1));
      Pos1 := Fixed.Index (aString (Pos2 + 1 .. Line_Length), ",");
      Data.Pixels_Per_Byte := GL.Types.Int'Value (aString (Pos2 + 1 .. Pos1 - 1));
      end;

      Close (Input_File);
   end Read_SDL_File;

end MGL_Common;
