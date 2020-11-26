
with Ada.Text_IO; use Ada.Text_IO;

package body MGL_Common is

   procedure Read_SDL_File (File_Name : String; Data : SDL_Data) is
      Input_File : File_Type;
      aLine      : Unbounded_String := To_Unbounded_String (" ");
      Pos        : Integer;
      Line_Length : Integer;
   begin
      Open (Input_File, In_File, File_Name);
      while Length (aline) < 2 and then Slice (aLine, 1, 1) /= " "
        and then Slice (aLine, 1, 2) /= "/*"
        and then Slice (aLine, 1, 2) /= "//"
        and then Slice (aLine, 1, 2) /= "--" loop
         aLine := To_Unbounded_String (Get_Line (Input_File));
      end loop;

      Line_Length := Length (aline);
      Pos := Index (aLine, 1, Line_Length, ',');



      Close (Input_File);
   end Read_SDL_File;

end MGL_Common;
