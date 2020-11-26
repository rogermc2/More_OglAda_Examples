
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
--        Longest     : Integer := 0;
--        Shortest    : Integer := 100;
      EOD         : Boolean := False;
   begin
      Open (Input_File, In_File, File_Name);
      aLine := To_Unbounded_String (Get_Line (Input_File));
      while Length (aLine) < 4 or else Slice (aLine, 1, 1) = " "
        or else Slice (aLine, 1, 2) = "/*"
        or else Slice (aLine, 1, 2) = "//"
        or else Slice (aLine, 1, 2) = "--" loop
         aLine := To_Unbounded_String (Get_Line (Input_File));
      end loop;

      declare
         aString : String := To_String (aLine);
      begin
         Line_Length := aString'Length;
         Pos1 := Fixed.Index (aString, ",");
         Data.Width := GL.Types.Int'Value (aString (1 .. Pos1 - 1));
         Pos2 := Fixed.Index (aString (Pos1 + 1 .. Line_Length), ",");
         Data.Height := GL.Types.Int'Value (aString (Pos1 + 2 .. Pos2 - 1));
         Pos1 := Fixed.Index (aString (Pos2 + 1 .. Line_Length), ",");
         Data.Pixels_Per_Byte := GL.Types.Int'Value (aString (Pos2 + 2 .. Pos1 - 1));
      end;

      while not End_Of_File (Input_File) and not EOD loop
         declare
            aLine   : constant String := Get_Line (Input_File);
            Last    : constant Integer := aLine'Length;
            D_Start : Natural;
            D_End   : Natural;
            Double_Quote : Natural;
         begin
            D_Start := Fixed.Index (aLine, """");
            if D_Start > 0 then
               Double_Quote :=  Fixed.Index (aLine (D_Start + 1 .. Last), """""");
               if Double_Quote = 0 then
                  D_End := Fixed.Index (aLine (D_Start + 1 .. Last), """");
               else
                  D_End := Fixed.Index (aLine (Double_Quote + 2 .. Last), """");
               end if;
               if D_End > 0 then
--                    if Last > Longest then
--                       Longest := Last;
--                    end if;
--                    if Last < Shortest then
--                       Shortest := Last;
--                    end if;
                  EOD := aLine (D_Start + 1 .. D_End - 1) = "\0";
                  if not EOD then
                     Data.Data.Append
                       (To_Unbounded_String (aLine (D_Start + 1 .. D_End - 1)));
                     Put_Line (aLine (D_Start + 1 .. D_End - 1));
                  end if;
               end if;
            end if;
         end;
      end loop;
      Close (Input_File);

--        Put_Line ("MGL_Common.Read_SDL_File, Shortest and longest line lengths: " &
--                    Integer'Image(Shortest) & ", " & Integer'Image(Longest));
      Put_Line ("MGL_Common.Read_SDL_File, number of lines: " &
                  Positive'Image(Data.Data.Last_Index));
   exception
      when others =>
         Put_Line ("An exception occurred in Textures.Load_Texture");
         raise;
   end Read_SDL_File;

end MGL_Common;
