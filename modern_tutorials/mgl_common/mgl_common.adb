
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
         Data.Pitch := GL.Types.Int'Value (aString (Pos2 + 2 .. Pos1 - 1));
      end;

      while not End_Of_File (Input_File) loop
         declare
            use GL.Types;
            aLine        : constant String := Get_Line (Input_File);
            Last         : constant Integer := aLine'Length;
            D_Start      : Natural;
            D_End        : Natural;
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
               Data.Data.Append
                 (To_Unbounded_String (aLine (D_Start + 1 .. D_End - 1)));

            end if;
         end;
      end loop;
      Close (Input_File);

   exception
      when others =>
         Put_Line ("An exception occurred in MGL_Common.Read_SDL_File");
         raise;
   end Read_SDL_File;

   --  -------------------------------------------------------------------------

   function To_UByte (Decimal : String) return GL.Types.UByte is
      use GL.Types;
      Last   : constant Integer := Decimal'Last;
      Power  : Integer := 0;
      Result : UByte := 0;
   begin
      for index in reverse Decimal'First .. Decimal'Last loop
         Result := Result +
           UByte'Value (Decimal (index .. index)) * 8 ** Power;
         Power := Power + 1;
      end loop;
      return Result;
   end To_UByte;

   --  -------------------------------------------------------------------------

end MGL_Common;
