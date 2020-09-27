
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

package body Game_Utils is

   Game_Log_File : constant String := "game.log";
   Log_File      : File_Type;

   --  ------------------------------------------------------------------------

   --     function Check_Param (Check : String) return Integer is
   --     begin
   --          return 0;
   --     end Check_Param;

   --  -------------------------------------------------------------------------

   procedure Close_Game_Log is
   begin
      Close (Log_File);
   end Close_Game_Log;

   --  -------------------------------------------------------------------------

   procedure Game_Log (Message : String) is
      File_Descriptor  : Ada.Text_IO.File_Type;
      Log_Directory    : constant String := ".";
      Directory_Set    : Boolean := False;
   begin
      Set_Directory (Log_Directory);
      Directory_Set := True;
      if Exists (Game_Log_File) then
         Open (File_Descriptor, Append_File, Game_Log_File);
      else
         Create (File_Descriptor, Append_File, Game_Log_File);
      end if;
      Put_Line (File_Descriptor, Message);
      Close (File_Descriptor);

   exception
      when anError : Ada.IO_Exceptions.Name_Error  =>
         if not Directory_Set then
            Put_Line ("Game_Utils.Game_Log, there is no directory " &
                        Log_Directory);
         end if;
         Put_Line (Ada.Exceptions.Exception_Information (anError));

      when anError : others =>
         Put_Line ("An exception occurred in Game_Utils.Game_Log! ");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Game_Log;

   --  ------------------------------------------------------------------------

   --      function Is_little_endian return Boolean is
   --          use Interfaces.C;
   --  	Test     : constant array (1 .. 2) of unsigned_char := (1, 0);
   --  	X        : constant short := short (Test);
   --      begin
   --  	return 1 = X;
   --      end Is_little_endian;

   --  ------------------------------------------------------------------------

   function Max (L, R : Integer) return Integer is
      Result : Integer;
   begin
      if L > R then
         Result := L;
      else
         Result := R;
      end if;
      return Result;
   end Max;

   --  ------------------------------------------------------------------------

   procedure Restart_Game_Log is
   begin
      Set_Directory (".");
      if Is_Open (Log_File) then
         Close (Log_File);
      end if;
      Delete_File (Game_Log_File);
      Create (Log_File, Out_File, Game_Log_File);
      Close (Log_File);

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Game_Utils.Restart_Game_Log! ");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Restart_Game_Log;

   --  ------------------------------------------------------------------------

end Game_Utils;
