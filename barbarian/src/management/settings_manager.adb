
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Input_Handler;

package body Settings_Manager is

   --  ------------------------------------------------------------------------

   function Load_Settings return Boolean is
      use Ada.Strings;
      Input_File       : File_Type;
   begin
      Open (Input_File, In_File, "src/settings.cfg");
      while not End_Of_File (Input_File) loop
         declare
            aLine          : constant String := Get_Line (Input_File);
            Last           : constant Integer := aLine'Length;
            Pos            : constant Natural := Fixed.Index (aLine, "_");
            Head           : constant String := aLine (1 .. Pos);
            Tail           : constant String := aLine (Pos + 1 .. Last);
         begin
            if Head = "KEY_" then
               Input_Handler.r;
            elsif Head = "Joy_" then
               null;
            elsif Head = "GFX_" then
               null;
            elsif Head = "AUD_" then
               null;
            end if;
         end;  --  declare block
      end loop;
      Close (Input_File);
      return True;
   end Load_Settings;

   --  ------------------------------------------------------------------------

end Settings_Manager;
