
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Game_Utils;
--  with Settings;
--  with Text;

package body Selected_Map_Manager is

   -- -------------------------------------------------------------------------

   function Get_Map_Name (Maps : Selected_Maps_List; Selected_Map_ID : Positive)
                          return String is
      aMap    : Selected_Map_Data;
      Result  : String := "";
   begin
      if not Maps.Is_Empty and then Selected_Map_ID <= Maps.Last_Index then
         aMap := Maps.Element (Selected_Map_ID);
         Result := To_String (aMap.Map_Title);
      else
         Game_Utils.Game_Log
           ("Selected_Map_Manager.Get_Map_Name " &
              "encountered an invalid Map ID: " & Positive'Image (Selected_Map_ID));
      end if;

      return Result;
   end Get_Map_Name;

   --  ------------------------------------------------------------------------

   procedure Load_Map (Path             : String; theMap : in out Selected_Map_Data;
                       Has_Hammer_Track : out Boolean) is
      use Ada.Streams;
      Input_File       : File_Type;
      Line_Length      : Integer;
      Num_Story_Lines  : Natural;
   begin
      Open (Input_File, In_File, Path);
      theMap.Map_Title := To_Unbounded_String (Get_Line (Input_File));
      Line_Length := Length (theMap.Map_Title);
      for index in 1 .. Line_Length - 1 loop
         if Slice (theMap.Map_Title, index, index + 1) = "\r" or
           Slice (theMap.Map_Title, index, index + 1) = "\n" then
            Delete  (theMap.Map_Title, index, index + 1);
            Line_Length := Line_Length - 2;
         end if;
      end loop;

      theMap.Par_Time := To_Unbounded_String (Get_Line (Input_File));

      --  Story
      declare
         aString  : constant String := Get_Line (Input_File);
         Num_Part : constant String := aString (13 .. aString'Length);
      begin
         Num_Story_Lines := Integer'Value (Num_Part);
         for line_num in 1 .. Num_Story_Lines loop
            declare
               Story_Line  : constant String := Get_Line (Input_File);
            begin
               theMap.Map_Intro_Text := theMap.Map_Intro_Text & " " & Story_Line;
            end;  --  declare block
         end loop;
      end;  --  declare block

      theMap.Music_Track := To_Unbounded_String (Get_Line (Input_File));
      theMap.Hammer_Track := To_Unbounded_String (Get_Line (Input_File));
      Has_Hammer_Track := Length (theMap.Hammer_Track) > 3;

      Close (Input_File);

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Selected_Map_Manager.Load_Map!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Load_Map;

   --  ------------------------------------------------------------------------

   function Map_Locked (aMap : Selected_Map_Data) return Boolean is
   begin
      return aMap.Locked;
   end Map_Locked;

   --  ------------------------------------------------------------------------

   procedure Set_Map_Lock (aMap : in out Selected_Map_Data; Lock : Boolean) is
   begin
      aMap.Locked := Lock;
   end Set_Map_Lock;

   --  ------------------------------------------------------------------------

end Selected_Map_Manager;
