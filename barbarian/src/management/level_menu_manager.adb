
with Ada.Directories;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Game_Utils;
with Settings;
with Text;

package body Level_Menu_Manager is

   Num_Levels : Integer := 0;

   -- -------------------------------------------------------------------------

   function Get_Map_Name (Maps : Levels_List; Selected_Map_ID : Positive)
                          return String is
   begin
      if Maps.Is_Empty or else Selected_Map_ID > Maps.Last_Index then
         raise Levels_Maps_Manager_Exception with
           "Level_Menu_Manager.Get_Map_Name encountered an invalid Map ID: "
           & Integer'Image (Selected_Map_ID);
      end if;

      return To_String (Maps.Element (Selected_Map_ID).Map_Name);

   end Get_Map_Name;

   --  ------------------------------------------------------------------------

   procedure Init_Level_Maps (Maps                    : in out Levels_List;
                              Selected_Map_ID         : Positive;
                              Left_Margin_Cl, Top_Margin_Cl : Single) is
      use Settings;
      use Levels_Package;
      In_Cursor          : Cursor := Maps.First;
      Text_Height        : constant Single := 50.0 / Single (Framebuffer_Height);
      Text_Offset_Height : constant Single := 220.0 / Single (Framebuffer_Height);
      Name_Y             : Single;
      Out_Data           : Level_Map_Data;
      Count              : Single := 0.0;
   begin
      --        Put_Line ("Level_Menu_Manager.Init_Level_Maps initalizing Maps");
      Game_Utils.Game_Log ("---Level_Menu_Manager.Init_Level_Maps initalizing Maps---");
      if Maps.Is_Empty then
         raise Levels_Maps_Manager_Exception with
           "Level_Menu_Manager.Init_Level_Maps Maps List is empty.";
      end if;

      while Has_Element (In_Cursor) loop
         Out_Data := Element (In_Cursor);
         Name_Y := Top_Margin_Cl - 2.0 * Count * Text_Height - Text_Offset_Height;
         Count := Count + 1.0;
         declare
            Name      : constant String := To_String (Out_Data.Map_Name);
            Out_Name  : String := Name (1 .. Name'Length - 4);
         begin
            for index in Positive range 1 .. Out_Name'Length loop
               if (Out_Name (index) = '_' or Out_Name (index) = '-') then
                  Out_Name (index) := ' ';
               end if;
            end loop;

            if Out_Data.Locked then
               Out_Data.Map_Name_Text_ID :=
                 Text.Add_Text (Out_Name, Left_Margin_Cl, Name_Y,
                                20.0, 1.0, 1.0, 1.0, 1.0);
            else
               Out_Data.Map_Name_Text_ID :=
                 Text.Add_Text (Out_Name, Left_Margin_Cl, Name_Y,
                                20.0, 0.25, 0.25, 0.25, 1.0);
            end if;
            Text.Set_Text_Visible (Out_Data.Map_Name_Text_ID, False);
            Maps.Replace_Element (In_Cursor, Out_Data);
         end;  --  declare block
         Next (In_Cursor);
      end loop;
      --        Put_Line ("Level_Menu_Manager.Init_Level_Maps Maps size : " &
      --                 Ada.Containers.Count_Type'Image (Maps.Length));

      Text.Change_Text_Colour
        (Maps.Element (Selected_Map_ID).Map_Name_Text_ID, 1.0, 0.0, 1.0, 1.0);
      Game_Utils.Game_Log ("---Level_Menu_Manager.Init_Level_Maps Maps Initialized---");

   end Init_Level_Maps;

   --  ----------------------------------------------------------------------------

   procedure Load_Story (Path             : String;
                         theMap           : in out Level_Map_Data;
                         Has_Hammer_Track : out Boolean) is
      use Maps_Manager;
      Input_File       : File_Type;
      Num_Story_Lines  : Natural;
      Story_Lines      : Maps_Manager.Story_Lines_List;
   begin
      Put_Line ("Level_Menu_Manager.Load_Story loading " & Path);
      Open (Input_File, In_File, Path);
      theMap.Map_Name := To_Unbounded_String (Get_Line (Input_File));
      theMap.Par_Time := To_Unbounded_String (Get_Line (Input_File));
      --  Story
      declare
         aString  : constant String := Get_Line (Input_File);
         Num_Part : constant String := aString (13 .. aString'Length);
      begin
         Num_Story_Lines := Integer'Value (Num_Part);
      end;  --  declare block

      for line_num in 1 .. Num_Story_Lines loop
         declare
            aLine  : constant String := Get_Line (Input_File);
         begin
            Story_Lines.Append (To_Unbounded_String (aLine));
         end;  --  declare block
      end loop;

      declare
         aLine  : String := Get_Line (Input_File);
         Length : Integer := aLine'Length;
         Index  : Integer := 0;
      begin
         while index <= Length loop
            Index := Index + 1;
            if aLine (index) < Character'Val (32) then
               Length := Length - 1;
               for index2 in index .. Length loop
                  aLine (index) := aLine (index + 1);
               end loop;
            end if;
         end loop;
         theMap.Music_Track := To_Unbounded_String (aLine (1 .. Length));
      end;  --  declare block

      declare
         aLine  : String := Get_Line (Input_File);
         Length : Integer := aLine'Length;
         Index  : Integer := 0;
      begin
         while index <= Length loop
            Index := Index + 1;
            if aLine (index) < Character'Val (32) then
               Length := Length - 1;
               for index2 in index .. Length loop
                  aLine (index) := aLine (index + 1);
               end loop;
            end if;
         end loop;
         theMap.Hammer_Track := To_Unbounded_String (aLine (1 .. Length));
         Has_Hammer_Track := aLine'Length > 3;
      end;  --  declare block
      Close (Input_File);

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Level_Menu_Manager.Load_Story!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Load_Story;

   --  ----------------------------------------------------------------------------

   procedure Load_Story_Names (Path : String; Names : in out Levels_List) is
      Input_File : File_Type;
      Line_Count : Integer := 0;

      procedure Append_Data (Name : String; Lock : Boolean := True) is
         Data : Level_Map_Data;
      begin
         Data.Map_Name := To_Unbounded_String (Name);
         Data.Locked := Lock;
         Names.Append (Data);
      end Append_Data;

   begin
      --        Put_Line ("Levels_Maps_Manager.Load_Story_Names loading " & Path);
      Open (Input_File, In_File, Path);
      if not Is_Open (Input_File) then
         Names.Clear;
--           Game_Utils.Game_Log ("Levels_Maps_Manager.Load_Story_Names, no file " &
--                                  Path & " found -- locking all but first.");
         Append_Data ("introduction", False);
         Append_Data ("three_doors");
         Append_Data ("warlock");
         Append_Data ("winder");
         Append_Data ("under");
         Append_Data ("sky_temple");
         Append_Data ("hall");
         Append_Data ("attercoppe");
         Num_Levels := Integer (Length (Names));
      else
         while not End_Of_File (Input_File) loop
            declare
               aLine : constant String := Get_Line (Input_File);
            begin
               if aLine'Length < 2 then
                  Game_Utils.Game_Log ("WARNING: Load_Story_Names skipping short line " &
                                         aLine & " in maps list.");
               else
                  Game_Utils.Game_Log ("Level_Menu_Manager.Load_Story_Names, level name: " &
                                        aLine);
                  Append_Data (aLine, true);
                  Num_Levels := Num_Levels + 1;
               end if;
            end;
         end loop;
         Close (Input_File);
      end if;

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Level_Menu_Manager.Load_Story_Names!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Load_Story_Names;

   --  ----------------------------------------------------------------------------

   function Number_Of_Levels return Integer is
   begin
      return Num_Levels;
   end Number_Of_Levels;

   --  ----------------------------------------------------------------------------

end Level_Menu_Manager;
