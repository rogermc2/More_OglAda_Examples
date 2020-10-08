
with Ada.Directories;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Game_Utils;
with Settings;
with Text;

package body Levels_Maps_Manager is

   --      Num_Official_Maps : constant Integer := 8;
   Num_Maps          : Integer := 0;

   -- -------------------------------------------------------------------------

   function Get_Map_Name (Maps : Maps_List; Selected_Map_ID : Positive)
                          return String is
   begin
      if Maps.Is_Empty or else Selected_Map_ID > Maps.Last_Index then
         raise Levels_Maps_Manager_Exception with
           "Levels_Maps_Manager.Get_Map_Name encountered an invalid Map ID: "
           & Integer'Image (Selected_Map_ID);
      end if;

      return To_String (Maps.Element (Selected_Map_ID).Map_Name);

   end Get_Map_Name;

   --  ------------------------------------------------------------------------

   procedure Init_Maps (Maps  : in out Maps_List; Selected_Map_ID : Positive;
                        Left_Margin_Cl, Top_Margin_Cl : Single) is
      use Settings;
      use Maps_Package;
      In_Cursor          : Cursor := Maps.First;
      --          Out_Cursor         : Cursor;
      Text_Height        : constant Single := 50.0 / Single (Framebuffer_Height);
      Text_Offset_Height : constant Single := 220.0 / Single (Framebuffer_Height);
      Name_Y             : Single;
      Out_Data           : Level_Map_Data;
      Count              : Single := 0.0;
   begin
      Put_Line ("Levels_Maps_Manager.Init_Maps initalizing Maps");
      Game_Utils.Game_Log ("---Levels_Maps_Manager.Init_Maps initalizing Maps---");
      if Maps.Is_Empty then
         raise Levels_Maps_Manager_Exception with
           "Levels_Maps_Manager.Init_Maps Maps List is empty.";
      end if;

      while Has_Element (In_Cursor) loop
         Out_Data := Element (In_Cursor);
         Name_Y := Top_Margin_Cl - 2.0 * Count * Text_Height - Text_Offset_Height;
         Count := Count + 1.0;
         declare
            Map_Index : Positive;
            Name      : String := To_String (Out_Data.Map_Name);
         begin
            Map_Index := To_Index (In_Cursor);
            for index in Positive range 1 .. Name'Length loop
               if index = Map_Index and Name (index) = '_' then
                  Name (index) := ' ';
               end if;
            end loop;

            Out_Data.Map_Name := To_Unbounded_String (Name);
            if Out_Data.Locked then
               Out_Data.Map_Name_Text_ID :=
                 Text.Add_Text (Name, Left_Margin_Cl, Name_Y,
                                25.0, 1.0, 1.0, 1.0, 1.0);
            else
               Out_Data.Map_Name_Text_ID :=
                 Text.Add_Text (Name, Left_Margin_Cl, Name_Y,
                                25.0, 0.25, 0.25, 0.25, 1.0);
            end if;
            Text.Set_Text_Visible (Out_Data.Map_Name_Text_ID, False);
            Maps.Replace_Element (Map_Index, Out_Data);
         end;  --  declare block
         Next (In_Cursor);
      end loop;
      Put_Line ("Levels_Maps_Manager.Init_Maps Maps size : " &
               Ada.Containers.Count_Type'Image (Maps.Length));

      Text.Change_Text_Colour
        (Maps.Element (Selected_Map_ID).Map_Name_Text_ID, 1.0, 0.0, 1.0, 1.0);
      Game_Utils.Game_Log ("---Levels_Maps_Manager.Init_Maps Maps Initialized---");

   end Init_Maps;

   --  ----------------------------------------------------------------------------

   procedure Load_Map (Path             : String; theMap : in out Level_Map_Data;
                       Has_Hammer_Track : out Boolean) is
      use Ada.Streams;
      use Maps_Manager;
      Input_File       : Stream_IO.File_Type;
      Input_Stream     : Stream_IO.Stream_Access;
      aLine            : Unbounded_String;
      Line_Length      : Integer;
      Num_Story_Lines  : Natural;
      Story_Lines      : Maps_Manager.Story_Lines_List;
   begin
      Put_Line ("Levels_Maps_Manager.Load_Map loading " & Path);
      Stream_IO.Open (Input_File, Stream_IO.In_File, Path);
      Input_Stream := Stream_IO.Stream (Input_File);
      Unbounded_String'Read (Input_Stream, theMap.Map_Name);
      Line_Length := Length (theMap.Map_Name) - 1;
      for index in 1 .. Line_Length loop
         if Slice (theMap.Map_Name, index, index + 1) = "\r" or
           Slice (theMap.Map_Name, index, index + 1) = "\n" then
            Delete  (theMap.Map_Name, index, index + 1);
            Line_Length := Line_Length - 2;
         end if;
      end loop;

      Unbounded_String'Read (Input_Stream, theMap.Par_Time);

      --  Story
      Unbounded_String'Read (Input_Stream, aLine);
      declare
         aString  : constant String := To_String (aLine);
         Num_Part : constant String := aString (13 .. aString'Length);
      begin
         Num_Story_Lines := Integer'Value (Num_Part);
         for line_num in 1 .. Num_Story_Lines loop
            Unbounded_String'Read (Input_Stream, aLine);
            Story_Lines.Append (aLine);
         end loop;
      end;  --  declare block

      Unbounded_String'Read (Input_Stream, aLine);
      for index in 1 .. Length (aLine) loop
         if Element (aLine, index) < Character'val (32) then
            Replace_Element (aLine, index, ASCII.NUL);
         end if;
      end loop;
      theMap.Music_Track := aLine;

      Unbounded_String'Read (Input_Stream, aLine);
      for index in 1 .. Length (aLine) loop
         if Element (aLine, index) < Character'val (32) then
            Replace_Element (aLine, index, ASCII.NUL);
         end if;
      end loop;
      theMap.Hammer_Track := aLine;
      Has_Hammer_Track := Length (aLine) > 3;
      Stream_IO.Close (Input_File);
   exception
      when anError : others =>
         Put_Line ("An exception occurred in Levels_Maps_Manager.Load_Map!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Load_Map;

   --  ----------------------------------------------------------------------------

   procedure Load_Names (Path : String; Names : in out Maps_List) is
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
      Put_Line ("Levels_Maps_Manager.Load_Names loading " & Path);
      Open (Input_File, In_File, Path);
      if not Is_Open (Input_File) then
         Names.Clear;
         Append_Data ("introduction", False);
         Append_Data ("three_doors");
         Append_Data  ("warlock");
         Append_Data  ("winder");
         Append_Data ("under");
         Append_Data ("sky_temple");
         Append_Data  ("hall");
         Append_Data  ("attercoppe");
         Num_Maps := Integer (Length (Names));
      else
         while not End_Of_File (Input_File) loop
            declare
               aLine : String := Get_Line (Input_File);
            begin
               if aLine'Length < 2 then
                  Game_Utils.Game_Log ("WARNING: Load_Names skipping short line " &
                                         aLine & " in maps list.");
               else
                  Append_Data (aLine, true);
                  Num_Maps := Num_Maps + 1;
               end if;
            end;
         end loop;
         Close (Input_File);
      end if;

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Levels_Maps_Manager.Load_Names!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Load_Names;

   --  ----------------------------------------------------------------------------

   function Number_Of_Maps return Integer is
   begin
      return Num_Maps;
   end Number_Of_Maps;

   --  ----------------------------------------------------------------------------

end Levels_Maps_Manager;
