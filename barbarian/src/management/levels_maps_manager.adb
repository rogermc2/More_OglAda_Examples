
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

   function Get_Map_Name (Maps : Maps_List; Selected_Map : Positive)
                           return String is
      aMap    : Level_Map_Data;
      Result  : String := "";
   begin
      if not Maps.Is_Empty and then Selected_Map <= Maps.Last_Index then
         aMap := Maps.Element (Selected_Map);
         Result := To_String (aMap.Map_Name);
      else
         Game_Utils.Game_Log
           ("Levels_Maps_Manager.Get_Map_Name " &
              "encountered an invalid Map ID: " & Integer'Image (Selected_Map));
      end if;

      return Result;
   end Get_Map_Name;

   --  ------------------------------------------------------------------------

   procedure Init_Maps (From                          : Maps_List; To          : in out Maps_List;
                        Left_Margin_Cl, Top_Margin_Cl : Single) is
      use Settings;
      use Maps_Package;
      In_Cursor          : Cursor := From.First;
      --          Out_Cursor         : Cursor;
      Text_Height        : constant Single := 50.0 / Single (Framebuffer_Height);
      Text_Offset_Height : constant Single := 220.0 / Single (Framebuffer_Height);
      Name_Y             : Single;
      In_Data            : Level_Map_Data;
      Out_Data           : Level_Map_Data;
      Count              : Single := 0.0;
   begin
      Put_Line ("Levels_Maps_Manager.Init_Maps initalizing To Maps");
      Game_Utils.Game_Log ("---Levels_Maps_Manager.Init_Maps initalizing To Maps---");
      if From.Is_Empty then
         raise Levels_Maps_Manager_Exception with
           "Levels_Maps_Manager.Init_Maps From Maps_List is empty.";
      end if;

      while Has_Element (In_Cursor) loop
         Put_Line ("Levels_Maps_Manager.Init_Maps Adding Map");
         In_Data := Element (In_Cursor);
         To.Append (In_Data);
         --              Out_Cursor := To.Last;
         Out_Data := To.Last_Element;
         Name_Y := Top_Margin_Cl - 2.0 * Count * Text_Height - Text_Offset_Height;
         Count := Count + 1.0;
         declare
            Map_Index : Positive;
            Name      : String := To_String (In_Data.Map_Name);
         begin
            Map_Index := To_Index  (In_Cursor);
            for index in Positive range 1 .. Name'Length loop
               if index = Map_Index and Name (index) = '_' then
                  Name (index) := ' ';
               end if;
            end loop;
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
         end;  --  declare block
         Next (In_Cursor);
      end loop;

      Text.Change_Text_Colour
        (Element (To.First).Map_Name_Text_ID, 1.0, 0.0, 1.0, 1.0);
      Game_Utils.Game_Log ("---Levels_Maps_Manager.Init_Maps To Maps Initialized---");

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
      use Ada.Streams;
      Input_File       : Stream_IO.File_Type;
      Input_Stream     : Stream_IO.Stream_Access;
      aLine            : Unbounded_String;
      Line_Count       : Integer := 0;
      Lock             : Boolean;
      procedure Append_Data (Name : String; Lock : Boolean := True) is
         Data : Level_Map_Data;
      begin
         Data.Map_Name := To_Unbounded_String (Name);
         Data.Locked := Lock;
         Names.Append (Data);
      end Append_Data;
   begin
      Put_Line ("Levels_Maps_Manager.Load_Names loading " & Path);
      Stream_IO.Open (Input_File, Stream_IO.In_File, Path);
      if not Stream_IO.Is_Open (Input_File) then
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
         Input_Stream := Stream_IO.Stream (Input_File);
         while not Stream_IO.End_Of_File (Input_File) loop
            Unbounded_String'Read (Input_Stream, aLine);
            Line_Count := Line_Count + 1;
            if Length (aLine) < 2 then
               Game_Utils.Game_Log ("WARNING: skipping short line " &
                                      To_String (aLine) & " in maps list.");
            else
               Boolean'Read (Input_Stream, Lock);
               Append_Data (To_String (aLine), Lock);
               Line_Count := Line_Count + 1;
            end if;
         end loop;
         Stream_IO.Close (Input_File);
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
