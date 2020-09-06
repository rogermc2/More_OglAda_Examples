
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

    procedure Load_Map (Path : String; theMap : in out Selected_Map_Data;
                        Has_Hammer_Track : out Boolean) is
        use Ada.Streams;
        Input_File       : Stream_IO.File_Type;
        Input_Stream     : Stream_IO.Stream_Access;
        aLine            : Unbounded_String;
        Line_Length      : Integer;
        Num_Story_Lines  : Natural;
--          Story_Lines      : Maps_Manager.Story_Lines_List;
    begin
        Stream_IO.Open (Input_File, Stream_IO.In_File, Path);
        Input_Stream := Stream_IO.Stream (Input_File);
        Unbounded_String'Read (Input_Stream, theMap.Map_Title);
        Line_Length := Length (theMap.Map_Title) - 1;
            for index in 1 .. Line_Length loop
                if Slice (theMap.Map_Title, index, index + 1) = "\r" or
                 Slice (theMap.Map_Title, index, index + 1) = "\n" then
                    Delete  (theMap.Map_Title, index, index + 1);
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
                theMap.Map_Intro_Text := theMap.Map_Intro_Text & aLine;
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
