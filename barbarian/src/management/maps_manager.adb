
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Character_Controller;
with Manifold;
with Properties_Manager;
with Text_Manager;

package body Maps_Manager is

    procedure Load_Maps (Path : String; theMap : out Map;
                        Current_Pos : out Integer) is
        use Ada.Streams;
        Input_File       : Stream_IO.File_Type;
        Input_Stream     : Stream_IO.Stream_Access;
        aLine            : Unbounded_String;
        Num_Story_Lines  : Natural;
        Story_Lines      : Story_Lines_List;
    begin
        Stream_IO.Open (Input_File, Stream_IO.In_File, Path);
        Input_Stream := Stream_IO.Stream (Input_File);
        Unbounded_String'Read (Input_Stream, theMap.Level_Title);
        Unbounded_String'Read (Input_Stream, theMap.Level_Par_Time);

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

        Unbounded_String'Read (Input_Stream, theMap.Music_Track);
        Unbounded_String'Read (Input_Stream, theMap.Hammer_Music_Track);

        Manifold.Load_Tiles (Input_Stream);

        Properties_Manager.Load_Properties
          (Input_Stream, Stream_IO.Index (Input_File));
        Current_Pos := Integer (Stream_IO.Index (Input_File));

        Character_Controller.Init;
        Character_Controller.Load_Characters (Input_Stream, False);

        Text_Manager.Preload_Comic_Texts  (Input_Stream);


    exception
        when anError : others =>
            Put_Line ("An exception occurred in Maps_Manager.Load_Maps!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Load_Maps;

    --  ----------------------------------------------------------------------------

    procedure Set_Title (aMap : in out Map; Title : Unbounded_String) is
    begin
        aMap.Level_Title := Title;
    end Set_Title;

    --  ----------------------------------------------------------------------------

    procedure Set_Par_Time (aMap : in out Map; Time : Unbounded_String) is
    begin
        aMap.Level_Title := Time;
    end Set_Par_Time;

    --  ----------------------------------------------------------------------------

    procedure Set_Story_Lines (aMap : in out Map; Lines : Unbounded_String) is
    begin
        aMap.Story_Lines := Lines;
    end Set_Story_Lines;

    --  ----------------------------------------------------------------------------

    procedure Set_Music_Track (aMap : in out Map; Track : Unbounded_String) is
    begin
        aMap.Music_Track := Track;
    end Set_Music_Track;

    --  ----------------------------------------------------------------------------

    procedure Set_Hammer_Music_Track (aMap : in out Map; Track : Unbounded_String) is
    begin
        aMap.Hammer_Music_Track := Track;
    end Set_Hammer_Music_Track;

    --  ----------------------------------------------------------------------------

end Maps_Manager;
