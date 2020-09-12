
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Game_Utils;
with Settings;
with Text;

package body Text_Manager is

    -- -------------------------------------------------------------------------

    procedure Preload_Comic_Texts (Path : String) is
        use Ada.Streams;
        Input_File       : Stream_IO.File_Type;
        Input_Stream     : Stream_IO.Stream_Access;
        aLine            : Unbounded_String;
        Line_Length      : Integer;
        Data             : Custom_Data;
        Text_Height_Offset : constant Float :=
                               220.0 / Float (Settings.Framebuffer_Height);
        Mname_Y            : constant Float :=
                               Top_Margin_Cl - 2.0 * Text_Height *
                               Float (Num_Custom_Maps) - Text_Height_Offset;
    begin
        Stream_IO.Open (Input_File, Stream_IO.In_File, Path);
        Input_Stream := Stream_IO.Stream (Input_File);
        while not Stream_IO.End_Of_File (Input_File) loop
            Unbounded_String'Read (Input_Stream, aLine);
            Line_Length := Length (aLine);
            if Line_Length < 2 then
                Game_Utils.Game_Log ("WARNING: skipping short line " &
                                       To_String (aLine) & " in custom names list.");
            else
                for index in 1 .. Line_Length loop
                    if Element (aLine, index) = '_'  then
                        Replace_Element (aLine, index, ' ');
                    end if;
                end loop;
                Data.Name := aLine;
                Data.Text_ID := Text.Add_Text
                  (To_String (aLine), Left_Margin_Cl, Mname_Y,
                   25.0, 1.0, 1.0, 1.0, 1.0);
                Text.Set_Text_Visible (Data.Text_ID, False);

                Maps.Replace_Element (Num_Custom_Maps, Data);
                Num_Custom_Maps := Num_Custom_Maps + 1;
            end if;
        end loop;

        Stream_IO.Close (Input_File);

    exception
        when anError : others =>
            Put_Line
              ("An exception occurred in Text_Manager.Preload_Comic_Texts!");
            Put_Line (Ada.Exceptions.Exception_Information (anError));
    end Preload_Comic_Texts;

    --  ----------------------------------------------------------------------------

end Text_Manager;
