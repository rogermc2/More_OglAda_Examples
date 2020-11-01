
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Game_Utils;
with Settings;
with Text;

package body Custom_Maps_Manager is

   -- -------------------------------------------------------------------------

   function Get_Custom_Map_Name (Custom_Maps  : Custom_Maps_List;
                                 Selected_Map : Positive) return String is
      aMap    : Custom_Data;
      Result  : String := "";
   begin
      if not Custom_Maps.Is_Empty and then
        Selected_Map <= Custom_Maps.Last_Index then
         aMap := Custom_Maps.Element (Selected_Map);
         Result := To_String (aMap.Name);
      else
         Game_Utils.Game_Log
           ("Custom_Maps_Manager.Get_Custom_Map_Name " &
              "encountered an invalid Map ID: " & Integer'Image (Selected_Map));
      end if;

      return Result;
   end Get_Custom_Map_Name;

   --  ------------------------------------------------------------------------

   procedure Load_Custom_Map (Path            : String;
                              Custom_Maps     : in out Custom_Maps_List;
                              Top_Margin_Cl, Left_Margin_Cl,
                              Text_Height     : Single;
                              Num_Custom_Maps : in out Integer) is
      use Ada.Streams;
      Input_File         : File_Type;
      Data               : Custom_Data;
      Text_Height_Offset : constant Single :=
                             220.0 / Single (Settings.Framebuffer_Height);
      Mname_Y            : constant Single :=
                             Top_Margin_Cl - 2.0 * Text_Height *
                               Single (Num_Custom_Maps) - Text_Height_Offset;
   begin
      Open (Input_File, In_File, Path);
      while not End_Of_File (Input_File) loop
         declare
            aLine        : String := Get_Line (Input_File);
            Line_Length  : constant Integer := aLine'Length;
         begin
            if Line_Length < 2 then
               Game_Utils.Game_Log ("WARNING: Load_Custom_Map skipping short line " &
                                      aLine & " in custom names list.");
            else
               for index in 1 .. Line_Length loop
                  if aLine (index) = '_'  then
                     aLine (index) := ' ';
                  end if;
               end loop;
               Data.Name := To_Unbounded_String (aLine);
               Data.Text_ID := Text.Add_Text
                 (aLine, Left_Margin_Cl, Mname_Y,
                  25.0, 1.0, 1.0, 1.0, 1.0);
               Text.Set_Text_Visible (Data.Text_ID, False);

--                 Custom_Maps.Replace_Element (Num_Custom_Maps, Data);
               Custom_Maps.Append (Data);
               Num_Custom_Maps := Custom_Maps.Last_Index;
            end if;
         end;  -- declare block
      end loop;
      Close (Input_File);

   exception
      when anError : others =>
         Put_Line
           ("An exception occurred in Load_Custom_Maps_Manager.Load_Custom_Map!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Load_Custom_Map;

   --  ----------------------------------------------------------------------------

   procedure Replace_Custom_Map (Path        : String; Maps : in out Custom_Maps_List;
                                 Top_Margin_Cl, Left_Margin_Cl,
                                 Text_Height : Single; Map_ID : Positive) is
      use Ada.Streams;
      Input_File         : Stream_IO.File_Type;
      Input_Stream       : Stream_IO.Stream_Access;
      aLine              : Unbounded_String;
      Line_Length        : Integer;
      Data               : Custom_Data;
      Text_Height_Offset : constant Single :=
                             220.0 / Single (Settings.Framebuffer_Height);
      Mname_Y            : constant Single :=
                             Top_Margin_Cl - 2.0 * Text_Height *
                               Single (Map_ID) - Text_Height_Offset;
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

            Maps.Replace_Element (Map_ID, Data);
         end if;
      end loop;

      Stream_IO.Close (Input_File);

   exception
      when anError : others =>
         Put_Line
           ("An exception occurred in Load_Custom_Maps_Manager.Replace_Custom_Map!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Replace_Custom_Map;

   --  ------------------------------------------------------------------------

end Custom_Maps_Manager;
