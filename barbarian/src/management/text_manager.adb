
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types.Colors;


package body Text_Manager is
   use GL.Types;
   use GL.Types.Colors;

   type Coloured_Text is record
      Text   : Unbounded_String := To_Unbounded_String ("");
      Colour : Color;
   end record;

   package Coloured_Text_Package is new
     Ada.Containers.Vectors (Positive, Coloured_Text);
   type Coloured_Text_List is new Coloured_Text_Package.Vector with null record;

   type Popup_Data is record
      Rx          : Integer;
      RGBA        : GL.Types.Colors.Color;
      Popup_Text  : Unbounded_String := To_Unbounded_String ("");
   end record;

   Preloaded_Comic_Texts   : Coloured_Text_List;

   -- -------------------------------------------------------------------------

   procedure Preload_Comic_Texts (Input_File : File_Type) is
      use Ada.Strings;
      use GL.Types;
      aLine       : constant String := Get_Line (Input_File);
      Last        : Integer := Integer (aLine'Length);
      Pos1        : Natural := Fixed.Index (aLine, " ");
      Pos2        : Natural;
      Popup_Count : Integer := 0;

      function Process_Text (theText : String) return Unbounded_String is
         Last      : Integer := theText'Length;
         Formatted : Unbounded_String := To_Unbounded_String ("");
         I_Index   : Integer := 1;
      begin
         while I_Index <= Last loop
            if I_Index < Last - 1 and then
              theText (I_Index .. I_Index + 1) = "\n" then
                  Append (Formatted, ASCII.LF);
                  Append (Formatted, ASCII.CR);
                  I_Index := I_Index + 2;
            else
               Append (Formatted, theText (I_Index));
               I_Index := I_Index + 1;
            end if;
         end loop;
         return Formatted;
      end Process_Text;

   begin
      Preloaded_Comic_Texts.Clear;
      if aLine (1 .. Pos1 - 1) /= "popups" then
         raise Text_Manager_Exception with
           "Text_Manager.Preload_Comic_Texts; Invalid Comic_Texts format: "
           & aLine;
      end if;

      Popup_Count := Integer'Value (aLine (Pos1 + 1 .. Last));
      for index in 1 .. Popup_Count loop
         declare
            Pop_Line   : constant String := Get_Line (Input_File);
            J_Index    : Integer := 0;
            R          : Single := 1.0;
            G          : Single := 1.0;
            B          : Single := 1.0;
            A          : Single := 1.0;
            Rx         : Integer;
            Data       : Coloured_Text;
         begin
            Last := Integer (Pop_Line'Length);
            Pos1 := Fixed.Index (Pop_Line, " ");
            Rx := Integer'Value (Pop_Line (1 .. Pos1 - 1));
            Pos2 := Fixed.Index (Pop_Line (Pos1 + 1 .. Last), "(");
            Pos1 := Fixed.Index (Pop_Line, " ");
            R := Single'Value (Pop_Line (Pos2 + 1 .. Pos1 - 1));
            Pos1 := Fixed.Index (Pop_Line (Pos2 + 1 .. Last), " ");
            Pos2 := Fixed.Index (Pop_Line (Pos1 + 1 .. Last), " ");
            G := Single'Value (Pop_Line (Pos1 + 1 .. Pos2 - 1));
            Pos2 := Fixed.Index (Pop_Line (Pos1 + 1 .. Last), " ");
            Pos1 := Fixed.Index (Pop_Line, " ");
            B := Single'Value (Pop_Line (Pos2 + 1 .. Pos1 - 1));
            Pos1 := Fixed.Index (Pop_Line (Pos2 + 1 .. Last), " ");
            Pos2 := Fixed.Index (Pop_Line (Pos1 + 1 .. Last), ")");
            A := Single'Value (Pop_Line (Pos1 + 1 .. Pos2 - 1));
            Data := (Process_Text (Pop_Line), (R, G, B ,A));
            Preloaded_Comic_Texts.Append (Data);
            --  register rx code
            --  if (!gEventController.addReceiver (rx, RX_STORY,
            --  g_preloaded_comic_colours.size () - 1))
         end;
      end loop;

   exception
      when anError : others =>
         Put_Line
           ("An exception occurred in Text_Manager.Preload_Comic_Texts!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Preload_Comic_Texts;

   --  ----------------------------------------------------------------------------

end Text_Manager;
