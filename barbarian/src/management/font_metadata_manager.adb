
--  with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Game_Utils;

package body Font_Metadata_Manager is

   type Metadata is record
      Ascii_Code : Integer := -1;
      X_Min      : Single := 0.0;
      Width      : Single := 0.0;
      Y_Min      : Single := 0.0;
      Height     : Single := 0.0;
      Y_Offset   : Single := 0.0;
   end record;

   -- -------------------------------------------------------------------------
   --  Derived from text.cpp.load_font_meta
   procedure Load_Metadata (Path : String; Glyphs : out Glyph_Array) is
      use Ada.Strings;
      Input_File   : File_Type;
      First_Line   : Unbounded_String;
      Meta_Record  : Metadata;
      Ascii_Code   : Integer;
   begin
      Game_Utils.Game_Log ("loading font meta-data from file " &
                             Path);
      Open (Input_File, In_File, Path);
      First_Line := To_Unbounded_String (Get_Line (Input_File));
      while not End_Of_File (Input_File) loop
         declare
            Metadata      : constant String := Get_Line (Input_File);
            Last          : constant Positive := Metadata'Length;
            Prop_Xmin     : Single := 0.0;
            Prop_Width    : Single := 0.0;
            Prop_Ymin     : Single := 0.0;
            Prop_Height   : Single := 0.0;
            Prop_Y_Offset : Single := 0.0;
            Pos1          : Positive;
            Pos2          : Natural;
         begin
--              Game_Utils.Game_Log ("Font meta-data: " & Metadata);
            Pos1 := Fixed.Index (Metadata, " ");
            Ascii_Code := Integer'Value (Metadata (1 .. Pos1 - 1));
            Pos2 := Fixed.Index (Metadata (Pos1 + 1 .. Last), " ");
            Prop_Xmin := Single'Value (Metadata (Pos1 + 1 .. Pos2 - 1));
            Pos1 := Fixed.Index (Metadata (Pos2 + 1 .. Last), " ");
            Prop_Width := Single'Value (Metadata (Pos2 + 1 .. Pos1 - 1));
            Pos2 := Fixed.Index (Metadata (Pos1 + 1 .. Last), " ");
            Prop_Ymin := Single'Value (Metadata (Pos1 + 1 .. Pos2 - 1));
            Pos1 := Fixed.Index (Metadata (Pos2 + 1 .. Last), " ");
            Prop_Height := Single'Value (Metadata (Pos2 + 1 .. Pos1 - 1));
            Pos2 := Fixed.Index (Metadata (Pos1 + 1 .. Last), "/");
            if Pos2 = 0 then
               Pos2 := Fixed.Index (Metadata (Pos1 + 1 .. Last), " ");
               if Pos2 = 0 then
                  Prop_Y_Offset := Single'Value (Metadata (Pos1 + 1 .. Last));
               else
                  Prop_Y_Offset := Single'Value (Metadata (Pos1 + 1 .. Pos2 - 1));
               end if;
            end if;

            Glyphs (Ascii_Code).Width := Prop_Width;
            Glyphs (Ascii_Code).Y_Offset := 1.0 - Prop_Height - Prop_Y_Offset;
         end;
      end loop;
      Close (Input_File);
      Game_Utils.Game_Log ("Font meta-data loaded.");

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Font_Metadata_Manager.Load_Metadata!");
         Put_Line (Ada.Exceptions.Exception_Information (anError));
   end Load_Metadata;

   --  ----------------------------------------------------------------------------

   function Width (Glyphs : Glyph_Array; Index : Integer) return Single is
   begin
      return Glyphs (Index).Width;
   end Width;

   --  ----------------------------------------------------------------------------

   function Y_Offset (Glyphs : Glyph_Array; Index : Integer) return Single is
   begin
      return Glyphs (Index).Y_Offset;
   end Y_Offset;

   --  ----------------------------------------------------------------------------

end Font_Metadata_Manager;
