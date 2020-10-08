
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Images;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Pixels;

with Utilities;

with Game_Utils;
with Font_Metadata_Manager;
with Shader_Attributes;
with Text_Box_Shader_Manager;
with Text_Shader_Manager;

package body Text is

   type Renderable_Text is record
      VAO            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Points_VBO     : GL.Objects.Buffers.Buffer;
      Tex_Coords_VBO : GL.Objects.Buffers.Buffer;
      Top_Left_X     : Single := 0.0;
      Top_Left_Y     : Single := 0.0;
      Bottom_Right_X : Single := 0.0;
      Bottom_Right_Y : Single := 0.0;
      Size_Px        : Single := 0.0;
      Red            : Single := 0.0;
      Green          : Single := 0.0;
      Blue           : Single := 0.0;
      A              : Single := 0.0;
      Point_Count    : Integer := 0;
      Visible        : Boolean := False;
   end record;

   package Renderable_Texts_Package is new Ada.Containers.Vectors
     (Positive, Renderable_Text);
   type Renderable_Text_List is new Renderable_Texts_Package.Vector with null record;

   Atlas_Cols                      : constant Integer := 16;
   Atlas_Rows                      : constant Integer := 16;
   --      MAX_POPUP_TEXTS                 : constant Integer := 128;
   --      MAX_PARTICLE_TEXTS              : constant Integer := 8;
   --      COMIC_TEXT_FULL_COLOUR_TIME     : constant Float := 3.0;
   --      COMIC_TEXT_TIME                 : constant Float :=  6.0;
   --      PARTICLE_TEXT_FULL_COLOUR_TIME  : constant Float := 0.5;
   --      PARTICLE_TEXT_TIME              : constant Float := 0.25;
   --      PARTICLE_TEXT_SPEED             : constant Float := 5.0;

   Font_Shader                     : GL.Objects.Programs.Program;
   Font_Texture                    : GL.Objects.Textures.Texture;
   Text_Box_Shader                 : GL.Objects.Programs.Program;
   Text_Box_VAO                    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Renderable_Texts                : Renderable_Text_List;
   Font_Viewport_Width             : GL.Types.Int := 0;
   Font_Viewport_Height            : GL.Types.Int := 0;
   Num_Render_Strings              : Integer := 0;

   Glyphs : Font_Metadata_Manager.Glyph_Array;

   procedure Load_Font (Atlas_Image, Atlas_Metadata : String);
   procedure Text_To_VBO (theText        : String; Scale_Px : GL.Types.Single;
                          Points_VBO     : in out GL.Objects.Buffers.Buffer;
                          Tex_Coords_VBO : in out GL.Objects.Buffers.Buffer;
                          Point_Count    : in out Integer;
                          Br_X, Br_Y     : in out GL.Types.Single);

   --  ------------------------------------------------------------------------
   --  Add_Text adds a string of text to render on-screen
   --  returns an integer to identify it with later if we want to change the text
   --  returns <0 on error
   --  x,y are position of the bottom-left of the first character in clip space
   --  size_is_px is the size of maximum-sized glyph in pixels on screen
   --  r, g, b, a is the colour of the text string
   function Add_Text (theText                          : String;
                      X, Y, Size_In_Pixels, R, G, B, A : Single) return Integer is
      use GL.Objects.Buffers;
      use GL.Types;
      use Shader_Attributes;
      R_Text            : Renderable_Text;
      Point_Count       : Integer := 0;
      BR_X              : Single := 0.0;
      BR_Y              : Single := 0.0;
   begin
      R_Text.Visible := True;
      R_Text.Top_Left_X := X;
      R_Text.Top_Left_Y := Y;
      R_Text.Size_Px := Size_In_Pixels;
      R_Text.Red := R;
      R_Text.Green := G;
      R_Text.Blue := B;
      R_Text.A := A;
      R_Text.Points_VBO.Initialize_Id;
      R_Text.Tex_Coords_VBO.Initialize_Id;

      Text_To_VBO (theText, Size_In_Pixels, R_Text.Points_VBO,
                   R_Text.Tex_Coords_VBO, Point_Count, BR_X, BR_Y);
      R_Text.VAO.Initialize_Id;
      R_Text.VAO.Bind;

      Array_Buffer.Bind (R_Text.Points_VBO);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Attrib_VP, 2, Single_Type,
                                               False, 0, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (Attrib_VP);

      Array_Buffer.Bind (R_Text.Tex_Coords_VBO);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Attrib_VT, 2, Single_Type,
                                               False, 0, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (Attrib_VT);

      R_Text.Red := R;
      R_Text.Green := G;
      R_Text.Blue := B;
      R_Text.A := A;

      Renderable_Texts.Append (R_Text);
      Num_Render_Strings := Integer (Renderable_Texts.Length);
      return Renderable_Texts.Last_Index;

   end Add_Text;

   --  ------------------------------------------------------------------------

   procedure Centre_Text (ID : Positive; X, Y : Single) is
      Width  : Single;
      Length : Single;
   begin
      if ID <= Renderable_Texts.Last_Index then
         Width := Renderable_Texts.Element (ID).Bottom_Right_X;
         Length := X - 0.5 * Width;
         Move_Text (ID, Length, Y);
      else
         raise Text_Exception with "Text.Centre_Text encountered an invalid ID:" &
           Integer'Image (ID);
      end if;
   end Centre_Text;

   --  ------------------------------------------------------------------------

   procedure Change_Text_Colour (ID : Positive; R, G, B, A : Single) is
      use Renderable_Texts_Package;
      Curs     : Cursor := Renderable_Texts.First;
      Valid_ID : Boolean := False;
      theText  : Renderable_Text;
   begin
      while Has_Element (Curs) and not Valid_ID loop
         Valid_ID := To_Index (Curs) = ID;
         if Valid_ID then
            theText := Element (Curs);
            theText.Red := R;
            theText.Green := G;
            theText.Blue := B;
            theText.A := A;
            Renderable_Texts.Replace_Element (ID, theText);
         else
            Next (Curs);
         end if;
      end loop;

      if not Valid_ID then
         raise Text_Exception with "Text.Change_Text_Colour encountered an invalid ID:" &
           Integer'Image (ID);
      end if;

   end Change_Text_Colour;

   --  ------------------------------------------------------------------------

   procedure Create_Font_Shaders is
   begin
      Text_Shader_Manager.Init (Font_Shader);
      Text_Box_Shader_Manager.Init (Text_Box_Shader);
   end Create_Font_Shaders;

   --  ------------------------------------------------------------------------

   function Create_Text_Box (Text                    : String; Font_ID  : Integer;
                             X_Min, Y_Min, Scale     : Single;
                             Text_Colour, Box_Colour : GL.Types.Colors.Color)
                             return Integer is
      use GL.Types.Colors;
      Text_Index : Integer :=
                     Add_Text (Text, X_Min, Y_Min, Scale, Text_Colour (R),
                               Text_Colour (G), Text_Colour (B), Text_Colour (A));
   begin
      return Text_Index;
   end Create_Text_Box;

   --  ------------------------------------------------------------------------

   procedure Init_Comic_Texts is
   begin
      null;
   end Init_Comic_Texts;

   --  ------------------------------------------------------------------------

   procedure Init_Particle_Texts is
   begin
      null;
   end Init_Particle_Texts;

   --  ------------------------------------------------------------------------

   procedure Init_Text_Rendering
     (Font_image_File, Font_Metadata_File : String;
      Viewport_Width, Viewport_Height     : GL.Types.Int) is
      TB_Points_VBO : GL.Objects.Buffers.Buffer;
   begin
      Game_Utils.Game_Log ("Initialising text rendering");
      Font_Viewport_Width := Viewport_Width;
      Font_Viewport_Height := Viewport_Height;
      Create_Font_Shaders;
      Load_Font (Font_image_File, Font_Metadata_File);
      Text_Box_VAO.Initialize_Id;
      Text_Box_VAO.Bind;
      TB_Points_VBO.Initialize_Id;
      GL.Objects.Buffers.Array_Buffer.Bind (TB_Points_VBO);
      GL.Attributes.Enable_Vertex_Attrib_Array (1);
      GL.Attributes.Set_Vertex_Attrib_Pointer
        (1, 2, GL.Types.Single_Type, False, 0, 0);
      Game_Utils.Game_Log ("Text initialised");
   end Init_Text_Rendering;

   --  ------------------------------------------------------------------------

   function Is_Text_ID_Valid (ID          : Positive; File_Name : String;
                              Line_Number : Integer) return Boolean is
      Result : constant Boolean := ID <= Num_Render_Strings;
   begin
      if not Result then
         Put_Line ("Text.Is_Text_ID_Valid detected in valid Text ID for " &
                     File_Name & ", line" & Integer'Image (Line_Number));
      end if;
      return Result;
   end Is_Text_ID_Valid;

   --  ------------------------------------------------------------------------

   procedure Load_Font (Atlas_Image, Atlas_Metadata : String) is
      use GL.Images;
      use GL.Pixels;
   begin
      Load_File_To_Texture (Atlas_Image, Font_Texture, RGB8, False);
      Font_Metadata_Manager.Load_Metadata (Atlas_Metadata, Glyphs);
   end Load_Font;

   --  ------------------------------------------------------------------------

   procedure Move_Text (ID : Positive; X, Y : Single) is
      Text : Renderable_Text;
   begin
      if ID <= Renderable_Texts.Last_Index then
         Text := Renderable_Texts.Element (ID);
         Text.Top_Left_X := X;
         Text.Top_Left_Y := Y;
         Renderable_Texts.Replace_Element (ID, Text);
      else
         raise Text_Exception with
           "Text.Move.Text detected invalid Renderable Text ID: " &
           Positive'Image (ID);
      end if;
   end Move_Text;

   --  ------------------------------------------------------------------------

   procedure Set_Text_Visible (ID : Positive; Visible : Boolean) is
      use Renderable_Texts_Package;
      Curs     : Cursor := Renderable_Texts.First;
      Valid_ID : Boolean := False;
      theText  : Renderable_Text;
   begin
      while Has_Element (Curs) and not Valid_ID loop
         Valid_ID := To_Index (Curs) = ID;
         if Valid_ID then
            theText := Element (Curs);
            theText.Visible := Visible;
            Renderable_Texts.Replace_Element (ID, theText);
         else
            Next (Curs);
         end if;
      end loop;

      if not Valid_ID then
         raise Text_Exception with "Text.Set_Text_Visible encountered an invalid ID:" &
           Integer'Image (ID);
      end if;

   end Set_Text_Visible;

   --  ------------------------------------------------------------------------
   --  Text_To_VBO creates a VBO from a string of text using our font's
   --  glyph sizes to make a set of quads
   procedure Text_To_VBO (theText        : String; Scale_Px : Single
                          ;
                          Points_VBO     : in out GL.Objects.Buffers.Buffer;
                          Tex_Coords_VBO : in out GL.Objects.Buffers.Buffer;
                          Point_Count    : in out Integer;
                          Br_X, Br_Y     : in out GL.Types.Single) is
      use GL.Objects.Buffers;
      use GL.Types;
      Scale_Px_S         : constant Single := Single (Scale_Px);
      Text_Length        : constant Integer := theText'Length;
      Points_Tmp         : Singles.Vector2_Array (1 .. Int (6 * Text_Length));
      Tex_Coords_Tmp     : Singles.Vector2_Array (1 .. Int (6 * Text_Length));
      Atlas_Rows_S       : constant Single := Single (Atlas_Rows);
      Atlas_Cols_S       : constant Single := Single (Atlas_Cols);
      Font_VP_Height_S   : constant Single := Single (Font_Viewport_Height);
      Font_VP_Width_S    : constant Single := Single (Font_Viewport_Width);
      Line_Offset        : Single := 0.0;
      Current_X          : Single := 0.0;
      Current_Index      : Int := 0;
      Current_Index_12   : constant Int := 1;
      Ascii_Code         : Integer;
      Atlas_Col          : Integer;
      Atlas_Row          : Integer;
      S                  : Single;
      T                  : Single;
      X_Pos              : Single;
      Y_Pos              : Single;
      Skip_Next          : Boolean := False;
   begin
      Game_Utils.Game_Log ("Text.Text_To_VBO Text_Length, theText: " &
                            Integer'Image (Text_Length) & ", " & theText);
      Br_X := 0.0;
      Br_Y := 0.0;
      for index in 1 .. Text_Length loop
         if Skip_Next then
            Skip_Next := False;
         else
            if index < Text_Length and then
              theText (index .. index + 1) = "\n" then
               Line_Offset := Line_Offset + 2.0 * Scale_Px_S / Font_VP_Height_S;
               Current_X := 0.0;
               Skip_Next := True;
            else
               Ascii_Code := Character'Pos (theText (index));
               Atlas_Col := (Ascii_Code - Character'Pos (' ')) mod Atlas_Cols;
               Atlas_Row := (Ascii_Code - Character'Pos (' ')) / Atlas_Cols;
               --  work out texture coordinates in atlas
               S := Single (Atlas_Col) * (1.0 / Atlas_Cols_S);
               T := Single (Atlas_Row + 1) * (1.0 / Atlas_Rows_S);

               --  Work out position of glyphtriangle_width
               X_Pos := Current_X;
               Y_Pos := -2.0 * Scale_Px_S / Font_VP_Height_S *
                 Font_Metadata_Manager.Y_Offset (Glyphs, Ascii_Code) - Line_Offset;
               --  Move next glyph along to the end of this one
               if index + 1 < Text_Length then
                  --  Upper-case letters move twice as far
                  Current_X := Current_X + 2.0 *
                    Font_Metadata_Manager.Width (Glyphs, Ascii_Code) *
                    Scale_Px_S / Font_VP_Width_S;
               end if;

               -- add 6 points and texture coordinates to buffers for each glyph
               Points_Tmp (Current_Index_12) := (X_Pos, Y_Pos);
               Points_Tmp (Current_Index_12 + 1) :=
                 (X_Pos, Y_Pos - (2.0 * Scale_Px_S) / Font_VP_Height_S);
               Points_Tmp (Current_Index_12 + 2) :=
                 (X_Pos + (2.0 * Scale_Px_S) / Font_VP_Width_S,
                  Y_Pos - (2.0 * Scale_Px_S) / Font_VP_Height_S);

               Points_Tmp (Current_Index_12 + 3) :=
                 (X_Pos + (2.0 * Scale_Px_S) / Font_VP_Width_S,
                  Y_Pos - (2.0 * Scale_Px_S) / Font_VP_Height_S);
               Points_Tmp (Current_Index_12 + 4) :=
                 (X_Pos + (2.0 * Scale_Px_S) / Font_VP_Width_S, Y_Pos);
               Points_Tmp (Current_Index_12 + 5) := (X_Pos, Y_Pos);

               Tex_Coords_Tmp (Current_Index_12) :=
                 (S, 1.0 - T + 1.0 / Atlas_Rows_S);
               Tex_Coords_Tmp (Current_Index_12 + 1) := (S,  1.0 - T);
               Tex_Coords_Tmp (Current_Index_12 + 2) :=
                 (S + 1.0 / Atlas_Cols_S, 1.0 - T);

               Tex_Coords_Tmp (Current_Index_12 + 3) :=
                 (S + 1.0 / Atlas_Cols_S, 1.0 - T);
               Tex_Coords_Tmp (Current_Index_12 + 4) :=
                 (S + 1.0 / Atlas_Cols_S,
                  1.0 - T + 1.0 / Atlas_Rows_S);
               Tex_Coords_Tmp (Current_Index_12 + 5) :=
                 (S, 1.0 - T + 1.0 / Atlas_Rows_S);

               --  Update record of bottom-right corner of text area
               if X_Pos + 2.0 * Scale_Px_S / Font_VP_Width_S > Br_X then
                  Br_X := X_Pos + 2.0 * Scale_Px_S / Font_VP_Width_S;
               end if;
               if Y_Pos - 2.0 * Scale_Px_S / Font_VP_Height_S < Br_Y then
                  Br_Y := Y_Pos - 2.0 * Scale_Px_S / Font_VP_Height_S;
               end if;
               Current_Index := Current_Index + 1;
            end if;
         end if;
      end loop;

      Array_Buffer.Bind (Points_VBO);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Points_Tmp, Dynamic_Draw);

      Array_Buffer.Bind (Tex_Coords_VBO);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Tex_Coords_Tmp, Dynamic_Draw);
      Point_Count := 6 * Integer (Current_Index);
   end Text_To_VBO;

   --  ------------------------------------------------------------------------

   procedure Update_Text (ID : Positive; aString : String) is
      Item : Renderable_Text;
      use GL.Types;
   begin
      if ID <= Positive (Renderable_Texts.Length) then
         Item := Renderable_Texts.Element (ID);
         Text_To_VBO (aString, Item.Size_Px, Item.Points_VBO, Item.Tex_Coords_VBO,
                      Item.Point_Count, Single (Item.Bottom_Right_X),
                      Single (Item.Bottom_Right_Y));
      else
         raise Text_Exception with "Text.Update_Text encountered an invalid ID:" &
           Integer'Image (ID);
      end if;
   end Update_Text;

   --  ------------------------------------------------------------------------

end Text;
