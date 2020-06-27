
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces.C;

with GL.Attributes;
with GL.Blending;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Toggles;

with FT;
with FT.Faces;
with FT.Glyphs;

with Maths;
with Utilities;

package body Texture_Management is
    use Interfaces.C;

    --  Character_Record is based on the learnopengl.com/In-Practice/Text-Rendering tutorial
    type Character_Record is record     --  glyph bitmap data
        Texture   : GL.Objects.Textures.Texture;
        Width     : GL.Types.Int := 0;  -- character width in pixels
        Rows      : GL.Types.Int := 0;  -- number of rows of pixels in bitmap
        Left      : GL.Types.Int := 0;  -- position of left side of character in pixels
                                        --  relative to left side of bitmap
        Top       : GL.Types.Int := 0;  -- position of top of character in pixels
                                        --  relative to bitmap reference line
        Advance_X : GL.Types.Int := 0;   -- Total width of bitmap in 1/64th pixel
    end record;

    OGL_Exception      : Exception;

    procedure Load_Vertex_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (GL.Types.Singles.Vector4_Pointers);

    type Character_Data_Vector is array (Natural range <>) of Character_Record;

    Vertex_Array         : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Vertex_Buffer        : GL.Objects.Buffers.Buffer;
    Extended_Ascii_Data  : Character_Data_Vector (0 .. 255);

    procedure Setup_Character_Textures (Face_Ptr : FT.Faces.Face_Reference);
    procedure Setup_Font (theLibrary : FT.Library_Reference;
                          Face_Ptr   : out FT.Faces.Face_Reference;
                          Font_File  : String);

    --  ------------------------------------------------------------------------

--      function Advance_X (Data : Character_Record) return GL.Types.Int is
--      begin
--          return Data.Advance_X;
--      end Advance_X;

    --  ------------------------------------------------------------------------

    function Initialize_Font_Data (Font_File : String)
                                  return GL.Text.Renderer_Reference is
        theLibrary   : FT.Library_Reference;
        Face_Ptr     : FT.Faces.Face_Reference;
        Shader_Ref   : GL.Text.Shader_Program_Reference;
        Renderer_Ref : GL.Text.Renderer_Reference;
    begin
        theLibrary.Init;
        GL.Text.Create (Shader_Ref);
        --  Shader_Ref holds a reference to the compiled shader program used to render
        --  text as bitmaps. The shader program can only be initialized once the target
        --  OpenGL context has been created.
        Setup_Font (theLibrary, Face_Ptr, Font_File);
        GL.Text.Create (Renderer_Ref, Shader_Ref, Face_Ptr);
        --  A renderer needs to be created for each font family, size and variant.
        --  A renderer renders text to a monochrome texture which can then be used
        --  as an alpha-channel to render the text to the screen with
        --  arbitrary color / texture.

        Setup_Character_Textures (Face_Ptr);
        return Renderer_Ref;
    end Initialize_Font_Data;

    --  ------------------------------------------------------------------------

    procedure Load_Texture (Face_Ptr  : FT.Faces.Face_Reference;
                            Char_Data : in out Character_Record;
                            Width, Height : GL.Types.Size;
                            X_Offset, Y_Offset : GL.Types.Int) is
        use GL.Objects.Textures.Targets;
        use GL.Pixels;
        use GL.Types;
        aTexture          : GL.Objects.Textures.Texture;
        Bitmap_Image_Ptr  : GL.Objects.Textures.Image_Source;
        Bitmap            : constant FT.Bitmap_Record :=
                              FT.Glyphs.Bitmap (Face_Ptr.Glyph_Slot);
        Num_Levels        : constant GL.Types.Size := 1;
        Mip_Level_0       : constant GL.Objects.Textures.Mipmap_Level := 0;
    begin
        aTexture.Initialize_Id;
        Texture_2D.Bind (aTexture);
        Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
        Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
        Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
        Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T

        if Width > 0 and then Height > 0 then
            Texture_2D.Storage (Num_Levels, RGBA8, Width, Height);
        else
            Texture_2D.Storage (Num_Levels, RGBA8, 1, 1);
        end if;

        Bitmap_Image_Ptr := GL.Objects.Textures.Image_Source (Bitmap.Buffer);
        if Width > 0 and Height > 0 then
            Texture_2D.Load_Sub_Image_From_Data
              (Mip_Level_0, X_Offset, Y_Offset, Width, Height, Red, Unsigned_Byte,
               Bitmap_Image_Ptr);
        end if;
        Char_Data.Texture := aTexture;
    end Load_Texture;

    -- --------------------------------------------------------------------------

    procedure Render (Render_Program : GL.Objects.Programs.Program;
                      Text   : String; X, Y, Scale : GL.Types.Single;
                      Colour : GL.Types.Colors.Basic_Color;
                      String_Texture : GL.Objects.Textures.Texture;
                      Texture_ID, Projection_Matrix_ID : GL.Uniforms.Uniform;
                      Colour_ID : GL.Uniforms.Uniform;
                      Projection_Matrix : GL.Types.Singles.Matrix4) is
        use GL.Objects.Buffers;
        use GL.Objects.Textures.Targets;
        use GL.Types.Colors;
        use GL.Types;

        Triangles_Per_Quad  : constant GL.Types.Int := 2;
        Num_Vertices   : constant GL.Types.Int := 3 * Triangles_Per_Quad; -- Two triangles per quad
        Num_Components : constant GL.Types.Int := 2;                      -- Coords per vector element
        Stride         : constant GL.Types.Int := Maths.Stride4;
--          Stride         : constant GL.Types.Int := 0;
        Blend_State    : constant GL.Toggles.Toggle_State :=
                           GL.Toggles.State (GL.Toggles.Blend);
        Src_Alpha_Blend : constant  GL.Blending.Blend_Factor :=
                            GL.Blending.Blend_Func_Src_Alpha;
        One_Minus_Src_Alpha_Blend : constant  GL.Blending.Blend_Factor :=
                                      GL.Blending.One_Minus_Src_Alpha;
        Char           : Character;
        Char_Data      : Character_Record;
        --        Char_Texture   : GL.Objects.Textures.Texture;
        --        Width          : Pixel_Size;
        --        Y_Min          : Pixel_Difference;
        --        Y_Max          : Pixel_Size;

        Scale0         : constant GL.Types.Single := 0.01 * Scale;
        X_Orig         : constant Single := 0.0 * X;
        Y_Orig         : constant Single := 0.0 * Y;
        X_Pos          : Single;
        Base           : Single;
        Quad_Width     : Single;
        Quad_Height    : Single;
--          Num_Chars      : constant GL.Types.Int := Text'Length;
        --  2D quad rendered as two triangles requires 2 * 3 vertices
        Vertex_Data    : Singles.Vector4_Array (1 .. 1 * Num_Vertices);
--          Vertex_Data    : Singles.Vector4_Array (1 .. Num_Chars * Num_Vertices);
    begin
        --  Blending allows a fragment colour's alpha value to control the resulting
        --  colour which will be transparent for all the glyph's background colours and
        --  non-transparent for the actual character pixels.
        GL.Toggles.Enable (GL.Toggles.Blend);
        GL.Blending.Set_Blend_Func (GL.Blending.Src_Alpha,
                                    GL.Blending.One_Minus_Src_Alpha);
        GL.Objects.Programs.Use_Program (Render_Program);

        GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

--          for index in 1 .. Num_Chars loop
        for index in 1 .. 1 loop
            Char := Text (index);
--              Char := Text (Integer (index));
            Char_Data := Extended_Ascii_Data (Character'Pos (Char));
            X_Pos := X_Orig + Single (Char_Data.Left) * Scale0;
            Base := Y_Orig - Single (Char_Data.Rows - Char_Data.Top) * Scale0;
            Quad_Width := Single (Char_Data.Width) * Scale0;
            Quad_Height := Single (Char_Data.Rows) * Scale0;

--              Vertex_Data (6 * (index - 1) + 1 .. 6 * (index - 1) + 6) :=
            Vertex_Data (1 .. 6) :=
              ((X_Pos, Base + Quad_Height,        0.0, 0.0),   -- X, Y, U, V
               (X_Pos, Base,                      0.0, 1.0),
               (X_Pos + Quad_Width, Base,         1.0, 1.0),

               (X_Pos, Base + Quad_Height,              0.0, 0.0),
               (X_Pos + Quad_Width, Base,               1.0, 1.0),
               (X_Pos + Quad_Width, Base + Quad_Height, 1.0, 0.0));
--              X_Orig := X_Orig + Single (Advance_X (Char_Data)) / 64.0 * Scale;
        end loop;

        Utilities.Print_GL_Array4 ("Vertex_Data", Vertex_Data);
--          Utilities.Print_GL_Array4 ("Vertex_Data", Vertex_Data (1 .. 20));
        Vertex_Array.Bind;
        Array_Buffer.Bind (Vertex_Buffer);
        Load_Vertex_Buffer (Array_Buffer, Vertex_Data, Dynamic_Draw);

        --           Char_Texture :=  Char_Data.Texture;
        --           if not GL.Objects.Textures.Is_Texture  (Char_Texture.Raw_Id) then
        if not GL.Objects.Textures.Is_Texture  (String_Texture.Raw_Id) then
            raise OGL_Exception with "FT.OGL.Render_Text, String_Texture is invalid.";
        end if;

        GL.Objects.Textures.Set_Active_Unit (0);
        Texture_2D.Bind (String_Texture);
        GL.Uniforms.Set_Int (Texture_ID, 0);
        GL.Uniforms.Set_Single (Colour_ID, Colour (R), Colour (G), Colour (B));
        GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        Array_Buffer.Bind (Vertex_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0,
                                                 Count  => Num_Components,
                                                 Kind   => GL.Types.Single_Type,
                                                 Normalized => True,
                                                 Stride => Stride, Offset => 0);
        GL.Objects.Vertex_Arrays.Draw_Arrays (Triangle_Strip, 0, 6);
--          GL.Objects.Vertex_Arrays.Draw_Arrays (Triangle_Strip, 0, Num_Vertices);
        GL.Attributes.Disable_Vertex_Attrib_Array (0);
        --  Bitshift by 6 to get value in pixels (2^6 = 64
        --  (divide amount of 1/64th pixels by 64 to get amount of pixels))

        GL.Toggles.Set (GL.Toggles.Blend, Blend_State);
        GL.Blending.Set_Blend_Func (Src_Alpha_Blend, One_Minus_Src_Alpha_Blend);

    exception
        when others =>
            Put_Line ("An exception occurred in Texture_Management.Render.");
            raise;
    end Render;

    --  ------------------------------------------------------------------------

    procedure Render_Text (Render_Program : GL.Objects.Programs.Program;
                           Renderer_Ref : GL.Text.Renderer_Reference;
                           Text   : String; X, Y, Scale : GL.Types.Single;
                           Colour : GL.Types.Colors.Basic_Color;
                           Texture_ID, Projection_Matrix_ID : GL.Uniforms.Uniform;
                           Colour_ID : GL.Uniforms.Uniform;
                           Projection_Matrix : GL.Types.Singles.Matrix4) is
        use GL.Text;
        use GL.Types.Colors;

        Blend_State    : constant GL.Toggles.Toggle_State :=
                           GL.Toggles.State (GL.Toggles.Blend);
        Src_Alpha_Blend : constant  GL.Blending.Blend_Factor :=
                            GL.Blending.Blend_Func_Src_Alpha;
        One_Minus_Src_Alpha_Blend : constant  GL.Blending.Blend_Factor :=
                                      GL.Blending.One_Minus_Src_Alpha;
        String_Texture            : GL.Objects.Textures.Texture;
    begin
        --  Blending allows a fragment colour's alpha value to control the resulting
        --  colour which will be transparent for all the glyph's background colours and
        --  non-transparent for the actual character pixels.
        GL.Toggles.Enable (GL.Toggles.Blend);
        GL.Blending.Set_Blend_Func (GL.Blending.Src_Alpha,
                                    GL.Blending.One_Minus_Src_Alpha);
        GL.Objects.Programs.Use_Program (Render_Program);

        GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

        String_Texture := To_Texture (Renderer_Ref, UTF_8_String (Text),
                                      (Colour (R), Colour (G), Colour (B), 1.0));
        Render (Render_Program, Text, X, Y, Scale, Colour, String_Texture, Texture_ID,
                Projection_Matrix_ID, Colour_ID, Projection_Matrix);
        GL.Toggles.Set (GL.Toggles.Blend, Blend_State);
        GL.Blending.Set_Blend_Func (Src_Alpha_Blend, One_Minus_Src_Alpha_Blend);

    exception
        when others =>
            Put_Line ("An exception occurred in Texture_Management.Render.");
            raise;
    end Render_Text;

    --  ------------------------------------------------------------------------

    procedure Set_Char_Data (Char_Data : in out Character_Record;
                             Width     : GL.Types.Int; Height : GL.Types.Int;
                             Left      : GL.Types.Int; Top    : GL.Types.Int;
                             Advance_X : GL.Types.Int) is
    begin
        Char_Data.Width := Width;
        Char_Data.Rows := Height;
        Char_Data.Left := Left;
        Char_Data.Top := Top;
        Char_Data.Advance_X := Advance_X;
    end Set_Char_Data;

    --  -------------------------------------------------------------------------

    procedure Setup_Character_Textures (Face_Ptr : FT.Faces.Face_Reference) is
        use GL.Objects.Buffers;
        Glyph_Slot   : constant FT.Glyph_Slot_Reference := Face_Ptr.Glyph_Slot;
        Width        : GL.Types.Size;
        Height       : GL.Types.Size;
        X_Offset     : constant GL.Types.Int := 0;
        Y_Offset     : constant GL.Types.Int := 0;
        Char_Data    : Character_Record;
    begin
        --  Blending allows a fragment colour's alpha value to control the resulting
        --  colour which will be transparent for all the glyph's background colours and
        --  non-transparent for the actual character pixels.
        GL.Toggles.Enable (GL.Toggles.Blend);
        GL.Blending.Set_Blend_Func (GL.Blending.Src_Alpha,
                                    GL.Blending.One_Minus_Src_Alpha);

        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;

        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);

        for index in Extended_Ascii_Data'Range loop
            --  Load_Render asks FreeType to create an 8-bit grayscale bitmap image
            --  that can be accessed via face->glyph->bitmap.
            FT.Faces.Load_Character (Face_Ptr, unsigned_long (index),
                                     FT.Faces.Load_Render);
            --  Ensure that the glyph image is an anti-aliased bitmap
            FT.Glyphs.Render_Glyph (Glyph_Slot, FT.Faces.Render_Mode_Mono);

            Width := GL.Types.Int (FT.Glyphs.Bitmap (Glyph_Slot).Width);
            Height := GL.Types.Int (FT.Glyphs.Bitmap (Glyph_Slot).Rows);

            Set_Char_Data (Char_Data, Width, Height,
                           GL.Types.Int (FT.Glyphs.Bitmap_Left (Glyph_Slot)),
                           GL.Types.Int (FT.Glyphs.Bitmap_Top (Glyph_Slot)),
                           GL.Types.Int (FT.Glyphs.Advance (Glyph_Slot).X));

            Load_Texture (Face_Ptr, Char_Data, Width, Height, X_Offset, Y_Offset);
            Extended_Ascii_Data (index) := Char_Data;
        end loop;
    end Setup_Character_Textures;

    --  ------------------------------------------------------------------------

    procedure Setup_Font (theLibrary : FT.Library_Reference;
                          Face_Ptr   : out FT.Faces.Face_Reference;
                          Font_File  : String) is
    begin
        FT.Faces.New_Face (theLibrary, Font_File, 0, Face_Ptr);
        --  Set pixel size to 48 x 48
        FT.Faces.Set_Pixel_Sizes (Face_Ptr, 0, 48);
        --  Disable byte-alignment restriction
        GL.Pixels.Set_Unpack_Alignment (GL.Pixels.Bytes);
    end Setup_Font;

    --  ------------------------------------------------------------------------

end Texture_Management;
