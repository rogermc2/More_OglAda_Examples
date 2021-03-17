
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Textures.Targets;
with GL.Pixels;

with FT;
with FT.Faces;
with FT.Glyphs;

with Utilities;

package body Texture_Manager is

    Vertex_Data   : Vertex_Array;

    procedure Setup_Buffer (Vertex_Buffer : in out V_Buffer;
                            X, Y, Width, Height : GL.Types.Single);
    procedure Setup_Font (My_Library : FT.Library_Reference;
                          Renderer_Ref : out GL.Text.Renderer_Reference;
                          Face_Ptr   : in out FT.Faces.Face_Reference);
    procedure Setup_Texture (aTexture : in out GL.Objects.Textures.Texture;
                             Face_Ptr : FT.Faces.Face_Reference);

    --  ------------------------------------------------------------------------

    procedure Setup_Buffer (Vertex_Buffer : in out V_Buffer;
                            X, Y, Width, Height : GL.Types.Single) is
        use GL.Objects.Buffers;
        use GL.Types;
        Num_Triangles : constant Int := 2;
        Stride        : constant Int := 4;
        X_Pos         : constant Single := X;
        Y_Pos         : constant Single := Y;
    begin
        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Vertex_Data := (
                        (X_Pos, Y_Pos,                  0.0, 0.0),  --  Lower left
                        (X_Pos + Width, Y_Pos,          1.0, 0.0),  --  Lower right
                        (X_Pos, Y_Pos + Height,         0.0, 1.0),  --  Upper left

                        (X_Pos, Y_Pos + Height,         0.0, 1.0),  --  Upper left
                        (X_Pos + Width, Y_Pos + Height, 1.0, 1.0),  --  Upper Right
                        (X_Pos + Width, Y_Pos,          1.0, 0.0)); --  Lower right

        Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data, Static_Draw);
        GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => Num_Triangles,
                                                 Kind   => GL.Types.Single_Type,
                                                 Normalized => True,
                                                 Stride => Stride, Offset => 0);
    end Setup_Buffer;

    --  ------------------------------------------------------------------------

    procedure Setup_Font (My_Library : FT.Library_Reference;
                          Renderer_Ref : out GL.Text.Renderer_Reference;
                          Face_Ptr   : in out FT.Faces.Face_Reference) is
        Font_File  : constant String := "src/fonts/arial.ttf";
        Shader_Ref : GL.Text.Shader_Program_Reference;
    begin
        FT.Faces.New_Face (My_Library, Font_File, 0, Face_Ptr);
        if FT.Faces.Initialized (Face_Ptr) then
            --  Set pixel size to 26 x 26
            FT.Faces.Set_Pixel_Sizes (Face_Ptr, 0, 26);
            GL.Text.Create (Shader_Ref);
            GL.Text.Create (Renderer_Ref, Shader_Ref, Face_Ptr);
            GL.Pixels.Set_Unpack_Alignment (GL.Pixels.Bytes);  --  Disable byte-alignment restriction
        else
            raise Texture_Manager_Exception with
              "Texture_Manager.Setup_Font Face_Ptr initialization failed.";
        end if;

    end Setup_Font;

    --  ------------------------------------------------------------------------

    procedure Setup_Graphic (Vertex_Buffer : in out V_Buffer;
                             aTexture      : in out GL.Objects.Textures.Texture;
                             X, Y, Scale   : GL.Types.Single;
                             aString       : GL.Text.UTF_8_String) is
        use GL.Text;
        use GL.Types;
        My_Library   : FT.Library_Reference;
        Face_Ptr     : FT.Faces.Face_Reference;
        Width        : Pixel_Size;
        Height       : Single;
        Y_Min        : Pixel_Difference;
        Y_Max        : Pixel_Size;
        Renderer_Ref : Renderer_Reference;
    begin
        My_Library.Init;
        if FT.Initialized (My_Library) then
            Setup_Font (My_Library, Renderer_Ref, Face_Ptr);
            Calculate_Dimensions (Renderer_Ref, aString,
                                  Width, Y_Min, Y_Max);
            Height := Single (Y_Max - Y_Min) * Scale;
            Setup_Buffer (Vertex_Buffer, X, Y, Single (Width) * Scale, Height);
            Setup_Texture (aTexture, Face_Ptr);
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in Texture_Manager.Setup_Graphic.");
            raise;
    end Setup_Graphic;

    --  ------------------------------------------------------------------------

    procedure Setup_Texture (aTexture : in out GL.Objects.Textures.Texture;
                             Face_Ptr : FT.Faces.Face_Reference) is
        use GL.Objects.Textures.Targets;
        use GL.Pixels;
        use GL.Types;

        Bitmap : constant FT.Bitmap_Record :=
                   FT.Glyphs.Bitmap (Face_Ptr.Glyph_Slot);
        Width        : constant Size := Size (Bitmap.Width);
        Height       : constant Size := Size (Bitmap.Rows);
    begin
        aTexture.Initialize_Id;
        Texture_2D.Bind (aTexture);
        Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
        Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
        Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
        Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T
        Texture_2D.Load_From_Data
          (0, Red, Width, Height, Red, Unsigned_Byte,
           GL.Objects.Textures.Image_Source (Bitmap.Buffer));
    exception
        when others =>
            Put_Line  ("An exception occurred in Texture_Manager.Setup_Texture.");
            raise;
    end Setup_Texture;

    --  ------------------------------------------------------------------------

end Texture_Manager;
