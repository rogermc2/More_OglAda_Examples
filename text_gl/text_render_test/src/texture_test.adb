
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Blending;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Text;
with GL.Toggles;

with FT;
with FT.Faces;

package body Texture_Test is

   Renderer_Ref         : GL.Text.Renderer_Reference;
   Vertex_Array         : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer        : GL.Objects.Buffers.Buffer;
   String_Texture       : GL.Objects.Textures.Texture;
   Triangles_Per_Quad   : constant GL.Types.Int := 2;
   Font_File_1          : constant String := "../fonts/NotoSerif-Regular.ttf";

   function Initialize_Font_Data (Font_File : String)
                                  return GL.Text.Renderer_Reference;
   procedure Load_Vertex_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (GL.Types.Singles.Vector4_Pointers);
   procedure Setup_Buffer (Text : String; X, Y, Scale : GL.Types.Single);
   procedure Setup_Font (theLibrary : FT.Library_Reference;
                         Face_Ptr   : out FT.Faces.Face_Reference;
                         Font_File  : String);

   --  ------------------------------------------------------------------------

   procedure Initialize is
   begin
      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;
      Vertex_Buffer.Initialize_Id;
      GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
      String_Texture.Initialize_Id;
      GL.Objects.Textures.Targets.Texture_2D.Bind (String_Texture);

      Renderer_Ref := Initialize_Font_Data (Font_File_1);
   end Initialize;

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
      if GL.Text.Created (Shader_Ref) then
        Setup_Font (theLibrary, Face_Ptr, Font_File);
        GL.Text.Create (Renderer_Ref, Shader_Ref, Face_Ptr);
      else
        raise Texture_Test_Exception with
        "Texture_Test.Initialize_Font_Data, Shader_Ref creation failed.";
      end if;
      return Renderer_Ref;
   end Initialize_Font_Data;

   --  ------------------------------------------------------------------------

   procedure Render (String_Texture : GL.Objects.Textures.Texture;
                     Texture_ID : GL.Uniforms.Uniform) is
      use GL.Objects.Textures.Targets;
      use GL.Types;

      Blend_State               : constant GL.Toggles.Toggle_State :=
                                    GL.Toggles.State (GL.Toggles.Blend);
      Src_Alpha_Blend           : constant  GL.Blending.Blend_Factor :=
                                    GL.Blending.Blend_Func_Src_Alpha;
      One_Minus_Src_Alpha_Blend : constant  GL.Blending.Blend_Factor :=
                                    GL.Blending.One_Minus_Src_Alpha;
      Num_Components            : constant GL.Types.Int := 4;  -- Coords vector size;
      Num_Vertices              : constant GL.Types.Int := 3 * Triangles_Per_Quad;
   begin
      GL.Toggles.Set (GL.Toggles.Blend, Blend_State);
      GL.Blending.Set_Blend_Func (Src_Alpha_Blend, One_Minus_Src_Alpha_Blend);
      GL.Objects.Textures.Set_Active_Unit (0);
      Texture_2D.Bind (String_Texture);
      GL.Uniforms.Set_Int (Texture_ID, 0);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Index      => 0,
                                               Count      => Num_Components,
                                               Kind       => GL.Types.Single_Type,
                                               Normalized => True,
                                               Stride     => 0, Offset => 0);
      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangle_Strip, 0, Num_Vertices);
      GL.Attributes.Disable_Vertex_Attrib_Array (0);

   end Render;

   --  ------------------------------------------------------------------------

   procedure Render_Text (Render_Program                   : GL.Objects.Programs.Program;
                          Text                             : String; X, Y, Scale     : GL.Types.Single;
                          Colour                           : GL.Types.Colors.Basic_Color;
                          Texture_ID, Projection_Matrix_ID : GL.Uniforms.Uniform;
                          Colour_ID                        : GL.Uniforms.Uniform;
                          Projection_Matrix                : GL.Types.Singles.Matrix4) is
      use GL.Text;
      use GL.Types.Colors;
   begin
      if not Renderer_Ref.Created then
            Put_Line ("Texture_Management.Render_Text Renderer_Ref is not created.");
      elsif String_Texture.Initialized then
            String_Texture := To_Texture (Renderer_Ref, UTF_8_String (Text),
                                          (Colour (R), Colour (G), Colour (B), 1.0));
      else
            Put_Line ("Texture_Management.Render_Text String_Texture is not initialized.");
      end if;

      GL.Objects.Programs.Use_Program (Render_Program);
      GL.Uniforms.Set_Single (Colour_ID, Colour (R), Colour (G), Colour (B));
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

      Setup_Buffer (Text, X, Y, Scale);
      Render (String_Texture, Texture_ID);

   exception
      when others =>
         Put_Line ("An exception occurred in Texture_Management.Render_Text.");
         raise;
   end Render_Text;

   --  ------------------------------------------------------------------------

   procedure Setup_Buffer (Text : String; X, Y, Scale : GL.Types.Single) is
      use GL.Objects.Buffers;
      use GL.Text;
      use GL.Types;
      Num_Vertices : constant GL.Types.Int := 3 * Triangles_Per_Quad;
      X_End        : Single;
      Width        : Pixel_Difference;
      Base         : Pixel_Difference;
      S_Base       : Single;
      Top          : Pixel_Difference;
      S_Top        : Single;
      Vertex_Data  : Singles.Vector4_Array (1 .. Num_Vertices);

   begin
      Calculate_Dimensions (Renderer_Ref, Text, Width, Base, Top);
      S_Base := Y + Single (Base);
      S_Top := Y + Single (Top) * Scale;
      X_End := X + Single (Width) * Scale;

      Vertex_Data :=
           ((X,       S_Base, 0.0, 0.0),  --  Lower left X, Y, U, V
            (X_End,   S_Base, 1.0, 0.0),  --  Lower right
            (X,       S_Top,  0.0, 1.0),  --  Upper left

            (X,       S_Top,  0.0, 1.0),  --  Upper left
            (X_End,   S_Top,  1.0, 1.0),  --  Upper Right
            (X_End,   S_Base, 1.0, 0.0)); --  Lower right

      Vertex_Array.Bind;
      Array_Buffer.Bind (Vertex_Buffer);
      Load_Vertex_Buffer (Array_Buffer, Vertex_Data, Dynamic_Draw);
   end Setup_Buffer;

   --  ------------------------------------------------------------------------

   procedure Setup_Font (theLibrary : FT.Library_Reference;
                         Face_Ptr   : out FT.Faces.Face_Reference;
                         Font_File  : String) is
   begin
      FT.Faces.New_Face (theLibrary, Font_File, 0, Face_Ptr);
      FT.Faces.Set_Pixel_Sizes (Face_Ptr, 0, 48);
      GL.Pixels.Set_Unpack_Alignment (GL.Pixels.Bytes);
   end Setup_Font;

   --  ------------------------------------------------------------------------

end Texture_Test;
