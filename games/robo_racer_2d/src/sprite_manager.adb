
--  with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Blending;
with GL.Images;
with GL.Objects.Buffers;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Pixels;
with GL.Toggles;
with GL.Types;
with Utilities;

package body Sprite_Manager is

   Sprites_VAO    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Quad_Buffer    : GL.Objects.Buffers.Buffer;
   Texture_Buffer : GL.Objects.Buffers.Buffer;

   --  ------------------------------------------------------------------------

   procedure Add_Texture (aSprite      : in out Sprite; File_Name : String;
                          Transparency : Boolean := True) is
      use Textures_Manager;
      aTexture : GL.Objects.Textures.Texture;
   begin
      GL.Images.Load_File_To_Texture (Path           => File_Name,
                                      Texture        => aTexture,
                                      Texture_Format => GL.Pixels.RGBA,
                                      Try_TGA        => True);
      Add_Texture_To_List (aSprite.Textures, aTexture);

      aSprite.Is_Sprite_Sheet := Get_Last_Index (aSprite.Textures) = 1 and
        aSprite.Num_Frames > 1;
      aSprite.Use_Transparency := Transparency;

   end Add_Texture;

   --  ------------------------------------------------------------------------

   procedure Clear_Buffers is
   begin
      Quad_Buffer.Clear;
      Texture_Buffer.Clear;
      Quad_Buffer.Initialize_Id;
      Texture_Buffer.Initialize_Id;
   end Clear_Buffers;

   --  ------------------------------------------------------------------------

   procedure Flip_Horizontal (aSprite : in out Sprite; Flip : Boolean) is
   begin
      aSprite.Flip_Horizontal := Flip;
   end Flip_Horizontal;

   --  ------------------------------------------------------------------------

   procedure Flip_Vertical (aSprite : in out Sprite; Flip : Boolean) is
   begin
      aSprite.Flip_Vertical := Flip;
   end Flip_Vertical;

   --  ------------------------------------------------------------------------

   function Get_Current_Frame (aSprite : Sprite)
                               return GL.Objects.Textures.Texture is
      Result : GL.Objects.Textures.Texture;
   begin
      if aSprite.Is_Sprite_Sheet then
         Result := Textures_Manager.Get_Texture (aSprite.Textures, 1);
      else
         Result := Textures_Manager.Get_Texture
           (aSprite.Textures, aSprite.Current_Frame);
      end if;
      return Result;
   end Get_Current_Frame;

   --  ------------------------------------------------------------------------

   function Get_Position (aSprite : Sprite) return Point is
   begin
      return aSprite.Position;
   end Get_Position;

   --  ------------------------------------------------------------------------

   function Get_Size (aSprite : Sprite) return Object_Size is
   begin
      return aSprite.Sprite_Size;
   end Get_Size;

   --  -------------------------------------------------------------------------

   procedure Init is
   begin
      Sprites_VAO.Initialize_Id;
      Sprites_VAO.Bind;
      Quad_Buffer.Initialize_Id;
      Texture_Buffer.Initialize_Id;
   end Init;

   --  ------------------------------------------------------------------------

   function Is_Active (aSprite : Sprite) return Boolean is
   begin
      return aSprite.Is_Active;
   end Is_Active;

   --  ------------------------------------------------------------------------

   function Is_Collidable (aSprite : Sprite) return Boolean is
   begin
      return aSprite.Is_Collidable;
   end Is_Collidable;

   --  ------------------------------------------------------------------------

   function Is_Visible (aSprite : Sprite) return Boolean is
   begin
      return aSprite.Is_Visible;
   end Is_Visible;

   --  ------------------------------------------------------------------------

   procedure Render (aSprite : Sprite) is
      use GL.Attributes;
      use GL.Blending;
      use GL.Objects.Textures.Targets;
      use GL.Objects.Buffers;
      use GL.Types;
      Quad_Vertices  : Singles.Vector2_Array (1 .. 6);
      Texture_Coords : Singles.Vector2_Array (1 .. 6);
      Tex_Width      : constant Single :=
                         1.0 + Single (aSprite.Texture_Index - 1) /
                         Single (aSprite.Num_Frames);
      Tex_Height     : constant Single := 1.0;
      U              : Single := 0.0;
      V              : constant Single := 0.0;
      X              : constant Single := Single (aSprite.Position.X);
      Y              : constant Single := Single (aSprite.Position.Y);
      Width          : constant Single := Single (aSprite.Sprite_Size.Width);
      Height         : constant Single := Single (aSprite.Sprite_Size.Height);
   begin
      if aSprite.Is_Visible then
         Sprites_VAO.Bind;

         if aSprite.Use_Transparency then
            GL.Toggles.Enable (GL.Toggles.Blend);
            Set_Blend_Func (Src_Alpha, One_Minus_Src_Alpha);
         end if;

         Texture_2D.Bind (Get_Current_Frame (aSprite));
         if aSprite.Texture_Index + 1 < aSprite.Num_Frames then
            U := Single (aSprite.Num_Frames) * Tex_Width;
         end if;

         Quad_Vertices (1) := (X, Y + Height);          --  top left
         Texture_Coords (1) := (U, V + Tex_Height);

         Quad_Vertices (2) := (X, Y);                     --  bottom left
         Texture_Coords (2) := (U, V);

         Quad_Vertices (3) := (X + Width, Y);           --  bottom right
         Texture_Coords (3) := (U + Tex_Width, V);

         Quad_Vertices (4) := (X + Width, Y);           --  bottom right
         Texture_Coords (4) := (U + Tex_Width, V);

         Quad_Vertices (5) := (X + Width, Y + Height);  --  top right
         Texture_Coords (5) := (U + Tex_Width, V + Tex_Height);

         Quad_Vertices (6) := (X, Y + Height);          --  top left
         Texture_Coords (6) := (U, V + Tex_Height);

         --           Put_Line ("Sprite_Manager.Render, width, height: " &
         --                       Single'Image (Width) & ", " & Single'Image (Height));
         --              Utilities.Print_GL_Array2 ("Quad_Vertices", Quad_Vertices);
         --              Utilities.Print_GL_Array2 ("Texture_Coords", Texture_Coords);

         Array_Buffer.Bind (Quad_Buffer);
         Utilities.Load_Vertex_Buffer (Array_Buffer, Quad_Vertices,
                                       Static_Draw);
         Enable_Vertex_Attrib_Array (0);
         Set_Vertex_Attrib_Pointer (0, 2, Single_Type, False, 0, 0);

         Array_Buffer.Bind (Texture_Buffer);
         Utilities.Load_Vertex_Buffer (Array_Buffer, Texture_Coords,
                                       Static_Draw);
         Enable_Vertex_Attrib_Array (1);
         Set_Vertex_Attrib_Pointer (1, 2, Single_Type, False, 0, 0);

         GL.Objects.Vertex_Arrays.Draw_Arrays (GL.Types.Triangles, 0, 6);

         if aSprite.Use_Transparency then
            GL.Toggles.Disable (GL.Toggles.Blend);
         end if;
      end if;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Set_Active (aSprite : in out Sprite; Active : Boolean) is
   begin
      aSprite.Is_Active := Active;
   end Set_Active;

   --  ------------------------------------------------------------------------

   procedure Set_Frame_Size (aSprite : in out Sprite; Width, Height : Float) is
   begin
      aSprite.Sprite_Size := (Width, Height);
   end Set_Frame_Size;

   --  ------------------------------------------------------------------------

   procedure Set_Number_Of_Frames (aSprite : in out Sprite; Num : Integer) is
   begin
      aSprite.Num_Frames := Num;
   end Set_Number_Of_Frames;

   --  ------------------------------------------------------------------------

   procedure Set_Position (aSprite : in out Sprite; X, Y : Float) is
   begin
      aSprite.Position := (X, Y);
   end Set_Position;

   --  ------------------------------------------------------------------------

   procedure Set_Use_Transparency (aSprite          : in out Sprite;
                                   Use_Transparency : Boolean) is
   begin
      aSprite.Use_Transparency := Use_Transparency;
   end Set_Use_Transparency;

   --  ------------------------------------------------------------------------

   procedure Set_Velocity (aSprite : in out Sprite; Velocity : Float) is
   begin
      aSprite.Velocity := Velocity;
   end Set_Velocity;

   --  ------------------------------------------------------------------------

   procedure Set_Visible (aSprite : in out Sprite; Visible : Boolean) is
   begin
      aSprite.Is_Visible := Visible;
   end Set_Visible;

   --  ------------------------------------------------------------------------

   procedure Update (aSprite : Sprite; Delta_Time : Float) is
   begin
      null;
   end Update;

   --  ------------------------------------------------------------------------

end Sprite_Manager;
