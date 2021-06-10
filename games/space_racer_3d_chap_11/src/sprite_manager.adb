
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

    function Get_Centre (aSprite : Sprite) return Point is
        Centre : Point;
    begin
        Centre.X := aSprite.Position.X + aSprite.Centre.X;
        Centre.Y := aSprite.Position.Y + aSprite.Centre.Y;
        return Centre;
    end Get_Centre;

    --  ------------------------------------------------------------------------

    function Get_Collision_Rectangle  (aSprite : Sprite) return Rectangle is
        theRect : Rectangle;
    begin
        theRect.Left := aSprite.Position.X + aSprite.Collision.Left;
        theRect.Right := aSprite.Position.X + aSprite.Sprite_Size.Width +
          aSprite.Collision.Right;
        theRect.Top := aSprite.Position.Y + aSprite.Collision.Top;
        theRect.Bottom := aSprite.Position.Y + aSprite.Sprite_Size.Height +
          aSprite.Collision.Bottom;
        return theRect;
    end Get_Collision_Rectangle;

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

    function Get_Height (aSprite : Sprite) return Float is
    begin
        return aSprite.Sprite_Size.Height;
    end Get_Height;

    --  ------------------------------------------------------------------------

    function Get_Position (aSprite : Sprite) return Point is
    begin
        return aSprite.Position;
    end Get_Position;

    --  ------------------------------------------------------------------------

    function Get_Radius (aSprite : Sprite) return Float is
    begin
        return aSprite.Radius;
    end Get_Radius;

    --  ------------------------------------------------------------------------

    function Get_Size (aSprite : Sprite) return Object_Size is
    begin
        return aSprite.Sprite_Size;
    end Get_Size;

    --  -------------------------------------------------------------------------

    function Get_Value (aSprite : Sprite) return Integer is
    begin
        return aSprite.Value;
    end Get_Value;

    --  ------------------------------------------------------------------------

    function Get_Width (aSprite : Sprite) return Float is
    begin
        return aSprite.Sprite_Size.Width;
    end Get_Width;

    --  ------------------------------------------------------------------------

    procedure Init is
    begin
        Sprites_VAO.Initialize_Id;
        Sprites_VAO.Bind;
        Quad_Buffer.Initialize_Id;
        Texture_Buffer.Initialize_Id;
    end Init;

    --  ------------------------------------------------------------------------

    function Intersect_Circle (Sprite1, Sprite2 : Sprite) return Boolean is
        Result    : Boolean := False;
        P1        : Point;
        P2        : Point;
        X         : Float;
        Y         : Float;
        Dist_Sq   : Float;
        Rad_1_Sq  : Float;
        Rad_2_Sq  : Float;

    begin
        if Sprite1.Is_Collidable and Sprite2.Is_Collidable and
          Sprite1.Is_Active and Sprite2.Is_Active then
            P1 := Get_Centre (Sprite1);
            P2 := Get_Centre (Sprite2);
            X := P2.X - P1.X;
            Y := P2.Y - P1.Y;
            Dist_Sq := X ** 2 + Y ** 2;
            Rad_1_Sq := Get_Radius (Sprite1) ** 2;
            Rad_2_Sq := Get_Radius (Sprite2) ** 2;
            Result := Rad_1_Sq + Rad_2_Sq >= Dist_Sq;
        end if;
        return Result;
    end Intersect_Circle;

    --  ------------------------------------------------------------------------

    function Intersect_Rectangle (Sprite1, Sprite2 : Sprite) return Boolean is
        Result : Boolean := False;
        Rect1  : Rectangle;
        Rect2  : Rectangle;
    begin
        if Sprite1.Is_Collidable and Sprite2.Is_Collidable and
          Sprite1.Is_Active and Sprite2.Is_Active then
            Rect1 := Get_Collision_Rectangle (Sprite1);
            Rect2 := Get_Collision_Rectangle (Sprite2);
            if Rect1.Left >= Rect2.Left and Rect1.Left <= Rect2.Right and
              Rect1.Top >= Rect2.Top and Rect1.Top <= Rect2.Bottom then
                Result := True;
            elsif Rect1.Right >= Rect2.Left and Rect1.Right <= Rect2.Right and
              Rect1.Top >= Rect2.Top and Rect1.Top <= Rect2.Bottom then
                Result := True;
            elsif Rect1.Left >= Rect2.Left and Rect1.Right <= Rect2.Right and
              Rect1.Top >= Rect2.Top and Rect1.Top <= Rect2.Bottom then
                Result := True;
            elsif  Rect1.Right >= Rect2.Left and Rect1.Right <= Rect2.Right and
              Rect1.Top < Rect2.Top and Rect1.Bottom > Rect2.Bottom then
                Result := True;
            elsif  Rect1.Top >= Rect2.Top and Rect1.Bottom <= Rect2.Bottom and
              Rect1.Left < Rect2.Left and Rect1.Right > Rect2.Right then
                Result := True;
            elsif Rect2.Left >= Rect1.Left and Rect2.Right <= Rect1.Right and
              Rect2.Top >= Rect1.Top and Rect2.Top <= Rect1.Bottom then
                Result := True;
            elsif Rect2.Right >= Rect1.Left and Rect2.Right <= Rect1.Right and
              Rect2.Top >= Rect1.Top and Rect2.Top <= Rect1.Bottom then
                Result := True;
            elsif Rect2.Left >= Rect1.Left and Rect2.Right <= Rect1.Right and
              Rect2.Top < Rect1.Top and Rect2.Top > Rect1.Bottom then
                Result := True;
            elsif  Rect1.Top >= Rect2.Top and Rect1.Bottom <= Rect2.Bottom and
              Rect1.Left < Rect2.Left and Rect1.Right > Rect2.Right then
                Result := True;
            elsif  Rect2.Top >= Rect1.Top and Rect2.Bottom <= Rect1.Bottom and
              Rect2.Left < Rect1.Left and Rect2.Right > Rect1.Right then
                Result := True;
            end if;
        end if;
        return Result;
    end Intersect_Rectangle;

    --  ------------------------------------------------------------------------

    function Is_Active (aSprite : Sprite) return Boolean is
    begin
        return aSprite.Is_Active;
    end Is_Active;

    --  ------------------------------------------------------------------------

    function Is_Clicked (aSprite : Sprite) return Boolean is
    begin
        return aSprite.Is_Clicked;
    end Is_Clicked;

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

    procedure Jump (aSprite : in out Sprite; Status : Sprite_Status) is
    begin
        if Status = Sprite_Down then
            if aSprite.Position.Y > 50.0 then
                aSprite.Position.Y := aSprite.Position.Y - 7.0;
            end if;
        else  --  Sprite_Up
            if aSprite.Position.Y <= 470.0 then
                aSprite.Position.Y := aSprite.Position.Y + 7.0;
            end if;
        end if;
    end Jump;

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
            Clear_Buffers;

            if aSprite.Use_Transparency then
                GL.Toggles.Enable (GL.Toggles.Blend);
                Set_Blend_Func (Src_Alpha, One_Minus_Src_Alpha);
            end if;

            Texture_2D.Bind (Get_Current_Frame (aSprite));
            if aSprite.Texture_Index < aSprite.Num_Frames then
                U := Single (aSprite.Current_Frame) * Tex_Width;
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

    procedure Set_Centre (aSprite : in out Sprite; Centre : Point) is
    begin
        aSprite.Centre := Centre;
    end Set_Centre;

    --  ------------------------------------------------------------------------

    procedure Set_Clicked (aSprite : in out Sprite; Clicked : Boolean) is
    begin
        aSprite.Is_Clicked := Clicked;
    end Set_Clicked;

    --  ------------------------------------------------------------------------

    procedure Set_Collidable (aSprite : in out Sprite; Collidable : Boolean) is
    begin
        aSprite.Is_Collidable := Collidable;
    end Set_Collidable;

    --  ------------------------------------------------------------------------

    procedure Set_Collision (aSprite : in out Sprite; Rect : Rectangle) is
    begin
        aSprite.Collision := Rect;
    end Set_Collision;

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

    procedure Set_Position (aSprite : in out Sprite; Position : Point) is
    begin
        aSprite.Position := Position;
    end Set_Position;

    --  ------------------------------------------------------------------------

    procedure Set_Radius (aSprite : in out Sprite; Radius : Float) is
    begin
        aSprite.Radius := Radius;
    end Set_Radius;

    --  ------------------------------------------------------------------------

    procedure Set_Use_Transparency (aSprite          : in out Sprite;
                                    Use_Transparency : Boolean) is
    begin
        aSprite.Use_Transparency := Use_Transparency;
    end Set_Use_Transparency;

    --  ------------------------------------------------------------------------

    procedure Set_Value (aSprite : in out Sprite; Value : Integer) is
    begin
        aSprite.Value := Value;
    end Set_Value;

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

    procedure Update (aSprite : in out Sprite; Delta_Time : Float) is
    begin
        if aSprite.Is_Active then
            aSprite.Animation_Elapsed := aSprite.Animation_Elapsed + Delta_Time;
            if aSprite.Animation_Elapsed >= aSprite.Animation_Delay then
                aSprite.Current_Frame := aSprite.Current_Frame + 1;
                if aSprite.Current_Frame > aSprite.Num_Frames then
                    aSprite.Current_Frame := 1;
                end if;
                aSprite.Animation_Elapsed := 0.0;
            end if;
            aSprite.Position.X := aSprite.Position.X +
              aSprite.Velocity * Delta_Time;
        end if;
    end Update;

    --  ------------------------------------------------------------------------

end Sprite_Manager;
