

package body Sprite_Manager is

   --  ------------------------------------------------------------------------

   function Add_Texture (aSprite : in out Sprite; File_Name : String;
                         Use_Transparency : Boolean := True) return Boolean is
   begin
      return False;
   end Add_Texture;

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
         Result := aSprite.Textures (1);
      else
         Result := aSprite.Textures (aSprite.Current_Frame);
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
   begin
      null;
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
