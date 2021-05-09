
with GL.Objects.Programs;
with GL.Objects.Textures;

with Textures_Manager;

package Sprite_Manager is

   type Object_Size is private;
   type Point is private;
   type Sprite is private;

   procedure Add_Texture (aSprite      : in out Sprite; File_Name : String;
                          Transparency : Boolean := True);
   procedure Clear_Buffers;
   procedure Flip_Horizontal (aSprite : in out Sprite; Flip : Boolean);
   procedure Flip_Vertical (aSprite : in out Sprite; Flip : Boolean);
   function Get_Current_Frame (aSprite : Sprite)
                               return GL.Objects.Textures.Texture;
   function Get_Position (aSprite : Sprite) return Point;
   function Get_Size (aSprite : Sprite) return Object_Size;
   procedure Init;
   function Is_Active (aSprite : Sprite) return Boolean;
   function Is_Collidable (aSprite : Sprite) return Boolean;
   function Is_Visible (aSprite : Sprite) return Boolean;
   procedure Render (aSprite      : Sprite;
                     Game_Program : GL.Objects.Programs.Program);
   procedure Set_Active (aSprite : in out Sprite; Active : Boolean);
   procedure Set_Frame_Size (aSprite : in out Sprite; Width, Height : Float);
   procedure Set_Number_Of_Frames (aSprite : in out Sprite; Num : Integer);
   procedure Set_Position (aSprite : in out Sprite; X, Y : Float);
   procedure Set_Use_Transparency (aSprite          : in out Sprite;
                                   Use_Transparency : Boolean);
   procedure Set_Velocity (aSprite : in out Sprite; Velocity : Float);
   procedure Set_Visible (aSprite : in out Sprite; Visible : Boolean);
   procedure Update (aSprite : Sprite; Delta_Time : Float);

private
   type Object_Size is record
      Width  : Float := 0.0;
      Height : Float := 0.0;
   end record;

   type Point is record
      X  : Float := 0.0;
      Y  : Float := 0.0;
   end record;

   type Rectangle is record
      Top               : Float := 0.0;
      Bottom            : Float := 0.0;
      Left              : Float := 0.0;
      Right             : Float := 0.0;
   end record;

   type Sprite is record
      Textures          : Textures_Manager.Texture_List;
      Texture_Index     : Integer := 0;
      Current_Frame     : Integer := 0;
      Num_Frames        : Integer := 0;
      Animation_Delay   : Float := 0.25;
      Animation_Elapsed : Float := 0.0;
      Position          : Point := (0.0, 0.0);
      Sprite_Size       : Object_Size;
      Velocity          : Float := 0.0;
      Is_Collidable     : Boolean := True;
      Flip_Horizontal   : Boolean := False;
      Flip_Vertical     : Boolean := False;
      Is_Visible        : Boolean := False;
      Is_Active         : Boolean := False;
      Is_Sprite_Sheet   : Boolean := False;
      Use_Transparency  : Boolean := False;
   end record;

end Sprite_Manager;
