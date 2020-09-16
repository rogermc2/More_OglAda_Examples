
with GL.Objects.Textures;

with Texture_Manager;

package body Projectile_Manager is

--      Max_Javelin_Projectiles : constant Int := 32;
--      type Javelins_Array is array (1 .. Max_Javelin_Projectiles)
--        of Projectile_Status;

--      Arrows      : Javelins_Array;
--      Darts       : Javelins_Array;
--      Fireballs   : Javelins_Array;
--      Javelins    : Javelins_Array;
--      Skull_Balls : Javelins_Array;

    Jav_Diff_Texture            : GL.Objects.Textures.Texture;
    Jav_Spec_Texture            : GL.Objects.Textures.Texture;

    Jav_Arrow_Diff_Texture      : GL.Objects.Textures.Texture;
    Jav_Arrow_Spec_Texture      : GL.Objects.Textures.Texture;
    Jav_Bent_Diff_Texture       : GL.Objects.Textures.Texture;
    Jav_Bent_Spec_Texture       : GL.Objects.Textures.Texture;
    Jav_Dart_Diff_Texture       : GL.Objects.Textures.Texture;
    Jav_Dart_Spec_Texture       : GL.Objects.Textures.Texture;
    Jav_Fireball_Diff_Texture   : GL.Objects.Textures.Texture;
    Jav_Fireball_Spec_Texture   : GL.Objects.Textures.Texture;
    Jav_Skull_Ball_Diff_Texture : GL.Objects.Textures.Texture;
    Jav_Skull_Ball_Spec_Texture : GL.Objects.Textures.Texture;

--      Next_Javelin     : Int := 0;
--      Javelin_Count    : Int := 0;
--      Next_Dart        : Int := 0;
--      Dart_Count       : Int := 0;
--      Next_Arrow       : Int := 0;
--      Arrow_Count      : Int := 0;
--      Next_Fireball    : Int := 0;
--      Fireball_Count   : Int := 0;
--      Next_skull_Ball  : Int := 0;
--      Skull_Ball_Count : Int := 0;

    --  ------------------------------------------------------------------------

    procedure Init is
        use Texture_Manager;
        Create_Mips : constant Boolean := True;
        SRGB        : constant Boolean := True;
    begin
        Load_Image_To_Texture ("textures/javelin_diff.png",
                               Jav_Diff_Texture, Create_Mips, SRGB);
        Load_Image_To_Texture ("textures/javelin_spec.png",
                               Jav_Spec_Texture, Create_Mips, SRGB);
        Load_Image_To_Texture ("textures/arrow_diff.png",
                               Jav_Arrow_Diff_Texture, Create_Mips, SRGB);
        Load_Image_To_Texture ("textures/arrow_spec.png",
                               Jav_Arrow_Spec_Texture, Create_Mips, SRGB);
        Load_Image_To_Texture ("textures/bent_diff.png",
                               Jav_Bent_Diff_Texture, Create_Mips, SRGB);
        Load_Image_To_Texture ("textures/bent_spec.png",
                               Jav_Bent_Spec_Texture, Create_Mips, SRGB);
        Load_Image_To_Texture ("textures/darts_diff.png",
                               Jav_Dart_Diff_Texture, Create_Mips, SRGB);

        Load_Image_To_Texture ("textures/darts_spec.png",
                               Jav_Dart_Spec_Texture, Create_Mips, SRGB);
        Load_Image_To_Texture ("textures/fball_diff.png",
                               Jav_Fireball_Diff_Texture, Create_Mips, SRGB);
        Load_Image_To_Texture ("textures/fball_spec.png",
                               Jav_Fireball_Spec_Texture, Create_Mips, SRGB);
        Load_Image_To_Texture ("textures/sball_diff.png",
                               Jav_Skull_Ball_Diff_Texture, Create_Mips, SRGB);
        Load_Image_To_Texture ("textures/sball_spec.png",
                               Jav_Skull_Ball_Spec_Texture, Create_Mips, SRGB);
    end Init;
    --  ------------------------------------------------------------------------

--      procedure Reset_Projectiles is
--      begin
--          Next_Javelin := 0;
--          Javelin_Count := 0;
--          Next_Dart := 0;
--          Dart_Count := 0;
--          Next_Arrow := 0;
--          Arrow_Count := 0;
--          Next_Fireball := 0;
--          Fireball_Count := 0;
--          Next_skull_Ball := 0;
--          Skull_Ball_Count := 0;

--          for index in 1 .. Max_Javelin_Projectiles loop
--              Javelins (index).Sprite_Index :=
--                Add_Sprite (Jav_Diff_Texture, Jav_Spec_Texture, 1, 1);
--          end loop;

--      end Reset_Projectiles;

    --  ------------------------------------------------------------------------

--      procedure Update_Projectiles (Step_Time : Float) is
--      begin
--          Next_Javelin := 0;
--          Javelin_Count := 0;
--          Next_Dart := 0;
--          Dart_Count := 0;
--          Next_Arrow := 0;
--          Arrow_Count := 0;
--          Next_Fireball := 0;
--          Fireball_Count := 0;
--          Next_skull_Ball := 0;
--          Skull_Ball_Count := 0;
--      end Update_Projectiles;

    --  ------------------------------------------------------------------------

end Projectile_Manager;
