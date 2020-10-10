
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures;

with Particle_System;
with Sprite_Renderer;
with Texture_Manager;

package body Projectile_Manager is

   Max_Javelin_Projectiles : constant Integer := 32;
   type Javelins_Array is array (1 .. Max_Javelin_Projectiles)
     of Projectile_Status;

   Arrows      : Javelins_Array;
   Darts       : Javelins_Array;
   Fireballs   : Javelins_Array;
   Javelins    : Javelins_Array;
   Skull_Balls : Javelins_Array;

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

   Next_Javelin         : Integer := 0;
   Javelin_Count        : Integer := 0;
   Next_Dart            : Integer := 0;
   Dart_Count           : Integer := 0;
   Next_Arrow           : Integer := 0;
   Arrow_Count          : Integer := 0;
   Next_Fireball        : Integer := 0;
   Fireball_Count       : Integer := 0;
   Next_skull_Ball      : Integer := 0;
   Skull_Ball_Count     : Integer := 0;
   Blue_Spec_Particles  : Int := 0;
   Green_Spec_Particles : Int := 0;

   --  ------------------------------------------------------------------------

   procedure Init is
      use Texture_Manager;
      Create_Mips : constant Boolean := True;
      SRGB        : constant Boolean := True;
      Always_Draw : Boolean := False;
   begin
      Load_Image_To_Texture ("src/textures/javelin_diff.png",
                             Jav_Diff_Texture, Create_Mips, SRGB);
      Load_Image_To_Texture ("src/textures/javelin_spec.png",
                             Jav_Spec_Texture, Create_Mips, SRGB);
      Load_Image_To_Texture ("src/textures/arrow_diff.png",
                             Jav_Arrow_Diff_Texture, Create_Mips, SRGB);
      Load_Image_To_Texture ("src/textures/arrow_spec.png",
                             Jav_Arrow_Spec_Texture, Create_Mips, SRGB);
      Load_Image_To_Texture ("src/textures/javelin_bent_diff.png",
                             Jav_Bent_Diff_Texture, Create_Mips, SRGB);
      Load_Image_To_Texture ("src/textures/javelin_bent_spec.png",
                             Jav_Bent_Spec_Texture, Create_Mips, SRGB);
      Load_Image_To_Texture ("src/textures/darts_diff.png",
                             Jav_Dart_Diff_Texture, Create_Mips, SRGB);

      Load_Image_To_Texture ("src/textures/darts_spec.png",
                             Jav_Dart_Spec_Texture, Create_Mips, SRGB);
      Load_Image_To_Texture ("src/textures/fball_diff.png",
                             Jav_Fireball_Diff_Texture, Create_Mips, SRGB);
      Load_Image_To_Texture ("src/textures/fball_spec.png",
                             Jav_Fireball_Spec_Texture, Create_Mips, SRGB);
      Load_Image_To_Texture ("src/textures/sball_dm.png",
                             Jav_Skull_Ball_Diff_Texture, Create_Mips, SRGB);
      Load_Image_To_Texture ("src/textures/sball_sm.png",
                             Jav_Skull_Ball_Spec_Texture, Create_Mips, SRGB);

      Reset_Projectiles;
      Blue_Spec_Particles := Particle_System.Create_Particle_System
        ("blue_spectral.particles", False, True, Always_Draw);
      Green_Spec_Particles := Particle_System.Create_Particle_System
        ("green_spectral.particles", False, True, Always_Draw);
      Put_Line ("Projectile_Manager.Init projectiles initalized.");
   end Init;

   --  ------------------------------------------------------------------------

   procedure Reset_Projectiles is
   begin
      Next_Javelin := 0;
      Javelin_Count := 0;
      Next_Dart := 0;
      Dart_Count := 0;
      Next_Arrow := 0;
      Arrow_Count := 0;
      Next_Fireball := 0;
      Fireball_Count := 0;
      Next_skull_Ball := 0;
      Skull_Ball_Count := 0;

      for index in 1 .. Max_Javelin_Projectiles loop
         Javelins (index).Sprite_Index :=
           Sprite_Renderer.Add_Sprite (Jav_Diff_Texture, Jav_Spec_Texture, 1, 1);
      end loop;

   end Reset_Projectiles;

   --  ------------------------------------------------------------------------

   procedure Update_Projectiles (Step_Time : Float) is
   begin
      Next_Javelin := 0;
      Javelin_Count := 0;
      Next_Dart := 0;
      Dart_Count := 0;
      Next_Arrow := 0;
      Arrow_Count := 0;
      Next_Fireball := 0;
      Fireball_Count := 0;
      Next_skull_Ball := 0;
      Skull_Ball_Count := 0;
   end Update_Projectiles;

   --  ------------------------------------------------------------------------

end Projectile_Manager;
