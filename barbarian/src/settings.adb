
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Input_Handler;

package body Settings is

   G_Settings : Settings;

   --  ------------------------------------------------------------------------

   function Anisotroic_Texturing_Factor return Integer is
   begin
      return G_Settings.Anisotroic_Texturing_Factor;
   end Anisotroic_Texturing_Factor;

   --  ------------------------------------------------------------------------

   function Audio_Volume return Integer is
   begin
      return Integer (G_Settings.Audio_Volume);
   end Audio_Volume;

   --  ------------------------------------------------------------------------

   function Auto_Blood_Wipe return Boolean is
   begin
      return G_Settings.Auto_Blood_Wipe;
   end Auto_Blood_Wipe;

   --  ------------------------------------------------------------------------

   procedure Default_Settings is
   begin
      G_Settings.Joy_Axis_Thresh := 0.5;
      G_Settings.Gfx_Presets := Gfx_Medium;
      G_Settings.GL_Version := V3_2;
      G_Settings.GL_Version_To_Save := V3_2;
      G_Settings.Allow_Rand_Pitch := False;
      G_Settings.Full_Screen := True;
      G_Settings.GL_Window_Width := 1024;
      G_Settings.GL_Window_Height := 768;
      G_Settings.GL_Window_Width_To_Save := 1024;
      G_Settings.GL_Window_Height_To_Save := 768;
      G_Settings.Multi_Sample_Anti_Aliasing := 4;
      G_Settings.Shadow_Size := 512;
      G_Settings.Super_Sample_Anti_Aliasing := 2.0;
      G_Settings.Texf := 2;
      G_Settings.Anisotroic_Texturing_Factor := 4;
      G_Settings.Audio_Volume := 5;
      G_Settings.Music_Volume := 5;
      G_Settings.Render_Dist := 15;
      G_Settings.Far_Clip := 40.0;
      G_Settings.Tile_Batch_Width := 8;
      G_Settings.Render_OLS := True;
      G_Settings.Shadows_Enabled := True;
      G_Settings.Fb_Effects_Enabled := False;
      G_Settings.Particles_Enabled := True;
      G_Settings.Particle_Mipmaps_Enabled := True;
      G_Settings.Auto_Blood_Wipe := False;
      G_Settings.V_Sync := True;
      G_Settings.Hide_Gui := False;

   end Default_Settings;

   --  ------------------------------------------------------------------------

   function Disable_Joystick return Boolean is
   begin
      return G_Settings.Disable_Joystick;
   end Disable_Joystick;

   --  ------------------------------------------------------------------------

   function Far_Clip return GL.Types.Single is
   begin
      return G_Settings.Far_Clip;
   end Far_Clip;

   --  ------------------------------------------------------------------------

   function Fb_Effects_Enabled return Boolean is
   begin
      return G_Settings.Fb_Effects_Enabled;
   end Fb_Effects_Enabled;

   --  ------------------------------------------------------------------------

   function Framebuffer_Height return GL.Types.Int is
   begin
      return G_Settings.GL_Framebuffer_Height;
   end Framebuffer_Height;

   --  ------------------------------------------------------------------------

   function Framebuffer_Width return GL.Types.Int is
   begin
      return G_Settings.GL_Framebuffer_Width;
   end Framebuffer_Width;

   --  ------------------------------------------------------------------------

   function Full_Screen return Boolean is
   begin
      return G_Settings.Full_Screen;
   end Full_Screen;

   --  ------------------------------------------------------------------------

   function Graphic_Preset return Gfx_Preset_Type is
   begin
      return G_Settings.Gfx_Presets;
   end Graphic_Preset;

   --  ------------------------------------------------------------------------

   procedure Load_Settings (theSettings : Settings) is
   begin
      G_Settings := theSettings;
   end Load_Settings;

   --  ------------------------------------------------------------------------

   function Multi_Sample_Anti_Aliasing return Integer is
   begin
      return G_Settings.Multi_Sample_Anti_Aliasing;
   end Multi_Sample_Anti_Aliasing;

   --  ------------------------------------------------------------------------

   function Music_Volume return Audio_Volume_Range is
   begin
      return G_Settings.Music_Volume;
   end Music_Volume;

   --  ------------------------------------------------------------------------

   function Particle_Mipmaps_Enabled return Boolean is
   begin
      return G_Settings.Particle_Mipmaps_Enabled;
   end Particle_Mipmaps_Enabled;

   --  ------------------------------------------------------------------------

   function Render_Distance return Integer is
   begin
      return G_Settings.Render_Distance;
   end Render_Distance;

   --  ------------------------------------------------------------------------

   function Render_OLS return Boolean is
   begin
      return G_Settings.Render_OLS;
   end Render_OLS;

   --  ------------------------------------------------------------------------

   function Save_Settings return Boolean is
   begin
      return True;
   end Save_Settings;

   --  ------------------------------------------------------------------------

   procedure Set_Audio_Volume (Volume : Integer) is
   begin
      G_Settings.Audio_Volume := Volume;
   end Set_Audio_Volume;

   --  ------------------------------------------------------------------------

   procedure Set_Graphic_Preset (Preset : Gfx_Preset_Type) is
   begin
      G_Settings.Gfx_Presets := Preset;
   end Set_Graphic_Preset;

   --  ------------------------------------------------------------------------

   procedure Set_Window_Height_To_Save (Height : Integer) is
   begin
      G_Settings.GL_Window_Height_To_Save := Height;
   end Set_Window_Height_To_Save;

   --  ------------------------------------------------------------------------

   procedure Set_Window_Width_To_Save (Width : Integer) is
   begin
      G_Settings.GL_Window_Width_To_Save := Width;
   end Set_Window_Width_To_Save;

   --  ------------------------------------------------------------------------
   function Set_Menu_Graphic_Presets return Boolean is
      OK : Boolean := True;
   begin
      case Graphic_Preset is
         when Gfx_Dire =>
            G_Settings.Multi_Sample_Anti_Aliasing := 1;
            G_Settings.Super_Sample_Anti_Aliasing := 0.5;
            G_Settings.Texf := 0;
            G_Settings.Anisotroic_Texturing_Factor := 1;
            G_Settings.Render_Dist := 7;
            G_Settings.Far_Clip := 15.0;
            G_Settings.Render_Ols := False;
            G_Settings.Shadows_Enabled := False;
            G_Settings.Shadow_Size := 256;
            G_Settings.Fb_Effects_Enabled := True;

         when Gfx_Low =>
            G_Settings.Multi_Sample_Anti_Aliasing := 1;
            G_Settings.Super_Sample_Anti_Aliasing := 1.0;
            G_Settings.Texf := 2;
            G_Settings.Anisotroic_Texturing_Factor := 1;
            G_Settings.Render_Dist := 9;
            G_Settings.Far_Clip := 25.0;
            G_Settings.Render_Ols := False;
            G_Settings.Shadows_Enabled := True;
            G_Settings.Shadow_Size := 256;
            G_Settings.Fb_Effects_Enabled := True;

         when Gfx_Medium =>
            G_Settings.Multi_Sample_Anti_Aliasing := 4;
            G_Settings.Super_Sample_Anti_Aliasing := 2.0;
            G_Settings.Texf := 2;
            G_Settings.Anisotroic_Texturing_Factor := 4;
            G_Settings.Render_Dist := 15;
            G_Settings.Far_Clip := 40.0;
            G_Settings.Render_Ols := True;
            G_Settings.Shadows_Enabled := True;
            G_Settings.Shadow_Size := 512;
            G_Settings.Fb_Effects_Enabled := True;

         when Gfx_High =>
            G_Settings.Multi_Sample_Anti_Aliasing := 4;
            G_Settings.Super_Sample_Anti_Aliasing := 2.0;
            G_Settings.Texf := 2;
            G_Settings.Anisotroic_Texturing_Factor := 16;
            G_Settings.Render_Dist := 17;
            G_Settings.Far_Clip := 45.0;
            G_Settings.Render_Ols := True;
            G_Settings.Shadows_Enabled := True;
            G_Settings.Shadow_Size := 1024;
            G_Settings.Fb_Effects_Enabled := True;

         when Gfx_Ultra =>
            G_Settings.Multi_Sample_Anti_Aliasing := 16;
            G_Settings.Super_Sample_Anti_Aliasing := 2.0;
            G_Settings.Texf := 2;
            G_Settings.Anisotroic_Texturing_Factor := 16;
            G_Settings.Render_Dist := 19;
            G_Settings.Far_Clip := 50.0;
            G_Settings.Render_Ols := True;
            G_Settings.Shadows_Enabled := True;
            G_Settings.Shadow_Size := 2048;
            G_Settings.Fb_Effects_Enabled := True;
         when others => OK := False;
      end case;
      return OK;
   end Set_Menu_Graphic_Presets;

   --  ------------------------------------------------------------------------

   function Shadows_Enabled return Boolean is
   begin
      return G_Settings.Shadows_Enabled;
   end Shadows_Enabled;

   --  ------------------------------------------------------------------------

   function Shadows_Size return Integer is
   begin
      return G_Settings.Shadow_Size;
   end Shadows_Size;

   --  ------------------------------------------------------------------------

   function Show_FPS return Boolean is
   begin
      return G_Settings.Show_Fps;
   end Show_FPS;

   --  ------------------------------------------------------------------------

   function Super_Sample_Anti_Aliasing return Single is
   begin
      return Single (G_Settings.Super_Sample_Anti_Aliasing);
   end Super_Sample_Anti_Aliasing;

   --  ------------------------------------------------------------------------

   function Texture_Filter return Integer is
   begin
      return G_Settings.Texture_Filtering;
   end Texture_Filter;

   --  ------------------------------------------------------------------------

   function Texf return Integer is
   begin
      return G_Settings.Texf;
   end Texf;

   --  ------------------------------------------------------------------------

   function Tile_Batch_Width return Integer is
   begin
      return G_Settings.Tile_Batch_Width;
   end Tile_Batch_Width;

   --  ------------------------------------------------------------------------

   function V_Sync return Boolean is
   begin
      return G_Settings.V_Sync;
   end V_Sync;

   --  ------------------------------------------------------------------------

   function Window_Width_To_Save return Integer is
   begin
      return G_Settings.GL_Window_Width_To_Save;
   end Window_Width_To_Save;

   --  ------------------------------------------------------------------------

   function Window_Height_To_Save return Integer is
   begin
      return G_Settings.GL_Window_Height_To_Save;
   end Window_Height_To_Save;

   --  ------------------------------------------------------------------------

end Settings;
