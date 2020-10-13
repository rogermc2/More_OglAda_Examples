
with GL.Types; use GL.Types;

package Settings is

   type Gfx_Preset_Type is (Gfx_Dire, Gfx_Low, Gfx_Medium, Gfx_High,
                            Gfx_Ultra, Gfx_Custom);
   type Settings is private;
   type V_GL is (V2_1, V3_2);
   subtype Audio_Volume_Range is Integer range 0 .. 10;

   function Anisotroic_Texturing_Factor return Float;
   function Audio_Volume return Audio_Volume_Range;
   function Auto_Blood_Wipe return Boolean;
   procedure Default_Settings;
   function Disable_Joystick return Boolean;
   function Far_Clip return GL.Types.Single;
   function Fb_Effects_Enabled return Boolean;
   function Framebuffer_Height return GL.Types.Int;
   function Framebuffer_Width return GL.Types.Int;
   function Full_Screen return Boolean;
   function Graphic_Preset return Gfx_Preset_Type;
   procedure Load_Settings;
   function Multi_Sample_Anti_Aliasing return Integer;
   function Music_Volume return Audio_Volume_Range;
   function Particle_Mipmaps_Enabled return Boolean;
   function Render_OLS return Boolean;
   function Render_Distance return Integer;
   function Save_Settings return Boolean;
   function Shadows_Enabled return Boolean;
   function Show_FPS return Boolean;
   function Shadows_Size return Integer;
   function Super_Sample_Anti_Aliasing return Single;
   function Texf return Integer;
   function Texture_Filter return Integer;
   function Tile_Batch_Width return Integer;
   function V_Sync return Boolean;
   function Window_Width_To_Save return Integer;
   function Window_Height_To_Save return Integer;

private
   type Settings is record
      Joy_Axis_Thresh              : Float := 0.5;
      Gfx_Presets                  : Gfx_Preset_Type := Gfx_Medium;
      GL_Version                   : V_GL := V3_2;
      GL_Version_To_Save           : V_GL := V3_2;
      Allow_Rand_Pitch             : Boolean := False;
      Full_Screen                  : Boolean := False;
      --  lock drawing to monitor's vertical synch i.e. 60Hz
      GL_Window_Width              : Integer := 1024;
      GL_Window_Height             : Integer := 768;
      GL_Framebuffer_Width         : Int := 512;
      GL_Framebuffer_Height        : Int := 512;
      GL_Window_Width_To_Save      : Integer := 1024;
      GL_Window_Height_To_Save     : Integer := 768;
      --  Built-in multi-sample anti-aliasing. Not used much if using FB FX
      Multi_Sample_Anti_Aliasing   : Integer := 4;
      Shadow_Size                  : Integer := 512;
      Super_Sample_Anti_Aliasing   : Float := 2.0;
      --  Texture filtering nearest = 0, bilinear, trilinear
      Texture_Filtering            : Integer := 2;
      Anisotroic_Texturing_Factor  : Float := 1.0;  -- 1.0 to 16.0
      --  Maximum rendering distance in tiles from player
      Render_Distance              : Integer := 15;
      Far_Clip                     : Single := 0.0;
      --  Number of tiles*tiles to put in batches
      Tile_Batch_Width             : Integer := 8;
      Texf                         : Integer := 2;
      Audio_Volume                 : Audio_Volume_Range := 5;
      Music_Volume                 : Audio_Volume_Range := 5;
      Render_Dist                  : Integer := 15;
      Disable_Joystick             : Boolean := True;
      Joy_Axis_Threshold           : Float := 0.0;
      Show_Fps                     : Boolean := False;
      Vid_Rec_Mode                 : Boolean := False;
      Render_OLS                   : Boolean := True;
      Shadows_Enabled              : Boolean := True;
      Fb_Effects_Enabled           : Boolean := True;
      Particles_Enabled            : Boolean := True;
      Particle_Mipmaps_Enabled     : Boolean := True;
      Auto_Blood_Wipe              : Boolean := False;
      V_Sync                       : Boolean := True;
      Hide_Gui                     : Boolean := False;
   end record;

end Settings;
