
with GL.Types;

package Settings is

    type Settings is private;
    type V_GL is (V2_1, V3_2);
    subtype Audio_Volume_Range is Integer range 0 .. 10;

    procedure Default_Settings;
    function Far_Clip return GL.Types.Single;
    function Framebuffer_Height return GL.Types.Int;
    function Framebuffer_Width return GL.Types.Int;
    function Load_Settings return Boolean;
    function Particle_Mipmaps_Enabled return Boolean;
    function Save_Settings return Boolean;
    function Shadows_Enabled return Boolean;
    function Texture_Filter return Integer;
    function Tile_Batch_Width return Integer;

private
    type Settings is record
        Gfx_Presets                  : Boolean := False;
        GL_Version                   : V_GL := V3_2;
        GL_Version_To_Save           : V_GL := V3_2;
        Allow_Rand_Pitch             : Boolean := False;
        Full_Screen                  : Boolean := False;
        --  lock drawing to monitor's vertical synch i.e. 60Hz
        GL_Window_Width              : Integer := 1024;
        GL_Window_Height             : Integer := 768;
        GL_Framebuffer_Width         : GL.Types.Int := 512;
        GL_Framebuffer_Height        : GL.Types.Int := 512;
        GL_Window_Width_To_Save      : Integer := 1024;
        GL_Window_Height_To_Save     : Integer := 768;
	--  Built-in multi-sample anti-aliasing. Not used much if using FB FX
        Multi_Sample_Anti_Aliasing   : Integer := 4;
        Shadow_Size                  : Integer := 512;
        Super_Sample_Anti_Aliasing   : Float := 2.0;
        --  Texture filtering nearest = 0, bilinear, trilinear
        Texture_Filtering            : Integer := 2;
        An_Isotroic_Texturing_Factor : Float := 1.0;  -- 1.0 to 16.0
        --  Maximum rendering distance in tiles from player
        Render_Distance              : Integer := 15;
        Far_Clip                     : GL.Types.Single := 0.0;
        --  Number of tiles*tiles to put in batches
	Tile_Batch_Width             : Integer := 8;
        Audio_Volume                 : Audio_Volume_Range := 5;
        Music_Volume                 : Audio_Volume_Range := 5;
        Disable_Joystick             : Boolean := True;
        Joy_Axis_Threshold           : Float := 0.0;
	Show_Fps                     : Boolean := False;
	Vid_Rec_Mode                 : Boolean := False;
        Render_OLS                   : Boolean := True;
        Shadows_Enabled              : Boolean := True;
	Fb_effects_Enabled           : Boolean := True;
	Particles_Enabled            : Boolean := True;
	Particle_Mipmaps_Enabled     : Boolean := True;
	Auto_Blood_Wipe              : Boolean := False;
	V_Sync                       : Boolean := True;
	Hide_Gui                     : Boolean := False;
    end record;

end Settings;
