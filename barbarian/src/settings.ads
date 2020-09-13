
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
        Full_Screen                  : Boolean := False;
        --  lock drawing to monitor's vertical synch i.e. 60Hz
        V_Sync                       : Boolean := False;
        GL_Window_Width              : Integer := 512;
        GL_Window_Height             : Integer := 512;
        GL_Framebuffer_Width         : GL.Types.Int := 512;
        GL_Framebuffer_Height        : GL.Types.Int := 512;
        GL_Window_Width_To_Save      : Integer := 512;
        GL_Window_Height_To_Save     : Integer := 512;
	--  Built-in multi-sample anti-aliasing. Not used much if using FB FX
        Multi_Sample_Anti_Aliasing   : Integer := 0;
        Shadow_Size                          : Integer := 0;
        Super_Sample_Anti_Aliasing   : Float := 1.0;
        --  Texture filtering nearest = 0, bilinear, trilinear
        Texture_Filtering            : Integer := 0;
        An_Isotroic_Texturing_Factor : Float := 1.0;  -- 1.0 to 16.0
        --  Maximum rendering distance in tiles from player
        Render_Distance              : Integer := 0;
        Far_Clip                     : GL.Types.Single := 0.0;
        --  Number of tiles*tiles to put in batches
	GL_Tile_Batch_Width          : Integer := 0;
        Audio_Volume                 : Audio_Volume_Range := 0;
        Music_Volume                 : Audio_Volume_Range := 0;
        Allow_Random_Pitch           : Boolean := False;
        Disable_Joystick             : Boolean := True;
        Joy_Axis_Threshold           : Float := 0.0;
        Render_OLS                   : Boolean := False;
        Shadows_Enabled              : Boolean := False;
	Fb_effects_Enabled           : Boolean := False;
	Particles_Enabled            : Boolean := False;
	Auto_Blood_Wipe              : Boolean := False;
	--  Can disable to get around bug in MESA renderer
	Particle_Mipmaps_Enabled     : Boolean := False;
	Show_Fps                     : Boolean := False;
	Vid_Rec_Mode                 : Boolean := False;
	--  CL switch with -hidegui
	Hide_Gui                     : Boolean := True;
    end record;

end Settings;
