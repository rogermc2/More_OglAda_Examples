
package body Settings is

    G_Settings : Settings;

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
        G_Settings.Aniso := 4;
        G_Settings.Audio_Volume := 5;
        G_Settings.Music_Volume := 5;
        G_Settings.Render_Dist := 15;
        G_Settings.Far_Clip := 40.0;
        G_Settings.Tile_Batch_Width := 8;
        G_Settings.Render_OLS := True;
        G_Settings.Shadows_Enabled := True;
        G_Settings.Fb_Effects_Enabled := True;
        G_Settings.Particles_Enabled := True;
        G_Settings.Particle_Mipmaps_Enabled := True;
        G_Settings.Auto_Blood_Wipe := False;
        G_Settings.V_Sync := True;
        G_Settings.Hide_Gui := False;

    end Default_Settings;

    --  ------------------------------------------------------------------------

    function Far_Clip return GL.Types.Single is
    begin
        return G_Settings.Far_Clip;
    end Far_Clip;

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

    function Load_Settings return Boolean is
    begin
        return True;
    end Load_Settings;

    --  ------------------------------------------------------------------------

    function Particle_Mipmaps_Enabled return Boolean is
    begin
        return G_Settings.Particle_Mipmaps_Enabled;
    end Particle_Mipmaps_Enabled;

    --  ------------------------------------------------------------------------

    function Save_Settings return Boolean is
    begin
        return True;
    end Save_Settings;

    --  ------------------------------------------------------------------------

    function Shadows_Enabled return Boolean is
    begin
        return G_Settings.Shadows_Enabled;
    end Shadows_Enabled;

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

    function Tile_Batch_Width return Integer is
    begin
        return G_Settings.Tile_Batch_Width;
    end Tile_Batch_Width;

    --  ------------------------------------------------------------------------

end Settings;
