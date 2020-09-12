
package body Settings is

    G_Settings : Settings;

    --  ------------------------------------------------------------------------

    procedure Default_Settings is
    begin
        null;
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

    function Texture_Filter return Integer is
    begin
        return G_Settings.Texture_Filtering;
    end Texture_Filter;

    --  ------------------------------------------------------------------------

    function Tile_Batch_Width return Integer is
    begin
        return G_Settings.GL_Tile_Batch_Width;
    end Tile_Batch_Width;

    --  ------------------------------------------------------------------------

end Settings;
