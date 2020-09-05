
with GL.Objects.Programs;

with GUI_Atlas_Shader_Manager;
with Health_Shader_Manager;
with Image_Panel_Shader_Manager;

package body GUI is

    -- texture paths
--      TEX_HEALTH_METER_BASE : constant String := "textures/hbar_black_dragon.png";
--      TEX_HEALTH_METER_RED : constant String := "textures/hbar_red_dragon.png";
--      TEX_CRONG_HEAD : constant String := "textures/gui_crong_head.png";
--      TEX_FIST : constant String := "textures/hairy_hand.png";
--      TEX_SCREEN_SPLAT : constant String := "textures/gui_splatlass.png";
--      TEX_ICON_KILLS : constant String := "textures/gui_icon_kills.png";
--      TEX_ICON_GOLD : constant String := "textures/gui_icon_coin.png";
--      TEX_ICON_JAVS : constant String := "textures/gui_icon_javs.png";

    -- shader paths
--      VS_HEALTH_METER : constant String := "health.vert";
--      FS_HEALTH_METER : constant String := "health.frag";
--      VS_CRONG_HEAD : constant String := "gui_atlas.vert";
--      FS_CRONG_HEAD : constant String := "gui_atlas.frag";
--      VS_IMAGE_PANEL : constant String := "image_panel.vert";
--      FS_IMAGE_PANEL : constant String := "image_panel.frag";

    -- audio paths
--      FIST_SOUND_FILE : constant String := "squeak_short.ogg";
--      SCREEN_SPLAT_SOUND_FILE : constant String := "GORE_Splat_Hit_Bubbles_mono.wav";
--      WIN_SOUND : constant String := "MUSIC_EFFECT_Orchestral_Battle_Neutral_stereo.wav";
--      LOSE_SOUND : constant String := "MUSIC_EFFECT_Orchestral_Battle_Negative_stereo.wav";

    HB_SP          : GL.Objects.Programs.Program;
    Crong_Head_SP  : GL.Objects.Programs.Program;
    Image_Panel_SP : GL.Objects.Programs.Program;

    --  ----------------------------------------------------------------------------

    procedure Draw_Controller_Button_Overlays (Elapsed : Float) is
    begin
        null;
    end Draw_Controller_Button_Overlays;

    --  ----------------------------------------------------------------------------
    function Init_GUIs return Boolean is
    begin
        return False;
    end Init_GUIs;

    --  ----------------------------------------------------------------------------

    function Load_Controller_Textures return Boolean is
    begin
        return False;
    end Load_Controller_Textures;

    --  ----------------------------------------------------------------------------

    procedure Load_Gui_Shaders is
    begin
        Health_Shader_Manager.Init (HB_SP);
        Image_Panel_Shader_Manager.Init (Image_Panel_SP);
        GUI_Atlas_Shader_Manager.Init (Crong_Head_SP);
    end Load_Gui_Shaders;

    --  ----------------------------------------------------------------------------

end GUI;
