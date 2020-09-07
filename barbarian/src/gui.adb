
with GL.Culling;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types;

with Maths;

with GL_Utils;
with GUI_Atlas_Shader_Manager;
with Health_Shader_Manager;
with Image_Panel_Shader_Manager;
with Settings;

package body GUI is

    type Boolean_Array is array (Integer range <>) of Boolean;
    type Float_Array is array (Integer range <>) of Float;
    type Texture_Array is array (GL.Types.Int range <>) of
      GL.Objects.Textures.Texture;

    type Controller_Button_Overlays_Data is record
        --  temporary values for each of 3 positions
        In_Use        : Boolean_Array (1 .. 3) := (False, False, False);
        Textures      : Texture_Array (1 .. 3);
        Life_Time     : Float_Array (1 .. 3) := (0.0, 0.0, 0.0);
--          Loaded_Glyphs :
    end record;

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

    VAO_Quad_Tristrip       : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    HB_SP                   : GL.Objects.Programs.Program;
    Crong_Head_SP           : GL.Objects.Programs.Program;
    Image_Panel_SP          : GL.Objects.Programs.Program;
    Control_Button_Overlays : Controller_Button_Overlays_Data;
    Seconds_To_Show_Controller_Glyphs : constant Float := 1.5;

    --  ----------------------------------------------------------------------------

    procedure Draw_Controller_Button_Overlays (Elapsed : Float) is
        use  GL.Toggles;
        use GL.Types;
        use Singles;
        Scale_X    : constant Single := 64.0 / Single (Settings.Framebuffer_Width);
	Scale_Y    : constant Single := 64.0 / Single (Settings.Framebuffer_Height);
	X          : constant Singles.Vector3 :=
                       ((-3.0 * Scale_X), (0.0), (3.0 * Scale_X));
        X_GL_Index : constant array (1 .. 3) of GL.Index_Homogeneous :=
                       (GL.X, GL.Y, GL.X);
	Y          : constant Single := -1.0 + 3.0 * Scale_Y;
        M_Matrix   : Singles.Matrix4 := Singles.Identity4;
    begin
        GL.Culling.Set_Front_Face (GL.Types.Clockwise);
        Enable (Blend);
        Disable (Depth_Test);
        GL.Objects.Vertex_Arrays.Bind (VAO_Quad_Tristrip);
        GL.Objects.Textures.Set_Active_Unit (0);

        for index in 1 .. 3 loop
            if Control_Button_Overlays.In_Use (index) then
                Control_Button_Overlays.Life_Time (index) :=
                  Control_Button_Overlays.Life_Time (index) + Elapsed;
               if Control_Button_Overlays.Life_Time (index) >
                  Seconds_To_Show_Controller_Glyphs then
                    Control_Button_Overlays.In_Use (index) := False;
               else
                   M_Matrix := Maths.Translation_Matrix
                      ((X (X_GL_Index (index)), Y, 0.0)) *
                        Maths.Scaling_Matrix ((Scale_X, Scale_Y, 1.0));
                   Image_Panel_Shader_Manager.Set_Model_Matrix (M_Matrix);
                   GL.Objects.Textures.Targets.Texture_2D.Bind
                      (Control_Button_Overlays.Textures (Int (index)));
                   GL.Objects.Vertex_Arrays.Draw_Arrays
                      (GL.Types.Triangle_Strip, 0, 4);
                   GL_Utils.Update_Batch_Count (1);
                   GL_Utils.Update_Vertex_Count (4);
               end if;
            end if;
        end loop;

        Enable (Depth_Test);
        Disable (Blend);
        GL.Culling.Set_Front_Face (GL.Types.Counter_Clockwise);
    end Draw_Controller_Button_Overlays;

    --  -------------------------------------------------------------------------

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
