
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Attributes;
with GL.Culling;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types;

with Maths;

--  with Attributes;
with Game_Utils;
with GL_Utils;
with GUI_Atlas_Shader_Manager;
with Health_Shader_Manager;
with Image_Panel_Shader_Manager;
with Settings;
with Shader_Attributes;
with Text;

package body GUI is

    type Boolean_Array is array (Integer range <>) of Boolean;
    type Float_Array is array (Integer range <>) of Float;
    --      type Integer_Array is array (Integer range <>) of Integer;
    type Texture_Array is array (GL.Types.Int range <>) of
      GL.Objects.Textures.Texture;

    --      Num_Steam_Controller_Images : constant Integer := 43;
--      type GUI_Icon_Data is record
--          XY_Position       : Float_Array (1 .. 6) := (others => 0.0);
--          Anim_Countdowns   : Float_Array (1 .. 3) := (others => 0.0);
--          Textures          : Texture_Array (1 .. 3);
--      end record;

    type Controller_Button_Overlays_Data is record
    --  temporary values for each of 3 positions
        In_Use        : Boolean_Array (1 .. 3) := (False, False, False);
        Textures      : Texture_Array (1 .. 3);
        Life_Time     : Float_Array (1 .. 3) := (0.0, 0.0, 0.0);
        --          Loaded_Glyphs_IDs : Integer_Array (1 .. Num_Steam_Controller_Images)
        --            := (others => 0);
    end record;

    --   Absolute pixel sizes of gui elements
    Seconds_To_Show_Controller_Glyphs : constant Float := 1.5;
    Health_Bar_Width_Px               : constant Integer := 256;
    Health_Bar_Height_Px              : constant Integer := 64;
    Crong_Head_Width_Px               : constant Integer := 80;
--      Crong_Head_Height_Px              : constant Integer := 80;
--      Gold_Panel_Width_Px               : constant Integer := 512;
--      Gold_Panel_Height_Px              : constant Integer := 64;
--      Fist_Scale                        : constant Float := 1.2;
--      Screen_Splat_Scale                : constant Float := 0.27;

    --  Effect settings
--      Max_Screen_Splats                 : constant Integer := 32;
--      Fist_Time                         : constant Float := 0.75;
--      Javelin_X_Offset                  : constant Float := 1.25;

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

    --  Text
--      Gold_Text_Index          : Integer := 0;
--      Javelin_Ammo_Text_Index  : Integer := 0;
--      Kills_Text_Index         : Integer := 0;
    Bottom_Health_Text_Index : Integer := 0;
    Top_Health_Text_Index    : Integer := 0;
    Finish_Stats_Text_Index  : Integer := 0;
    Bottom_Health_Name       : Unbounded_String := To_Unbounded_String ("");
    Top_Health_Name          : Unbounded_String := To_Unbounded_String ("");

    VAO_Quad_Tristrip        : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    HB_SP                    : GL.Objects.Programs.Program;
    Crong_Head_SP            : GL.Objects.Programs.Program;
    Image_Panel_SP           : GL.Objects.Programs.Program;
    Control_Button_Overlays  : Controller_Button_Overlays_Data;
--      GUI_Icons                : GUI_Icon_Data;
    GUIs_Initialized         : Boolean := False;

    function Init_Crong_Head return Boolean;
    function Init_Fist return Boolean;
    function Init_Health_Bar return Boolean;
    function Init_Screen_Splat return Boolean;

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

    function Init_Crong_Head return Boolean is
    begin

        return False;
    end Init_Crong_Head;

    --  --------------------------------------------------------------------------

    function Init_Fist return Boolean is
    begin

        return False;
    end Init_Fist;

    --  --------------------------------------------------------------------------

    function Init_GUIs return Boolean is
        use GL.Types;
        Quad_Tristrip : constant Singles.Vector2_Array (1 .. 4) :=
                          ((-1.0, -1.0),
                           (-1.0, 1.0),
                           (1.0, -1.0),
                           (1.0, 1.0));
        Quad_VBO      : GL.Objects.Buffers.Buffer;
        Result        : Boolean := False;
    begin
        Game_Utils.Game_Log ("--- GUI.Init_GUIs ___");
        GUIs_Initialized := False;
        --  Load generic VAO for 2d panels
        VAO_Quad_Tristrip.Initialize_Id;
        VAO_Quad_Tristrip.Bind;

        Quad_VBO := GL_Utils.Create_2D_VBO (Quad_Tristrip);
        GL.Objects.Buffers.Array_Buffer.Bind (Quad_VBO);
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (Shader_Attributes.Attrib_VP, 2, Single_Type, False, 0, 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (Shader_Attributes.Attrib_VP);

        Result := Init_Crong_Head;
        Result := Result and  Init_Fist;
        Result := Result and  Init_Health_Bar;
        Result := Result and  Init_Screen_Splat;

        if Result then
            Bottom_Health_Name := To_Unbounded_String ("crongdor");
            Top_Health_Name := To_Unbounded_String ("enemy");

            Bottom_Health_Text_Index :=
              Text.Add_Text (To_String (Bottom_Health_Name),
                             0.0, 0.0, 32.0, 1.0, 1.0, 1.0, 1.0);
            Text.Centre_Text (Bottom_Health_Text_Index,
                              Float (5 + Health_Bar_Width_Px + 2 * Crong_Head_Width_Px)
                              / Float (Settings.Framebuffer_Width) - 1.0,
                              Float (32 + Health_Bar_Height_Px)
                              / Float (Settings.Framebuffer_Height) - 1.0);
            Top_Health_Text_Index :=
              Text.Add_Text (To_String (Top_Health_Name),
                             0.0, 0.0, 32.0, 1.0, 1.0, 1.0, 1.0);
            Text.Centre_Text (Top_Health_Text_Index, 0.0, 0.0);
            Text.Set_Text_Visible (Top_Health_Text_Index, False);
            Finish_Stats_Text_Index  :=
              Text.Add_Text ("Level finished.",
                             0.0, 0.0, 24.0, 0.0, 1.0, 0.0, 1.0);
            Text.Centre_Text (Finish_Stats_Text_Index, 0.0, 0.0);
            Text.Set_Text_Visible (Finish_Stats_Text_Index, False);
            GUIs_Initialized := True;
        end if;
        return GUIs_Initialized;

    exception
        when others =>
            Put_Line ("An exception occurred in Gui.Init_GUIs.");
            raise;
    end Init_GUIs;

    --  ----------------------------------------------------------------------------

    function Init_Health_Bar return Boolean is
    begin

        return False;
    end Init_Health_Bar;

    --  --------------------------------------------------------------------------

    function Init_Screen_Splat return Boolean is
    begin

        return False;
    end Init_Screen_Splat;

    --  --------------------------------------------------------------------------

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

--      procedure Set_GUI_Gold (Amount : Integer) is
--      begin
--          Text.Update_Text (Gold_Text_Index, Integer'Image (Amount));
--          GUI_Icons.Anim_Countdowns (2) := 1.0;
--      end Set_GUI_Gold;

    --  ----------------------------------------------------------------------------

--      procedure Set_GUI_Javalin_Ammo (Amount : Integer) is
--      begin
--          Text.Update_Text (Javelin_Ammo_Text_Index, Integer'Image (Amount));
--          GUI_Icons.Anim_Countdowns (1) := 1.0;
--      end Set_GUI_Javalin_Ammo;

    --  ------------------------------------------------------------------------

--      procedure Set_GUI_Kills (Amount : Integer) is
--      begin
--          Text.Update_Text (Gold_Text_Index, Integer'Image (Amount));
--          GUI_Icons.Anim_Countdowns (3) := 1.0;
--      end Set_GUI_Kills;

    --  ----------------------------------------------------------------------------

end GUI;
