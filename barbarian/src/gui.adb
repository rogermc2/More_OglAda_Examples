
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
with GL.Types; use GL.Types;

with Maths;

--  with Attributes;
with Controller_Textures_Manager;
with Game_Utils;
with GL_Maths;
with GL_Utils;
with GUI_Atlas_Shader_Manager;
with Health_Shader_Manager;
with Image_Panel_Shader_Manager;
with Settings;
with Shader_Attributes;
with Text;
with Texture_Manager;

package body GUI is

   type Boolean_Array is array (Integer range <>) of Boolean;
   type Texture_Array is array (Integer range <>) of
     GL.Objects.Textures.Texture;

   type GUI_Icon_Data is record
      XY_Position       : Single_Array (1 .. 6) := (others => 0.0);
      Anim_Countdowns   : Single_Array (1 .. 3) := (others => 0.0);
      Textures          : Texture_Array (1 .. 3);
   end record;

   type Controller_Button_Overlays_Data is record
      --  temporary values for each of 3 positions
      In_Use            : Boolean_Array (1 .. 3) := (False, False, False);
      Textures          : Texture_Array (1 .. 3);
      Life_Time         : Single_Array (1 .. 3) := (0.0, 0.0, 0.0);
      Loaded_Glyphs_IDs : Texture_Array
        (1 .. Controller_Textures_Manager.Num_Steam_Controller_Images);
   end record;

   --   Absolute pixel sizes of gui elements
   Seconds_To_Show_Controller_Glyphs : constant Single := 1.5;
   Health_Bar_Width_Px               : constant Int := 256;
   Health_Bar_Height_Px              : constant Int := 64;
   Crong_Head_Width_Px               : constant Int := 80;
   Crong_Head_Height_Px              : constant Int := 80;
   Gold_Panel_Width_Px               : constant Int := 512;
   Gold_Panel_Height_Px              : constant Int := 64;
   Fist_Scale                        : constant Single := 1.2;
   Screen_Splat_Scale                : constant Single := 0.27;

   --  player and enemy health bars
   Health_Bar_Factor  : Single_Array (1.. 2) := (1.0, 0.0);
   Player_Hb_Mat      : Singles.Matrix4 := Singles.Identity4;
   Enemy_Hb_Mat       : Singles.Matrix4 := Singles.Identity4;
   Fist_Mat           : Singles.Matrix4 := Singles.Identity4;
   Defeated_Model_Mat : Singles.Matrix4 := Singles.Identity4;
   Ch_Model_Mat       : Singles.Matrix4 := Singles.Identity4;

   --  Effect settings
   Max_Screen_Splats  : constant Integer := 32;
   Fist_Time          : constant Float := 0.75;
   Javelin_X_Offset   : constant Float := 1.25;

   -- textures
   Health_Texture_Base  : GL.Objects.Textures.Texture;
   Health_Texture_Red   : GL.Objects.Textures.Texture;
   Crong_Head_Texture   : GL.Objects.Textures.Texture;
   Fist_Texture         : GL.Objects.Textures.Texture;
   Screen_Splat_Texture : GL.Objects.Textures.Texture;

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
   Gold_Text_Index          : Integer := 0;
   Javelin_Ammo_Text_Index  : Integer := 0;
   Kills_Text_Index         : Integer := 0;
   Bottom_Health_Text_Index          : Integer := 0;
   Top_Health_Text_Index             : Integer := 0;
   Finish_Stats_Text_Index           : Integer := 0;
   Bottom_Health_Name                : Unbounded_String := To_Unbounded_String ("");
   Top_Health_Name                   : Unbounded_String := To_Unbounded_String ("");

   VAO_Quad_Tristrip        : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   HB_SP                    : GL.Objects.Programs.Program;
   Crong_Head_SP            : GL.Objects.Programs.Program;
   Image_Panel_SP           : GL.Objects.Programs.Program;
   Control_Button_Overlays  : Controller_Button_Overlays_Data;
   GUI_Icons                : GUI_Icon_Data;
   GUIs_Initialized         : Boolean := False;

   procedure Init_Crong_Head;
   procedure Init_Fist;
   procedure Init_Health_Bar;
   procedure Init_Screen_Splat;

   --  ----------------------------------------------------------------------------

   procedure Draw_Controller_Button_Overlays (Elapsed : Float) is
      use  GL.Toggles;
      use GL.Types;
      use Singles;
      Scale_X    : constant Single := 64.0 / Single (Settings.Framebuffer_Width);
      Scale_Y    : constant Single := 64.0 / Single (Settings.Framebuffer_Height);
      X          : constant Singles.Vector3 :=
                     (-3.0 * Scale_X, 0.0, 3.0 * Scale_X);
      X_GL_Index : constant array (Int range 1 .. 3) of GL.Index_Homogeneous :=
                     (GL.X, GL.Y, GL.X);
      Y          : constant Single := -1.0 + 3.0 * Scale_Y;
      M_Matrix   : Singles.Matrix4 := Singles.Identity4;
   begin
      GL.Culling.Set_Front_Face (GL.Types.Clockwise);
      Enable (Blend);
      Disable (Depth_Test);
--        GL.Objects.Vertex_Arrays.Bind (VAO_Quad_Tristrip);
      GL_Utils.Bind_VAO (VAO_Quad_Tristrip);
      GL.Objects.Textures.Set_Active_Unit (0);

      for index in 1 .. 3 loop
         if Control_Button_Overlays.In_Use (index) then
            Game_Utils.Game_Log ("GUI.Draw_Controller_Button_Overlays, Control_Button_Overlay In_Use");
            Control_Button_Overlays.Life_Time (Int (index)) :=
              Control_Button_Overlays.Life_Time (Int (index)) + Single (Elapsed);
            if Control_Button_Overlays.Life_Time (Int (index)) >
              Seconds_To_Show_Controller_Glyphs then
               Control_Button_Overlays.In_Use (index) := False;
            else
               M_Matrix := Maths.Translation_Matrix
                 ((X (X_GL_Index (Int (index))), Y, 0.0)) *
                   Maths.Scaling_Matrix ((Scale_X, Scale_Y, 1.0));
               Image_Panel_Shader_Manager.Set_Model_Matrix (M_Matrix);
               GL.Objects.Textures.Targets.Texture_2D.Bind
                 (Control_Button_Overlays.Textures (index));
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

   procedure Init_Crong_Head is
      use GL.Types;
      use GL.Types.Singles;
      use Maths;
      Scale_X  : constant Single := Single (Crong_Head_Width_Px) /
                   Single (Settings.Framebuffer_Width);
      Scale_Y  : constant Single := Single (Crong_Head_Height_Px) /
                   Single (Settings.Framebuffer_Height);
      Scale_XY : constant Singles.Matrix4 :=
                   Scaling_Matrix ((Scale_X, Scale_Y, 1.0));
      X        : constant Single := -1.0 + Single (Crong_Head_Width_Px + 5) /
                   Single (Settings.Framebuffer_Width);
      Y        : constant Single := -1.0 + Scale_Y - 15.0 /
                   Single (Settings.Framebuffer_Height);
   begin
      GUI_Atlas_Shader_Manager.Init (Crong_Head_SP);
      Ch_Model_Mat := Translation_Matrix ((X, Y, 0.0)) * Scale_XY;
      Texture_Manager.Load_Image_To_Texture
        ("src/textures/gui_crong_head.png", Crong_Head_Texture, False, True);
   end Init_Crong_Head;

   --  --------------------------------------------------------------------------

   procedure Init_Fist is
      use GL.Types;
      use GL.Types.Singles;
      use Maths;
      Scale_X  : Single := Fist_Scale;
      Scale_Y  : Single := Fist_Scale;
      Scale_XY : constant Singles.Matrix4 :=
                   Scaling_Matrix ((Scale_X, Scale_Y, 1.0));
      Aspect   : constant Single := Single (Settings.Framebuffer_Width) /
                   Single (Settings.Framebuffer_Height) ;
      X        : constant Single := 1.0 - Scale_X;
      Y        : constant Single := -1.0 + Scale_Y;
   begin
      if Settings.Framebuffer_Width > Settings.Framebuffer_Height then
         Scale_X := Fist_Scale / Aspect;
         Scale_Y := 1.0;
      else
         Scale_X := Fist_Scale;
         Scale_Y := Aspect * Fist_Scale;
      end if;
      Fist_Mat := Translation_Matrix ((X, Y, 0.0)) * Scale_XY;
      Texture_Manager.Load_Image_To_Texture
        ("src/textures/hairy_hand.png", Fist_Texture, False, True);

   end Init_Fist;

   --  --------------------------------------------------------------------------

   procedure Init_GUIs is
      use GL.Types;
      Quad_Tristrip : constant Singles.Vector2_Array (1 .. 4) :=
                        ((-1.0, -1.0),
                         (-1.0, 1.0),
                         (1.0, -1.0),
                         (1.0, 1.0));
      Quad_VBO      : GL.Objects.Buffers.Buffer;
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

      Init_Crong_Head;
      Init_Fist;
      Init_Health_Bar;
      Init_Screen_Splat;

      Bottom_Health_Name := To_Unbounded_String ("crongdor");
      Top_Health_Name := To_Unbounded_String ("enemy");

      Bottom_Health_Text_Index :=
        Text.Add_Text (To_String (Bottom_Health_Name),
                       0.0, 0.0, 32.0, 1.0, 1.0, 1.0, 1.0);
      Text.Centre_Text (Bottom_Health_Text_Index,
                        Single (5 + Health_Bar_Width_Px + 2 * Crong_Head_Width_Px)
                        / Single (Settings.Framebuffer_Width) - 1.0,
                        Single (32 + Health_Bar_Height_Px)
                        / Single (Settings.Framebuffer_Height) - 1.0);
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
      Game_Utils.Game_Log ("---GUIs  Initialized---");

   exception
      when others =>
         Put_Line ("An exception occurred in Gui.Init_GUIs.");
         raise;
   end Init_GUIs;

   --  ----------------------------------------------------------------------------

   procedure Init_Health_Bar is
      use GL.Types.Singles;
      use Maths;
      Scale_X  : constant Single := Single (Health_Bar_Width_Px) /
                   Single (Settings.Framebuffer_Width);
      Scale_Y  : constant Single := Single (Health_Bar_Height_Px) /
                   Single (Settings.Framebuffer_Height);
      Scale_XY : constant Singles.Matrix4 :=
                   Scaling_Matrix ((Scale_X, Scale_Y, 1.0));
      X        : Single := 0.0;
      Y        : Single := 1.0 - Scale_Y;
   begin
      Texture_Manager.Load_Image_To_Texture
        ("src/textures/hbar_red_dragon.png", Health_Texture_Red, False, True);
      Texture_Manager.Load_Image_To_Texture
        ("src/textures/hbar_black_dragon.png", Health_Texture_Base, False, True);
      Health_Shader_Manager.Init (HB_SP);
      Health_Shader_Manager.Set_Red_Texture (0);
      Health_Shader_Manager.Set_Red_Texture (1);
      Enemy_Hb_Mat := Translation_Matrix ((X, Y, 0.0)) * Scale_XY;
      X := -1.0 + Scale_X + Single (2 * Crong_Head_Width_Px + 5) /
        Single (Settings.Framebuffer_Width);
      Y  := -1.0 + Scale_Y;
      Player_Hb_Mat := Translation_Matrix ((X, Y, 0.0)) * Scale_XY;
   end Init_Health_Bar;

   --  --------------------------------------------------------------------------

   procedure Init_Screen_Splat is
   begin
      Texture_Manager.Load_Image_To_Texture
        ("src/textures/gui_splatlass.png", Screen_Splat_Texture, False, True);
   end Init_Screen_Splat;

   --  --------------------------------------------------------------------------

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

   procedure Show_Controller_Button_Overlay (Pos_Index, Tex_Index : in out Integer) is
   begin
      if Pos_Index < 1 or Pos_Index > 3 then
         Game_Utils.Game_Log ("GUI.Show_Controller_Button_Overlay, WARNING: " &
                              "gui overlay pos_idx " & Integer'Image (Pos_Index)
                              & " is invalid.");
         Pos_Index := 2;
      end if;
      if Tex_Index < 1 or
        Tex_Index > Controller_Textures_Manager.Num_Steam_Controller_Images then
         Game_Utils.Game_Log
           ("GUI.Show_Controller_Button_Overlay, WARNING: controller glyph texture index "
              & Integer'Image (Tex_Index) & " is invalid.");
         Tex_Index := 1;
      end if;

      Control_Button_Overlays.In_Use (Pos_Index) := True;
      Control_Button_Overlays.Life_Time (Int (Pos_Index)) := 0.0;
      Control_Button_Overlays.Textures (Pos_Index) :=
      Control_Button_Overlays.Loaded_Glyphs_IDs (Tex_Index);

   end Show_Controller_Button_Overlay;

   --  ----------------------------------------------------------------------------

end GUI;
