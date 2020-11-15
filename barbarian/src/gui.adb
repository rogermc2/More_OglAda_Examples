
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

with Maths;

--  with Attributes;
with Audio;
with Character_Controller;
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
   use GL.Types;
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

   type Screen_Splat_Data is record
      Position     : Singles.Vector2  := (0.0, 0.0);
      Alpha        : Single := 0.0;
      Speed        : Single := 0.0;
      Model_Matrix : Singles.Matrix4 := Singles.Identity4;
      Sprite_Index : Positive := 1;
      Is_Active    : Boolean := False;
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
   Health_Bar_Factor  : Single_Array (1 .. 2) := (1.0, 0.0);
   Player_Hb_Mat      : Singles.Matrix4 := Singles.Identity4;
   Enemy_Hb_Mat       : Singles.Matrix4 := Singles.Identity4;
   Fist_Mat           : Singles.Matrix4 := Singles.Identity4;
   Defeated_Model_Mat : Singles.Matrix4 := Singles.Identity4;
   Ch_Model_Mat       : Singles.Matrix4 := Singles.Identity4;

   --  misc effect settings
   Blood_Overlay_Alpha      : Float := 1.0;
--     Cronhead_Sprite_Index    : Float;
   Num_Active_Screen_Splats : Natural := 0;

   --  gui flag
   Guis_Initialized    : Boolean := False;
   Fist_Activated      : Boolean := False;
   Show_Defeated_State : Boolean := False;
   Show_Victory_State  : Boolean := False;

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
   Fist_Sound_File : constant String := "squeak_short.ogg";
   Screen_Splat_Sound_File : constant String := "GORE_Splat_Hit_Bubbles_mono.wav";
   --      WIN_SOUND : constant String := "MUSIC_EFFECT_Orchestral_Battle_Neutral_stereo.wav";
   Lose_Sound      : constant String := "MUSIC_EFFECT_Orchestral_Battle_Negative_stereo.wav";

   --  timers
   Fist_Countdown           : Float := 0.0;

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
   Crong_Head_Sprite_Index  : Positive := 6;

   Screen_Splats            : array (1 .. Max_Screen_Splats) of Screen_Splat_Data;
   Next_Splat_Index         : Natural := 0;

   procedure Init_Crong_Head;
   procedure Init_Fist;
   procedure Init_Health_Bar;
   procedure Init_Screen_Splat;
   procedure Start_Fist;
   procedure Update_Screen_Splats (Seconds : Float);

   --  ----------------------------------------------------------------------------

   procedure Add_Screen_Splats (Number : Integer) is
      use GL.Types.Singles;
      use Settings;
      Aspect    : Single;
      Scale_X   : Single;
      Scale_Y   : Single;
      X         : Single;
      Y         : Single;
      Scale_Mat : Singles.Matrix4;
      Trans_Mat : Singles.Matrix4;
   begin
      Blood_Overlay_Alpha := 1.0;
      for index in 1 .. Number loop
         if Screen_Splats (Next_Splat_Index).Is_Active then
            Num_Active_Screen_Splats := Num_Active_Screen_Splats - 1;
         end if;
         Screen_Splats (Next_Splat_Index).Is_Active := True;
         Screen_Splats (Next_Splat_Index).Alpha := 1.0;
         Scale_X := Screen_Splat_Scale;
         Scale_Y := Scale_X;
         Aspect := Single (Framebuffer_Width) / Single (Framebuffer_Height);
         if Framebuffer_Width > Framebuffer_Height then
            Scale_X := Scale_X / Aspect;
         else
            Scale_Y := Aspect * Scale_Y;
         end if;

         X := 2.0 * Abs (Maths.Random_Float) - 1.0 - Scale_X;
         Y := 2.0 * Abs (Maths.Random_Float) - 1.0 + Scale_Y;
         Screen_Splats (Next_Splat_Index).Position := (X, Y);
         Scale_Mat := Maths.Scaling_Matrix ((Scale_X, Scale_Y, 1.0));
         Trans_Mat := Maths.Translation_Matrix ((X, Y, 0.0));
         Screen_Splats (Next_Splat_Index).Model_Matrix := Trans_Mat * Scale_Mat;
         --  between 0 and 0.5 (max 2 seconds to clear whole screen)
         Screen_Splats (Next_Splat_Index).Speed := 0.25 * Abs (Maths.Random_Float);
         Next_Splat_Index := (Next_Splat_Index + 1) mod Max_Screen_Splats + 1;
         Num_Active_Screen_Splats := Num_Active_Screen_Splats + 1;
      end loop;
      Audio.Play_Sound (Screen_Splat_Sound_File, True);
      if Auto_Blood_Wipe then
         Start_Fist;
      end if;
   end Add_Screen_Splats;

   --  --------------------------------------------------------------------------

   procedure Change_Crong_Head (Health_Factor : Single) is
      Health_Sprite_Index : Positive := 6;
   begin
      if Health_Factor > 0.8 then
         Health_Sprite_Index := 1;
      elsif Health_Factor > 0.6 then
         Health_Sprite_Index := 2;
      elsif Health_Factor > 0.4 then
         Health_Sprite_Index := 3;
      elsif Health_Factor > 0.2 then
         Health_Sprite_Index := 4;
      elsif Health_Factor > 0.0 then
         Health_Sprite_Index := 5;
      end if;
      Crong_Head_Sprite_Index := Health_Sprite_Index;
   end Change_Crong_Head;

   --  --------------------------------------------------------------------------

   procedure Change_Health_Bar (Index : Int; Health_Factor : Single;
                                Name  : String) is
      S_FB_Width    : constant Single := Single (Settings.Framebuffer_Width);
      S_FB_Height   : constant Single := Single (Settings.Framebuffer_Height);
      S_HBar_Width  : constant Single := Single (Health_Bar_Width_Px);
      S_HBar_Height : constant Single := Single (Health_Bar_Height_Px);
   begin
      Health_Bar_Factor (Index) := Health_Factor;
      if Index = 1 then
         if Name /= Bottom_Health_Name then
            Bottom_Health_Name := To_Unbounded_String (Name);
            Text.Update_Text (Bottom_Health_Text_Index, Name);
            Text.Centre_Text (Bottom_Health_Text_Index,
                              S_HBar_Width / S_FB_Width - 1.0,
                              S_HBar_Height / S_FB_Height - 1.0);
         else
            if Name /= Top_Health_Name then
               Top_Health_Name := To_Unbounded_String (Name);
               Text.Update_Text (Top_Health_Text_Index, Name);
               Text.Centre_Text (Top_Health_Text_Index,
                                 S_HBar_Width / S_FB_Width - 1.0,
                                 (S_HBar_Height + 32.0) / S_FB_Height - 1.0);
            end if;
         end if;
      end if;

   end Change_Health_Bar;

   --  --------------------------------------------------------------------------

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

   procedure Hide_Finish_Stats is
   begin
      Text.Set_Text_Visible (Finish_Stats_Text_Index, True);
   end Hide_Finish_Stats;

   --  --------------------------------------------------------------------------

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

   procedure Init_Gold_Counter is
      use GL.Types.Singles;
      use Maths;
      FB_Width  : constant Single := Single (Settings.Framebuffer_Width);
      FB_Height : constant Single := Single (Settings.Framebuffer_Height);
      X_Ref    : constant Single := -1023.0 / FB_Width;
      Y        : constant Single := 63.0 / FB_Height;
      X        : Single;
   begin
      Texture_Manager.Load_Image_To_Texture
        ("src/textures/gui_icon_javs.png", GUI_Icons.Textures (1), False, True);
      Texture_Manager.Load_Image_To_Texture
        ("src/textures/gui_icon_coin.png", GUI_Icons.Textures (2), False, True);
      Texture_Manager.Load_Image_To_Texture
        ("src/textures/gui_icon_javs.png", GUI_Icons.Textures (3), False, True);

      X := X_Ref + 384.0 / FB_Width;
      Javelin_Ammo_Text_Index := Text.Add_Text
        ("0", X, Y, 20.0, 1.0, 1.0, 1.0, 1.0);
      Text.Set_Text_Visible (Javelin_Ammo_Text_Index, False);

      X := X_Ref + 640.0 / FB_Width;
      Gold_Text_Index := Text.Add_Text
        ("0", X, Y, 20.0, 1.0, 1.0, 1.0, 1.0);
      Text.Set_Text_Visible (Gold_Text_Index, False);

      X := X_Ref + 896.0 / FB_Width;
      Kills_Text_Index := Text.Add_Text
        ("0", X, Y, 20.0, 1.0, 1.0, 1.0, 1.0);
      Text.Set_Text_Visible (Kills_Text_Index, False);
   end Init_Gold_Counter;

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

   procedure Reset_GUIs is
   begin
      Change_Health_Bar (1, 1.0, "crongdor");
      Change_Health_Bar (2, 1.0, "name");
      Change_Crong_Head (1.0);
      Fist_Activated := False;
      for index in 1 .. Max_Screen_Splats loop
         Screen_Splats (index).Is_Active := False;
      end loop;
      Num_Active_Screen_Splats := 0;
      Show_Defeated_State := False;
      Show_Victory_State := False;
      Hide_Finish_Stats;
   end Reset_GUIs;

   --  ----------------------------------------------------------------------------

       procedure Set_GUI_Gold (Amount : Integer) is
       begin
           Text.Update_Text (Gold_Text_Index, Integer'Image (Amount));
           GUI_Icons.Anim_Countdowns (2) := 1.0;
       end Set_GUI_Gold;

   --  ----------------------------------------------------------------------------

       procedure Set_GUI_Javalin_Ammo (Amount : Integer) is
       begin
           Text.Update_Text (Javelin_Ammo_Text_Index, Integer'Image (Amount));
           GUI_Icons.Anim_Countdowns (1) := 1.0;
       end Set_GUI_Javalin_Ammo;

   --  ------------------------------------------------------------------------

       procedure Set_GUI_Kills (Amount : Integer) is
       begin
           Text.Update_Text (Kills_Text_Index, Integer'Image (Amount));
           GUI_Icons.Anim_Countdowns (3) := 1.0;
       end Set_GUI_Kills;

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

   function Show_Defeated return Boolean is
   begin
      return Show_Defeated_State;
   end Show_Defeated;

   --  ----------------------------------------------------------------------------

   procedure Show_Defeated_Screen (Show : Boolean) is
      use Character_Controller;
   begin
      if Show then
         Audio.Play_Sound (Lose_Sound, False);
         Show_Finished_Stats (False, Current_Kills, Max_Kills,
                             Total_Treasure_Found, Gold_Max, 0.0, "n/a");
      else
         Hide_Finish_Stats;
      end if;
      Show_Defeated_State := True;
   end Show_Defeated_Screen;

   --  ----------------------------------------------------------------------------

   procedure Show_Finished_Stats (Won : Boolean;
                                  Kills, Kills_Max, Gold, Gold_Max : Integer;
                                  Time  : Float; Par : String) is
      Mins  : Integer;
      Secs  : Integer;
      Stats : Unbounded_String;
   begin
      if Won then
         Mins := Integer (Float'Floor (Time / 60.0));
         Secs := Integer (Time - Float (60 * Mins));
         Stats := To_Unbounded_String
           ("VICTORY! kills   " & Integer'Image (kills) & " / " &
            Integer'Image (kills_max) & "gold    " & Integer'Image (gold) &
            "  / " & Integer'Image (gold_max) & " time    " &
            Integer'Image (Mins) & ":" & Integer'Image (Secs) & "par " & Par);
      else
         Stats := To_Unbounded_String
           ("DEFEAT! kills   " & Integer'Image (kills) & " / " &
            Integer'Image (kills_max) & "gold    " & Integer'Image (gold) &
            "  / " & Integer'Image (gold_max));
      end if;
      Text.Update_Text (Finish_Stats_Text_Index, To_String (Stats));
      Text.Centre_Text (Finish_Stats_Text_Index,
                        0.0, 256.0 / Single (Settings.Framebuffer_Height));
      Text.Set_Text_Visible (Finish_Stats_Text_Index, True);
   end Show_Finished_Stats;

   --  ----------------------------------------------------------------------------

   function Show_Victory return Boolean is
   begin
      return Show_Victory_State;
   end Show_Victory;

   --  ----------------------------------------------------------------------------

   procedure Start_Fist is
      Activated : constant Boolean := Fist_Activated;
   begin
      if not Activated then
         Blood_Overlay_Alpha := 1.0;
         if Settings.Auto_Blood_Wipe then
            Fist_Countdown := 2.0 * Fist_Time;
         else
            Fist_Countdown := Fist_Time;
            Audio.Play_Sound (Fist_Sound_File, True);
         end if;
         Fist_Activated := True;
      end if;
   end Start_Fist;

   --  ----------------------------------------------------------------------------

   procedure Update_Fist (Seconds : Float) is
   begin
      null;
   end Update_Fist;

   --  ----------------------------------------------------------------------------

   procedure Update_Gold_Bar (Seconds : Float) is
   begin
      null;
   end Update_Gold_Bar;

   --  ----------------------------------------------------------------------------

   procedure Update_GUIs (Seconds : Float) is
   begin
      Update_Fist (Seconds);
      Update_Screen_Splats (Seconds);
      Update_Gold_Bar (Seconds);
   end Update_GUIs;

   --  ----------------------------------------------------------------------------

   procedure Update_Screen_Splats (Seconds : Float) is
   begin
      null;
   end Update_Screen_Splats;

   --  ----------------------------------------------------------------------------


end GUI;
