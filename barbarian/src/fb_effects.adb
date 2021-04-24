
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Low_Level.Enums;
with GL.Objects.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Programs;
with GL.Objects.Renderbuffers;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Pixels;
with GL.Types.Colors;
with GL.Window;

with Utilities;

with Game_Utils;
with GL_Utils;
with Settings;
with Shader_Attributes;
with FB_Default_Shader_Manager;
with FB_Gold_Shader_Manager;
with FB_Red_Shader_Manager;
with FB_Fadein_Shader_Manager;
with FB_Fadeout_Shader_Manager;
with FB_Screw_Shader_Manager;
with FB_Grey_Shader_Manager;
with FB_White_Shader_Manager;
with FB_Green_Shader_Manager;
with Texture_Manager;

package body FB_Effects is
   use GL.Types;
   Grey                    : constant Colors.Color := (0.6, 0.6, 0.6, 1.0);
   Num_Shader_Effects      : constant Integer := 9;
   FB_Effect_Elapsed       : Single := 0.0;
   Ww_Fb_Current_Effect    : FB_Effect := FB_Default_Effect;
   Ww_Fb_Effect_Elapsed    : Single := 0.0;
   FB_Screw_Factor         : Single := 0.0;
   Curr_Ssaa               : Single := 1.0;

   FB_VAO               : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Points_VBO           : GL.Objects.Buffers.Buffer;
   FB_Texture           : GL.Objects.Textures.Texture;
   --  g_fb:
   FB_FBO               : GL.Objects.Framebuffers.Framebuffer;
   WW_FB                : GL.Objects.Framebuffers.Framebuffer;
   WW_FB_Texture        : GL.Objects.Textures.Texture;
   Render_Buff          : GL.Objects.Renderbuffers.Renderbuffer;
   Draw_Buffers         : GL.Buffers.Explicit_Color_Buffer_List (1 .. 1);
   FB_Current_Effect    : FB_Effect := FB_Default_Effect;
   FB_Shader_Programs   : array (FB_Effect'Range) of
     GL.Objects.Programs.Program;

   FB_Durations         : constant array (FB_Effect'Range) of Single :=
                            (0.0,    --  default
                             0.25,   --  gold (was 1.0 / 2.0 * 0.5 -- wth?)
                             0.17,   --  red (was 1.0 / 3.0 * 0.5 -- ?)
                             2.0,    --  fadein
                             2.0,    --  fadeout
                             5.0,    --  screw
                             10.0,   --  greyscale
                             1.0,    --  white flash
                             0.25);  --  green

   FB_Expires           : constant array (FB_Effect'Range) of Boolean :=
                            (False, --  Default
                             True,  --  Gold
                             True,  --  Red
                             True,  --  Fade In
                             True,  --  Fade Out
                             False, --  Screw
                             False, --  Grey
                             True,  --  White
                             True); --  Green


   procedure Set_Up_FB_Textures  (FB_Width, FB_Height : Int);
   procedure Set_Up_WW_Texture (Width, Height : Int);

   --  -------------------------------------------------------------------------

   procedure Bind_Main_Scene_FB is
      use GL.Types;
      use GL.Objects.Framebuffers;
   begin
      if Settings.Fb_Effects_Enabled then
         Read_And_Draw_Target.Bind (FB_FBO);
         GL.Window.Set_Viewport
           (0, 0, Int (Single (Settings.Framebuffer_Width) * Curr_Ssaa),
            Int (Single (Settings.Framebuffer_Height) * Curr_Ssaa));
      else
         Read_And_Draw_Target.Bind (Default_Framebuffer);
         GL.Window.Set_Viewport
           (0, 0, Int (Single (Settings.Framebuffer_Width)),
            Int (Single (Settings.Framebuffer_Height)));
      end if;
   end Bind_Main_Scene_FB;

   --  -------------------------------------------------------------------------

   function Current_SSAA return GL.Types.Single is
   begin
      return Curr_Ssaa;
   end Current_SSAA;

   --  -------------------------------------------------------------------------

   procedure Draw_FB_Effects (Delta_Time : GL.Types.Single) is
      use GL.Objects.Framebuffers;
      use GL.Objects.Textures.Targets;
      Wibbly_Pass : Boolean := False;
   begin
      if Settings.Fb_Effects_Enabled then
--           Utilities.Clear_Background_Colour_And_Depth (Grey);
         FB_Effect_Elapsed := FB_Effect_Elapsed + Delta_Time;
         Ww_Fb_Effect_Elapsed := Ww_Fb_Effect_Elapsed + Delta_Time;
         Wibbly_Pass := Ww_Fb_Current_Effect /= FB_Default_Effect;
         if Wibbly_Pass then
            Read_And_Draw_Target.Bind (WW_FB);
         else
            Read_And_Draw_Target.Bind (Default_Framebuffer);
         end if;

         GL.Window.Set_Viewport (0, 0, Settings.Framebuffer_Width,
                                 Settings.Framebuffer_Height);
         Utilities.Clear_Background_Colour_And_Depth (Grey);
         GL.Objects.Textures.Set_Active_Unit (0);
         Texture_2D.Bind (FB_Texture);

         GL.Objects.Programs.Use_Program (FB_Shader_Programs (FB_Current_Effect));
         if FB_Expires (FB_Current_Effect) and
           FB_Effect_Elapsed > FB_Durations (FB_Current_Effect) then
            FB_Current_Effect := FB_Default_Effect;
         end if;

         case FB_Current_Effect is
            when FB_Gold_Flash_Effect =>
               FB_Gold_Shader_Manager.Set_Time (FB_Effect_Elapsed);
            when FB_Red_Flash_Effect =>
               FB_Red_Shader_Manager.Set_Time (FB_Effect_Elapsed);
            when FB_Fadein_Effect =>
               FB_Fadein_Shader_Manager.Set_Time (FB_Effect_Elapsed);
            when FB_Fadeout_Effect =>
               FB_Fadeout_Shader_Manager.Set_Time (FB_Effect_Elapsed);
            when FB_White_Flash_Effect =>
               FB_White_Shader_Manager.Set_Time (FB_Effect_Elapsed);
            when FB_Green_Flash_Effect =>
               FB_Green_Shader_Manager.Set_Time (FB_Effect_Elapsed);
            when others => null;
         end case;

         GL_Utils.Bind_VAO (FB_VAO);
         GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 6);

         GL.Objects.Textures.Set_Active_Unit (0);
         if Wibbly_Pass then
            Read_And_Draw_Target.Bind (Default_Framebuffer);
            Utilities.Clear_Colour_Buffer_And_Depth;
            Texture_2D.Bind (WW_FB_Texture);

            GL.Objects.Programs.Use_Program (FB_Shader_Programs (Ww_Fb_Current_Effect));
            if FB_Expires (Ww_Fb_Current_Effect) and
              Ww_Fb_Effect_Elapsed > FB_Durations (Ww_Fb_Current_Effect) then
               Ww_Fb_Current_Effect := FB_Default_Effect;
            end if;

            if Ww_Fb_Current_Effect = FB_Screw_Effect then
               GL.Objects.Programs.Use_Program (FB_Shader_Programs (FB_Screw_Effect));
               FB_Screw_Shader_Manager.Set_Time (Ww_Fb_Effect_Elapsed);
               FB_Screw_Shader_Manager.Set_Force (FB_Screw_Factor);
               GL.Objects.Programs.Use_Program (FB_Shader_Programs (FB_Current_Effect));
            end if;

            GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 6);
            Texture_2D.Bind (Texture_Manager.Get_Default_Texture);

            if FB_Screw_Factor < 0.1 then
               Ww_Fb_Current_Effect := FB_Default_Effect;
            end if;
         end if;
      end if;

   end Draw_FB_Effects;

   --  -------------------------------------------------------------------------

   procedure Init (Width, Height : Integer) is
      use GL.Attributes;
      use GL.Buffers;
      use GL.Objects.Framebuffers;
      use GL.Objects.Renderbuffers;
      use GL.Objects.Textures.Targets;
      use Shader_Attributes;
      Points       : constant Singles.Vector2_Array (1 .. 6) :=
                       ((-1.0, -1.0),   --  BL
                        ( 1.0,  1.0),   --  TR
                        (-1.0,  1.0),   --  BR
                        (-1.0, -1.0),   --  BL
                        ( 1.0, -1.0),   --  TL
                        ( 1.0,  1.0));  --  TR
      FB_Width     : constant Int := Int (Curr_Ssaa * Single (Width));
      FB_Height    : constant Int := Int (Curr_Ssaa * Single (Height));
   begin
      Game_Utils.Game_Log ("---INIT FRAMEBUFFER---");
      Draw_Buffers (1) := Color_Attachment0;
      Curr_Ssaa := Settings.Super_Sample_Anti_Aliasing;

      FB_FBO.Initialize_Id;  --  g_fb
      Read_And_Draw_Target.Bind (FB_FBO);

      Set_Up_FB_Textures (FB_Width, FB_Height);

      Draw_Buffers (1) := Color_Attachment0;
      Set_Active_Buffers (Draw_Buffers);
      if not GL_Utils.Verify_Bound_Framebuffer then
         raise FB_Effects_Exception with "Init Incomplete frambuffer";
      end if;

      --  create another FB for the wibbly wobbly after effect
      WW_FB.Initialize_Id;
      Read_And_Draw_Target.Bind (WW_FB);

      Render_Buff.Initialize_Id;
      Active_Renderbuffer.Bind (Render_Buff);
      Active_Renderbuffer.Allocate
        (GL.Pixels.Depth_Component, Int (Width), Int (Height));
      Read_And_Draw_Target.Attach_Renderbuffer (Depth_Attachment, Render_Buff);

      Set_Up_WW_Texture (Int (Width), Int (Height)) ;

      Read_And_Draw_Target.Attach_Texture (Color_Attachment_0, WW_FB_Texture, 0);
      Set_Active_Buffers (Draw_Buffers);

      if not GL_Utils.Verify_Bound_Framebuffer then
         raise FB_Effects_Exception with
           "Init framebuffer invalid";
      end if;

      FB_VAO.Initialize_Id;
      --  Create_2D_VBO initializes, binds and loads Points_VBO
      Points_VBO := GL_Utils.Create_2D_VBO (Points);
      GL_Utils.Bind_VAO (FB_VAO);
      Enable_Vertex_Attrib_Array (Attrib_VP);
      Set_Vertex_Attrib_Pointer (Attrib_VP, 2, Single_Type, False, 0, 0);

      FB_Default_Shader_Manager.Init (FB_Shader_Programs (FB_Default_Effect));
      FB_Gold_Shader_Manager.Init (FB_Shader_Programs (FB_Gold_Flash_Effect));
      FB_Red_Shader_Manager.Init (FB_Shader_Programs (FB_Red_Flash_Effect));
      FB_Fadein_Shader_Manager.Init (FB_Shader_Programs (FB_Fadein_Effect));

      FB_Fadeout_Shader_Manager.Init (FB_Shader_Programs (FB_Fadeout_Effect));
      FB_Screw_Shader_Manager.Init (FB_Shader_Programs (FB_Screw_Effect));
      FB_Grey_Shader_Manager.Init (FB_Shader_Programs (FB_Grey_Effect));
      FB_White_Shader_Manager.Init (FB_Shader_Programs (FB_White_Flash_Effect));
      FB_Green_Shader_Manager.Init (FB_Shader_Programs (FB_Green_Flash_Effect));

      FB_Current_Effect := FB_Default_Effect;
      WW_FB_Current_Effect := FB_Default_Effect;

      GL.Objects.Textures.Set_Active_Unit (0);
      Texture_2D.Bind (Texture_Manager.Get_Default_Texture);
      Read_And_Draw_Target.Bind (Default_Framebuffer);

      Game_Utils.Game_Log ("---FRAMEBUFFER INITIALIZED---");

   exception
      when others =>
         Put_Line ("An exception occurred in FB_Effects.Init.");
         raise;
   end Init;

   --  -------------------------------------------------------------------------

   procedure Set_Fade_In is
   begin
      FB_Current_Effect := FB_Fadein_Effect;
      FB_Effect_Elapsed := 0.0;
   end Set_Fade_In;

   --  -------------------------------------------------------------------------

   procedure Set_Fade_Out is
   begin
      if FB_Current_Effect /= FB_Fadeout_Effect then
         FB_Current_Effect := FB_Fadeout_Effect;
         FB_Effect_Elapsed := 0.0;
      end if;
   end Set_Fade_Out;

   --  -------------------------------------------------------------------------

   procedure FB_Gold_Flash is
   begin
      FB_Current_Effect := FB_Gold_Flash_Effect;
      FB_Effect_Elapsed := 0.0;
   end FB_Gold_Flash;

   --  -------------------------------------------------------------------------

   procedure FB_Green_Flash is
   begin
      FB_Current_Effect := FB_Green_Flash_Effect;
      FB_Effect_Elapsed := 0.0;
   end FB_Green_Flash;

   --  -------------------------------------------------------------------------

   procedure FB_White_Flash is
   begin
      FB_Current_Effect := FB_White_Flash_Effect;
      FB_Effect_Elapsed := 0.0;
   end FB_White_Flash;

   --  -------------------------------------------------------------------------

   procedure Set_Feedback_Effect (Effect : FB_Effect) is
   begin
      FB_Current_Effect := Effect;
      FB_Effect_Elapsed := 0.0;
   end Set_Feedback_Effect;

   --  -------------------------------------------------------------------------

   procedure Set_Feedback_Screw (Factor : Float) is
   begin
      Ww_Fb_Current_Effect := FB_Screw_Effect;
      FB_Screw_Factor := Single (Factor);
   end Set_Feedback_Screw;

   --  -------------------------------------------------------------------------

   procedure Set_WW_FB_Effect (Effect : FB_Effect) is
   begin
      WW_FB_Current_Effect := Effect;
      WW_FB_Effect_Elapsed := 0.0;
   end Set_WW_FB_Effect;

   --  ------------------------------------------------------------------------

   procedure Set_Up_FB_Textures (FB_Width, FB_Height : Int) is
      use GL.Objects.Textures.Targets;
      use GL.Objects.Framebuffers;
   begin
      FB_Texture.Initialize_Id;
      GL.Objects.Textures.Set_Active_Unit (0);
      Texture_2D.Bind (FB_Texture);
      if Curr_Ssaa > 1.0 then
         Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      else
         Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Nearest);
      end if;
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Nearest);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T
      Texture_2D.Load_Empty_Texture (0, GL.Pixels.SRGB_Alpha,
                                     Int (Single (FB_Width) * Curr_Ssaa),
                                     Int (Single (FB_Height) * Curr_Ssaa));
      --  Attach_Texture_2D not in OpenGLAda develop branch; use barb branch
      Read_And_Draw_Target.Attach_Texture_2D
        (Color_Attachment_0, GL.Low_Level.Enums.Texture_2D, FB_Texture);
   end Set_Up_FB_Textures;

   --  ------------------------------------------------------------------------

   procedure Set_Up_WW_Texture (Width, Height : Int) is
      use GL.Objects.Textures.Targets;
   begin
      WW_FB_Texture.Initialize_Id;
      GL.Objects.Textures.Set_Active_Unit (0);
      Texture_2D.Bind (WW_FB_Texture);

      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Nearest);
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Nearest);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T
      Texture_2D.Load_Empty_Texture (0, GL.Pixels.SRGB_Alpha, Width, Height);
   end Set_Up_WW_Texture;

   --  ------------------------------------------------------------------------

end FB_Effects;
