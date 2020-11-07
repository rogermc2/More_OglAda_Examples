
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Programs;
with GL.Objects.Renderbuffers;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Pixels;
with GL.Types; use GL.Types;

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

package body FB_Effects is

   type FB_Effect is (FB_Default, FB_Gold_Flash, FB_Red_Flash, FB_Fadein,
                      FB_Fadeout, FB_Screw, FB_Grey, FB_White_Flash,
                      FB_Green_Flash);

   Num_Shader_Effects   : constant Integer := 9;
   Effect_Elapsed       : Float := 0.0;
   Ww_Fb_Effect_Elapsed : Float := 0.0;
   Screw_Factor         : Float := 0.0;
   Curr_Ssaa            : Single := 1.0;

   FB_VAO               : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   FB_Texture           : GL.Objects.Textures.Texture;
   Special_FB           : GL.Objects.Framebuffers.Framebuffer;  --  g_fb
   Current_Effect       : FB_Effect := FB_Default;
   FB_Shader_Programs   : array (1 .. Num_Shader_Effects) of
     GL.Objects.Programs.Program;

   procedure Init (Width, Height : Integer) is
      use GL.Attributes;
      use GL.Buffers;
      use GL.Objects.Framebuffers;
      use GL.Objects.Renderbuffers;
      use GL.Objects.Textures.Targets;
      use Shader_Attributes;
      Points       : constant Singles.Vector2_Array (1 .. 6) :=
                       ((-1.0, -1.0),
                        ( 1.0,  1.0),
                        (-1.0,  1.0),
                        (-1.0, -1.0),
                        ( 1.0, -1.0),
                        ( 1.0,  1.0));

      Draw_Buffers : Explicit_Color_Buffer_List (1 .. 1);
      VBO          : GL.Objects.Buffers.Buffer;
      FB_Width     : constant Int := Int (Curr_Ssaa * Single (Width));
      FB_Height    : constant Int := Int (Curr_Ssaa * Single (Height));
      RB           : Renderbuffer;
   begin
      Game_Utils.Game_Log ("---INIT FRAMEBUFFER---");
      Draw_Buffers (1) := Color_Attachment0;
      Curr_Ssaa := Settings.Super_Sample_Anti_Aliasing;

      Special_FB.Initialize_Id;
      Read_And_Draw_Target.Bind (Special_FB);

      RB.Initialize_Id;
      Active_Renderbuffer.Bind (RB);
      Active_Renderbuffer.Allocate (GL.Pixels.Depth_Component,
                                   FB_Width, FB_Height);
      Read_And_Draw_Target.Attach_Renderbuffer (Depth_Attachment, RB);

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
                                     FB_Width, FB_Height);
      Set_Active_Buffers (Draw_Buffers);
      if not GL_Utils.Verify_Bound_Framebuffer then
         raise FB_Effects_Exception with "FB_Effects.Init Incomplete frambuffer";
      end if;

      VBO := GL_Utils.Create_2D_VBO (Points);
      FB_VAO.Initialize_Id;
      GL_Utils.Bind_VAO (FB_VAO);
      Enable_Vertex_Attrib_Array (Attrib_VP);
      Set_Vertex_Attrib_Pointer (Attrib_VP, 2, Single_Type, False, 0, 0);

      FB_Default_Shader_Manager.Init (FB_Shader_Programs (1));
      FB_Gold_Shader_Manager.Init (FB_Shader_Programs (2));
      FB_Red_Shader_Manager.Init (FB_Shader_Programs (3));
      FB_Fadein_Shader_Manager.Init (FB_Shader_Programs (4));

      FB_Fadeout_Shader_Manager.Init (FB_Shader_Programs (5));
      FB_Screw_Shader_Manager.Init (FB_Shader_Programs (6));
      FB_Grey_Shader_Manager.Init (FB_Shader_Programs (7));
      FB_White_Shader_Manager.Init (FB_Shader_Programs (8));
      FB_Green_Shader_Manager.Init (FB_Shader_Programs (9));

      Game_Utils.Game_Log ("---FRAMEBUFFER INITIALIZED---");

   exception
      when others =>
         Put_Line ("An exception occurred in FB_Effects.Init.");
         raise;
   end Init;

   --  -------------------------------------------------------------------------

   procedure Fade_In is
   begin
      Current_Effect := FB_Fadein;
      Effect_Elapsed := 0.0;
   end Fade_In;

   --  -------------------------------------------------------------------------

   procedure Fade_Out is
   begin
      if Current_Effect /= FB_Fadeout then
         Current_Effect := FB_Fadeout;
         Effect_Elapsed := 0.0;
      end if;
   end Fade_Out;

   --  -------------------------------------------------------------------------

end FB_Effects;
