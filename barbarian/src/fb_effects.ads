
with GL.Types;

package FB_Effects is

   type FB_Effect is (FB_Default_Effect, FB_Gold_Flash_Effect,
                      FB_Red_Flash_Effect, FB_Fadein_Effect,
                      FB_Fadeout_Effect, FB_Screw_Effect, FB_Grey_Effect,
                      FB_White_Flash_Effect, FB_Green_Flash_Effect);
   pragma Ordered (FB_Effect);

   FB_Effects_Exception : Exception;

   procedure Bind_Main_Scene_FB;
   function Current_SSAA return GL.Types.Single;
   procedure Draw_FB_Effects (Delta_Time : GL.Types.Single);
   procedure Init (Width, Height : Integer);
   procedure Fade_In;
   procedure Fade_Out;
   procedure FB_Gold_Flash;
   procedure FB_Green_Flash;
   procedure FB_White_Flash;
   procedure Set_Feedback_Effect (Effect : FB_Effect);
   procedure Set_Feedback_Screw (Factor : Float);
   procedure Set_WW_FB_Effect (Effect : FB_Effect);

end FB_Effects;
