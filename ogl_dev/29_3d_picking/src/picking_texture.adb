
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Framebuffers;
with GL.Objects.Textures.Targets;
with GL.Pixels;

package body Picking_Texture is

   --  ------------------------------------------------------------------------

   procedure Disable_Writing is
      use GL.Objects.Framebuffers;
   begin
      Draw_Target.Bind (Default_Framebuffer);
   end Disable_Writing;

   --  ------------------------------------------------------------------------

   procedure Enable_Writing (aTexture : in out Pick_Texture) is
      use GL.Objects.Framebuffers;
   begin
      Draw_Target.Bind (aTexture.FBO);
   end Enable_Writing;

   --  ------------------------------------------------------------------------

   procedure Init (theTexture : in out Pick_Texture;
                   Window_Width, Window_Height : GL.Types.Int) is

      use GL.Objects.Framebuffers;
      use GL.Objects.Textures.Targets;
      use GL.Types;
      FB_Status : Framebuffer_Status := Undefined;
   begin
      theTexture.FBO.Initialize_Id;
      Read_And_Draw_Target.Bind (theTexture.FBO);

      theTexture.Picking_Texture.Initialize_Id;
      Texture_2D.Bind (theTexture.Picking_Texture);
      Texture_2D.Load_Empty_Texture  (Level           => 0,
                                      Internal_Format => GL.Pixels.RGB32F,
                                      Width           => Int (Window_Width),
                                      Height          => Int (Window_Width));

      Read_And_Draw_Target.Attach_Texture (Color_Attachment_0, theTexture.Picking_Texture, 0);

      theTexture.Depth_Texture.Initialize_Id;
      Texture_2D.Bind (theTexture.Depth_Texture);
      Texture_2D.Load_Empty_Texture  (Level           => 0,
                                      Internal_Format => GL.Pixels.Depth_Component,
                                      Width           => Int (Window_Width),
                                      Height          => Int (Window_Width));
      Read_And_Draw_Target.Attach_Texture (Depth_Attachment, theTexture.Depth_Texture, 0);

      --  Reset Read_Target to screen framebffer
      Draw_Target.Bind (Default_Framebuffer);
      Read_Target.Bind (Default_Framebuffer);
      --        Draw_Target.Bind (Color_Attachment_0);

      FB_Status := GL.Objects.Framebuffers.Status (Read_And_Draw_Target);
      if  FB_Status /= Complete then
         Put_Line ("Picking_Texture.Init_Picking_Texture, Frame buffer error : "
                   & Framebuffer_Status'Image (FB_Status));
      end if;

   exception
      when  others =>
         Put_Line ("An exception occurred in Picking_Texture.Init_Picking_Texture.");
         raise;
   end Init;

   --  ------------------------------------------------------------------------

end Picking_Texture;
