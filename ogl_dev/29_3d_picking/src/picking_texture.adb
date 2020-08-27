
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Framebuffers;
with GL.Objects.Textures.Targets;
with GL.Pixels;

with Ogldev_Engine_Common;

package body Picking_Texture is

   FBO           :  GL.Objects.Framebuffers.Framebuffer;
   Depth_Texture : GL.Objects.Textures.Texture;
   Pick_Texture  : GL.Objects.Textures.Texture;

   --  ------------------------------------------------------------------------

  procedure  Bind (aTexture : in out GL.Objects.Textures.Texture;
                    Texture_Unit : GL.Objects.Textures.Texture_Unit) is
   use GL.Objects.Textures;
   begin
      Set_Active_Unit (Texture_Unit);
      Targets.Texture_1D.Bind (aTexture);
   end Bind;

   --  ------------------------------------------------------------------------

   procedure Disable_Writing is
   begin
      null;
   end Disable_Writing;

   --  ------------------------------------------------------------------------

   procedure Enable_Writing is
      use GL.Objects.Framebuffers;
   begin
      null;
   end Enable_Writing;

   --  ------------------------------------------------------------------------

   procedure Init_Picking_Texture (Window_Width, Window_Height : GL.Types.UInt) is
      use GL.Objects.Textures.Targets;
      use GL.Types;
   begin
      --  GL_TEXTURE_1D reads data as a sequence of signed or unsigned bytes,
      --  shorts, longs or single-precision floating-point values,
      --  depending on type.
      --  These values are grouped into sets of one, two, three or four values,
      --  depending on format, to form elements.

      GL.Objects.Textures.Set_Active_Unit (Ogldev_Engine_Common.Random_Texture_Unit);
      aTexture.Initialize_Id;
      Texture_1D.Bind (aTexture);
      Texture_1D.Load_From_Data
        (Level           => 0,
         Internal_Format => GL.Pixels.RGB,
         Width           => Int (Texture_Length),
         Source_Format   => GL.Pixels.RGB,
         Source_Type     => GL.Pixels.Float,
         Source          => GL.Objects.Textures.Image_Source (Random_Data'Address));

      Texture_1D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_1D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_1D.Set_X_Wrapping (GL.Objects.Textures.Repeat); --  Wrap_S

   exception
      when  others =>
         Put_Line ("An exception occurred in Picking_Texture.Init_Picking_Texture.");
         raise;
   end Init_Picking_Texture;

   --  ------------------------------------------------------------------------

end Picking_Texture;
