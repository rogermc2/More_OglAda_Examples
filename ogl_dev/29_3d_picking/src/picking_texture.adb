
with Ada.Text_IO; use Ada.Text_IO;

with GL.Framebuffer;
with GL.Objects.Framebuffers;
with GL.Objects.Textures.Targets;
with GL.Pixels;

package body Picking_Texture is
   use GL.Types;

   type Pixel_Array_Type is array (Int range <>) of aliased UByte;
   procedure Picking_Read_Pixels is new
     GL.Framebuffer.Read_Pixels (UByte, Int, Pixel_Array_Type);

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
   --  X, Y are the window coordinates of the first pixel that is read from
   --  the frame buffer.
   --  X, Y is the lower left corner of a rectangular block of pixels.
   --  A width and height of one correspond to a single pixel.
   function Read_Pixel (Window   : in out Glfw.Windows.Window;
                        aTexture : in out Pick_Texture;
                        X, Y : Gl.Types.Int) return Pixel_Info is
      use GL.Objects.Framebuffers;
      use GL.Pixels;
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
      Info          : Pixel_Info;
   begin
      Read_Target.Bind (aTexture.FBO);
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      declare
         Pixel_Width   : Size := 1;
         Pixel_Height  : Size := 1;
         Pixels        : Pixel_Array_Type
           (1 .. GL.Types.Int (Window_Width) * GL.Types.Int (Window_Height));
      begin
         Picking_Read_Pixels (X, Y, Pixel_Width, Pixel_Height,
                              RGB, GL.Pixels.Float, Pixels);
      end;  --  declare block
      return Info;
   exception
      when  others =>
         Put_Line ("An exception occurred in Picking_Texture.Read_Pixel.");
         raise;
   end Read_Pixel;

   --  ------------------------------------------------------------------------

end Picking_Texture;
