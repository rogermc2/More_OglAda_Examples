
with GL.Objects.Framebuffers;
with GL.Objects.Textures;
with GL.Types;

with Glfw.Windows;

package Picking_Texture is

   type Pick_Texture is private;
   type Pixel_Info is private;

   procedure Disable_Writing;
   procedure Enable_Writing (aTexture : in out Pick_Texture);
   procedure Init (theTexture : in out Pick_Texture;
                   Window_Width, Window_Height : GL.Types.Int);
   function Read_Pixel (Window   : in out Glfw.Windows.Window;
                        aTexture : in out Pick_Texture;
                        X, Y : Gl.Types.Int) return Pixel_Info;

private
   type Pick_Texture is record
      FBO             : GL.Objects.Framebuffers.Framebuffer;
      Picking_Texture : GL.Objects.Textures.Texture;
      Depth_Texture   : GL.Objects.Textures.Texture;
   end record;

   type Pixel_Info is record
      Object_ID : GL.Types.Single := 0.0;
      Draw_ID   : GL.Types.Single := 0.0;
      Prim_ID   : GL.Types.Single := 0.0;
   end record;

end Picking_Texture;
