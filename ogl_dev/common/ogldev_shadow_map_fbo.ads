
with GL.Objects.Framebuffers;
with GL.Objects.Textures;
with GL.Types;

package Ogldev_Shadow_Map_FBO is

   type Shadow_Map_FBO is private;

   Shadow_Map_Exception : Exception;

   procedure Bind_For_Reading (aShadow_Map : in out Shadow_Map_FBO;
                               Tex_Unit : GL.Objects.Textures.Texture_Unit);
   procedure Bind_For_Writing (aShadow_Map : in out Shadow_Map_FBO);
   procedure Init (aShadow_Map : in out Shadow_Map_FBO;
                   Window_Width, Window_Height : GL.Types.Int);
--                     Draw_Buffer_List : in out GL.Buffers.Explicit_Color_Buffer_List);

private

   type Shadow_Map_FBO is record
      FBO     : GL.Objects.Framebuffers.Framebuffer;
      Texture : GL.Objects.Textures.Texture;
   end record;

end Ogldev_Shadow_Map_FBO;
