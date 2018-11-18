
with GL.Objects.Framebuffers;
with GL.Objects.Textures;
with GL.Types;

package Shadow_Map_FBO is

   type Shadow_Map is private;

   Shadow_Map_Exception : Exception;

   procedure Bind_For_Reading (aShadow_Map : in out Shadow_Map;
                               Tex_Unit : GL.Objects.Textures.Texture_Unit);
   procedure Bind_For_Writing (Frame_Buffer : in out GL.Objects.Framebuffers.Framebuffer);
   function Init (aShadow_Map : in out Shadow_Map;
                  Window_Width, Window_Height : GL.Types.Int)
                  return Boolean;

private

   type Shadow_Map is record
      FBO   : GL.Objects.Framebuffers.Framebuffer;
      Map   : GL.Objects.Textures.Texture;
   end record;

end Shadow_Map_FBO;
