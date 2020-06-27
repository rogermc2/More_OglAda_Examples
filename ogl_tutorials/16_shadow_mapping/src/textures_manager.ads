
with GL.Objects.Textures;
with GL.Objects.Framebuffers;

package Textures_Manager is

   procedure Init (Frame_Buffer : in out GL.Objects.Framebuffers.Framebuffer;
                   Depth_Texture : in out GL.Objects.Textures.Texture);
   procedure Load_Texture (Frame_Buffer : in out GL.Objects.Framebuffers.Framebuffer;
                           Image_File_Name : String;
                           theTexture : in out GL.Objects.Textures.Texture);

   Image_Error : Exception;
   Framebuffer_Error : Exception;

end Textures_Manager;
