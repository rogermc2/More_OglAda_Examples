
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Textures;

package My_Buffers is

    subtype tFrame_Buffer is GL.Objects.Framebuffers.Framebuffer;

    procedure Setup_Buffers (Frame_Buffer     : in out GL.Objects.Framebuffers.Framebuffer;
                             Position_Buffer  : in out  GL.Objects.Buffers.Buffer;
                             Index_Buffer     : in out  GL.Objects.Buffers.Buffer);
    procedure Setup_Textures (Frame_Buffer      : in out GL.Objects.Framebuffers.Framebuffer;
                              Colour_Texture    : in out GL.Objects.Textures.Texture;
                              Draw_Buffer_List  : in out GL.Buffers.Explicit_Color_Buffer_List);
end My_Buffers;
