
with GL.Objects.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Textures;

package Project_Buffers is
    procedure Init_Ground_Buffer (Ground_Buffer : in out GL.Objects.Buffers.Buffer);
    procedure Init_Texture (Depth_Frame_Buffer : in out GL.Objects.Framebuffers.Framebuffer;
                            theTexture  : in out GL.Objects.Textures.Texture);
end Project_Buffers;
