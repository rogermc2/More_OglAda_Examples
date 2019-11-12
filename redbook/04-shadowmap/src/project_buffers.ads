
with GL.Objects.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Textures;

package Project_Buffers is
    procedure Init_Ground_Buffer (Ground_Buffer : in out GL.Objects.Buffers.Buffer);
    procedure Init_Depth_Texture (Depth_Texture : in out GL.Objects.Textures.Texture);
    procedure Init_Depth_Frame_Buffer (Depth_Frame_Buffer : in out GL.Objects.Framebuffers.Framebuffer;
                                       Depth_Texture      : GL.Objects.Textures.Texture);
end Project_Buffers;
