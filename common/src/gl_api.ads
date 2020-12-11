
--  with GL.Objects.Framebuffers;
--  with GL.Objects.Textures;
--  with GL.Low_Level.Enums;
with GL.Types; use GL.Types;

package GL_API is

   procedure Set_Scissor_Rectangle (X, Y : Int; Width, Height : Size);
   pragma Import (C, Set_Scissor_Rectangle, "glScissor");

--     procedure Attach_Texture (Target : GL.Objects.Framebuffers.Framebuffer_Target;
--                               Attachment : GL.Objects.Framebuffers.Attachment_Point;
--                               Tex_Target : GL.Low_Level.Enums.Texture_Kind;
--                               Object : GL.Objects.Textures.Texture'Class;
--                               Level  : GL.Objects.Textures.Mipmap_Level);

end GL_API;
