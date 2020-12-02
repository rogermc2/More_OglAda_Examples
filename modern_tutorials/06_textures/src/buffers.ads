
with GL.Objects.Buffers;

package Buffers is

   procedure Create_Elements_Buffer (IBO   : in out GL.Objects.Buffers.Buffer);
   procedure Create_Tex_Coords_Buffer (VBO : in out GL.Objects.Buffers.Buffer);
   procedure Create_Vertex_Buffer (VBO     : in out GL.Objects.Buffers.Buffer);

end Buffers;
