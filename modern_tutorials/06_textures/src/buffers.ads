
with GL.Objects.Buffers;
with GL.Types;

with Cube_Data;

package Buffers is

   procedure Create_Elements_Buffer (IBO     : in out GL.Objects.Buffers.Buffer;
                                     Indices : GL.Types.Int_Array);
   procedure Create_Tex_Coords_Buffer (VBO      : in out GL.Objects.Buffers.Buffer;
                                       Coords   : Cube_Data.Tex_Coords);
   procedure Create_Vertex_Buffer (VBO      : in out GL.Objects.Buffers.Buffer;
                                   Vertices : Cube_Data.Vertices_Data);
end Buffers;
