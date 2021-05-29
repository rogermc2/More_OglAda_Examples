
with GL.Buffers;
with GL.Objects.Buffers;

package Buffers_Manager is

    subtype Buffer_List is GL.Buffers.Explicit_Color_Buffer_List;

    procedure Setup_Buffers
     (Vertex_Buffer, Colour_Buffer : in out GL.Objects.Buffers.Buffer);

end Buffers_Manager;
