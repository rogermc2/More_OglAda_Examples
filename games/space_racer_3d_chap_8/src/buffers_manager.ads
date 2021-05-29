
with GL.Buffers;
with GL.Objects.Vertex_Arrays;

package Buffers_Manager is

    subtype Buffer_List is GL.Buffers.Explicit_Color_Buffer_List;
    Vertices_Array_Object : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

    procedure Setup_Buffers;

end Buffers_Manager;
