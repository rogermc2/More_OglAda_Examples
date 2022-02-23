
with GL.Buffers;

package Buffers_Manager is

   subtype Buffer_List is GL.Buffers.Explicit_Color_Buffer_List;

   procedure Bind_Cube_VAO;
   procedure Setup_Buffers;

end Buffers_Manager;
