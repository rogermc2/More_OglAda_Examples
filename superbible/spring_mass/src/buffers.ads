
with GL.Objects.Buffers;

package Buffers is

    type Buffer_Array is array (Integer range <>) of GL.Objects.Buffers.Buffer;

    procedure Setup_Buffers (VBO : in out Buffer_Array;
                             Index_Buffer : in out GL.Objects.Buffers.Buffer;
                             Position_Tex_Buffer : in out Buffer_Array);
end Buffers;
