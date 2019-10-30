
with GL.Objects.Buffers;
with GL.Types;

package Buffers is

   type Buffer_Array is array (Integer range <>) of GL.Objects.Buffers.Buffer;

   --  Buffer types
   Position_A  : constant Integer := 1;
   Position_B  : constant Integer := 2;
   Velocity_A  : constant Integer := 3;
   --     Velocity_B           : constant Integer := 4;
   Connection  : constant Integer := 5;

   procedure Setup_Buffers (VBO_Array           : in out Buffer_Array;
                            Index_Buffer        : in out GL.Objects.Buffers.Buffer;
                            Position_Tex_Buffers : in out Buffer_Array);
   function Total_Connections return GL.Types.Int;
   function Total_Points return GL.Types.Int;

   --  -----------------------------------------------------------------------------------------------------------------------


end Buffers;
