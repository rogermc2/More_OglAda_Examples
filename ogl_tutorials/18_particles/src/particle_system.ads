
with GL.Objects.Buffers;
with GL.Types; use GL.Types;

package Particle_System  is

   type Colour_Data_Array is new Single_Array;
   type Position_Size_Data_Array is new Single_Array;

    Max_Particles  : constant Int := 100000;

   function Get_Colour_Data return Colour_Data_Array;
   function Get_Position_Size_Data return Position_Size_Data_Array;
   procedure Init (Position_Buffer, Colour_Buffer : in out GL.Objects.Buffers.Buffer);
   procedure Update_Particles (Delta_Time : Single);

end Particle_System;
