
with GL.Objects.Buffers;
with GL.Types; use GL.Types;

package Particle_System  is
    Max_Particles  : constant Int := 100000;

    procedure Init (Position_Buffer, Colour_Buffer : in out GL.Objects.Buffers.Buffer);
    procedure Sort_Particles;
    procedure Update_Particles (Delta_Time : Single);

end Particle_System;
