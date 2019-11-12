
with GL.Types; use GL.Types;

package Particle_System  is

   procedure Init;
   procedure Render_Particles;
   procedure Set_IDs (VP : Singles.Matrix4);
   procedure Update_Particles;

end Particle_System;
