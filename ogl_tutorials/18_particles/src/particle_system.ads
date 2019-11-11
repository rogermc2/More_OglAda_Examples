
with GL.Types; use GL.Types;

package Particle_System  is

   function Get_Colour_Data return Singles.Vector4_Array;
   function Get_Position_Size_Data return Singles.Vector4_Array;
   procedure Init;
   procedure Render_Particles;
   procedure Set_Texture_ID (Tex : Singles.Vector3);
   procedure Set_IDs (VP : Singles.Matrix4);
   procedure Update_Particles;

end Particle_System;
