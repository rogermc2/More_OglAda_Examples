
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Types; use GL.Types;

with Ogldev_Texture;

with Billboard_Technique;
with PS_Update_Technique;

package Particle_System is

   type Particle_System is private;

   function Get_Billboard_Technique (PS : Particle_System) return
     Billboard_Technique.Technique;
   function Get_Update_Technique (PS : Particle_System) return
     PS_Update_Technique.Update_Technique;
   procedure Init_Particle_System (PS : in out Particle_System;
                                   Pos : Singles.Vector3);
   procedure Render (PS : in out Particle_System; Delta_Time : GL.Types.Int;
                     View_Point : Singles.Matrix4;
                     Camera_Pos : Singles.Vector3);
private
   type Particle_Buffer_Array is array (UInt range 1 .. 2) of GL.Objects.Buffers.Buffer;
   type Transform_Feedback_Array is array (UInt range 1 .. 2) of GL.Objects.Buffers.Transform_Buffer;

   type Particle_System is record
     Current_VB_Index  : UInt := 1;
     Current_TFB_Index : UInt := 2;
     Is_First          : Boolean := True;
     PS_Time           : GL.Types.Int := 0;
     Texture           : Ogldev_Texture.Ogl_Texture;
     Feedback_Buffer   : Transform_Feedback_Array;
     Particle_Buffer   : Particle_Buffer_Array;
     Update_Method     : PS_Update_Technique.Update_Technique;
     Billboard_Method  : Billboard_Technique.Technique;
     Random_Texture    : GL.Objects.Textures.Texture;
   end record;

end Particle_System;
