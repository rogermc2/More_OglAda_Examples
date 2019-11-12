
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Types; use GL.Types;

with Ogldev_Texture;

with Billboard_Technique;
with PS_Update_Technique;

package Particle_System is

   type Particle_System is private;

   procedure Init_Particle_System (PS : in out Particle_System;
                                   Pos : Singles.Vector3);
   procedure Render (PS : in out Particle_System; Delta_Time : GL.Types.UInt;
                     View_Point : Singles.Matrix4;
                     Camera_Pos : Singles.Vector3);
private
   type Buffer_Index is new UInt range 1 .. 2;
   type Particle_Buffer_Array is array (Buffer_Index'Range) of
     GL.Objects.Buffers.Buffer;
   type Transform_Feedback_Array is array (Buffer_Index'Range) of
     GL.Objects.Buffers.Transform_Buffer;

   type Particle_System is record
     Current_VB_Index  : Buffer_Index := 1;
     Is_First          : Boolean := True;
     PS_Time           : GL.Types.UInt := 0;
     Texture           : Ogldev_Texture.Ogl_Texture;
     Feedback_Buffer   : Transform_Feedback_Array;
     Particle_Buffer   : Particle_Buffer_Array;
     Update_Method     : PS_Update_Technique.Update_Technique;
     Display_Method    : Billboard_Technique.Technique;
     Random_Texture    : GL.Objects.Textures.Texture;
   end record;

end Particle_System;
