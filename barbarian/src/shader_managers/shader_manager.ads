
with Ada.Containers.Vectors;

with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package Shader_Manager is

   procedure Init;

private

   use  GL.Uniforms;
   use GL.Objects.Programs;
   package Shader_Uniforms_Package is new
     Ada.Containers.Vectors (Positive, Uniform);
   type Shader_Uniforms_List is new Shader_Uniforms_Package.Vector with null Record;

   type Shader_Data is record
      Shader_Uniforms : Shader_Uniforms_List;
      Sp              : Uniform := 0;
      Vs              : Uniform := 0;
      Fs              : Uniform := 0;
--        Uniform_Count   : Int := 0;
--        All_Compiled    : Boolean := False;
--        Linked          : Boolean := False;
   end record;

end Shader_Manager;
