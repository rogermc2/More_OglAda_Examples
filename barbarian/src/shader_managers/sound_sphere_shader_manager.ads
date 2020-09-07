
with GL.Objects.Programs;
with GL.Types; use GL.Types;

package Sound_Sphere_Shader_Manager is

    procedure Init_Sound_Sphere_Shader
      (Sound_Sphere_Shader : out GL.Objects.Programs.Program);
    procedure Set_PVM (PVM : Singles.Matrix4);

end Sound_Sphere_Shader_Manager;
