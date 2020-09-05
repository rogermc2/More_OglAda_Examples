
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Uniforms;

with Program_Loader;

with Shader_Attributes;

package body Sound_Sphere_Shader_Manager is
    use GL.Objects.Programs;

    type Sound_Sphere_Uniform is record
        PVM_ID          : GL.Uniforms.Uniform := 0;
    end record;

    Sound_Sphere_Uniforms : Sound_Sphere_Uniform;

    --  -------------------------------------------------------------------------

    procedure Init_Sound_Sphere_Shader
      (Sound_Sphere_Shader : out GL.Objects.Programs.Program) is
        use GL.Objects.Shaders;
        use GL.Types.Singles;
        use Program_Loader;
    begin
        Sound_Sphere_Shader := Program_From
          ((Src ("src/shaders_3_2/sound_sphere.vert", Vertex_Shader),
           Src ("src/shaders_3_2/sound_sphere.frag", Fragment_Shader)));

        Bind_Attrib_Location (Sound_Sphere_Shader, Shader_Attributes.Attrib_VP, "vp");

        Sound_Sphere_Uniforms.PVM_ID := Uniform_Location (Sound_Sphere_Shader, "PVM");

        Use_Program (Sound_Sphere_Shader);
        GL.Uniforms.Set_Single (Sound_Sphere_Uniforms.PVM_ID, Identity4);

    exception
        when others =>
            Put_Line ("An exception occurred in Coins_Shader_Manager.Init_Portal_Shader.");
            raise;
    end Init_Sound_Sphere_Shader;

    --  -------------------------------------------------------------------------

    procedure Set_PVM (PVM : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Sound_Sphere_Uniforms.PVM_ID, PVM);
    end Set_PVM;

    --  -------------------------------------------------------------------------

end Sound_Sphere_Shader_Manager;
