
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package FB_Default_Shader_Manager is

    type Shader_Uniforms is record
        Tex_ID  : GL.Uniforms.Uniform := 0;
    end record;

    procedure Init (Shader_Program : in out GL.Objects.Programs.Program);
    procedure Set_Tex (Tex : Int);

end FB_Default_Shader_Manager;
