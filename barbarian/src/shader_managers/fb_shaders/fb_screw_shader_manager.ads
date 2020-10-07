
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package FB_Screw_Shader_Manager is

    type Shader_Uniforms is record
        Force_ID : GL.Uniforms.Uniform := 0;
        Tex_ID   : GL.Uniforms.Uniform := 0;
        Time_ID  : GL.Uniforms.Uniform := 0;
    end record;

    procedure Init (Shader_Program : in out GL.Objects.Programs.Program);
    procedure Set_Force (F : Single);
    procedure Set_Tex (Tex : Int);
    procedure Set_Time (T : Single);

end FB_Screw_Shader_Manager;
