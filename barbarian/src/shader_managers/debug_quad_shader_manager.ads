
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package Debug_Quad_Shader_Manager is

    type Shader_Uniforms is record
        Texture_ID          : GL.Uniforms.Uniform := 0;
    end record;

    procedure Init (Shader_Program : in out GL.Objects.Programs.Program);
    procedure Set_Texture (Tex : Int);

end Debug_Quad_Shader_Manager;
