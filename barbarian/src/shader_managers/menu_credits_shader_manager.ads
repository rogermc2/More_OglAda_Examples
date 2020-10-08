
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package Menu_Credits_Shader_Manager is

    type Shader_Uniforms is record
        Position_ID  : GL.Uniforms.Uniform := 0;
        Scale_ID     : GL.Uniforms.Uniform := 0;
        Texture_ID   : GL.Uniforms.Uniform := 0;
    end record;

    procedure Init (Shader_Program : in out GL.Objects.Programs.Program);
    procedure Set_Position (Position : Singles.Vector2);
    procedure Set_Scale (Scale : Singles.Vector2);
    procedure Set_Texture (Tex : Int);

end Menu_Credits_Shader_Manager;
