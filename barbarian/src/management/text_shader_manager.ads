
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package Text_Shader_Manager is

    type Shader_Uniforms is record
        Position_ID    : GL.Uniforms.Uniform := 0;
        Text_Colour_ID : GL.Uniforms.Uniform := 0;
        Texture_ID     : GL.Uniforms.Uniform := 0;
    end record;

    procedure Init (Shader_Program : in out GL.Objects.Programs.Program);
    procedure Set_Position_ID (Position : Singles.Vector2);
    procedure Set_Text_Colour_ID (Colour : Singles.Vector4);
    procedure Set_Texture_Unit (Texture_Unit : Int);

end Text_Shader_Manager;
