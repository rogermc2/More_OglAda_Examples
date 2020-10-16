
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package Text_Shader_Manager is

    procedure Init (Shader_Program : in out GL.Objects.Programs.Program);
    procedure Set_Position_ID (Position : Singles.Vector2);
    procedure Set_Text_Colour_ID (Colour : Singles.Vector4);
    procedure Set_Texture_Unit (Texture_Unit : Int);

end Text_Shader_Manager;
