
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package Health_Shader_Manager is

    type Shader_Uniforms is record
        Base_Texture_ID  : GL.Uniforms.Uniform := 0;
        Health_Factor_ID : GL.Uniforms.Uniform := 0;
        Model_Matrix_ID  : GL.Uniforms.Uniform := 0;
        Red_Texture_ID   : GL.Uniforms.Uniform := 0;
    end record;

    procedure Init (Shader_Program : in out GL.Objects.Programs.Program);
    procedure Set_Base_Texture (Texture : Int);
    procedure Set_Health_Factor (Health_Factor : Single);
    procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4);
    procedure Set_Red_Texture (Texture : Int);

end Health_Shader_Manager;
