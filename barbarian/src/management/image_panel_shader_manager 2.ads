
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package Image_Panel_Shader_Manager is

    type Shader_Uniforms is record
        Model_Matrix_ID  : GL.Uniforms.Uniform := 0;
        Texture_Unit_ID  : GL.Uniforms.Uniform := 0;
    end record;

    procedure Init (Shader_Program : in out GL.Objects.Programs.Program);
    procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4);
    procedure Set_Texture_Unit (Texture_Unit : Int);

end Image_Panel_Shader_Manager;
