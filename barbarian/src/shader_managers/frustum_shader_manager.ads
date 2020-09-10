
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package Frustum_Shader_Manager is

    type Shader_Uniforms is record
        Model_Matrix_ID : GL.Uniforms.Uniform := 0;
        Colour_ID       : GL.Uniforms.Uniform := 0;
    end record;

    procedure Init (Shader_Program : in out GL.Objects.Programs.Program);
    procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4);
    procedure Set_Colour (Colour : Singles.Vector4);

end Frustum_Shader_Manager;
