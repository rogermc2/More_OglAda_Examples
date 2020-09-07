
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package Title_Shader_Manager is

    type Shader_Uniforms is record
        Model_Matrix_ID       : GL.Uniforms.Uniform := 0;
        Perspective_Matrix_ID : GL.Uniforms.Uniform := 0;
        View_Matrix_ID        : GL.Uniforms.Uniform := 0;
        Time_ID               : GL.Uniforms.Uniform := 0;
    end record;

    procedure Init (Shader_Program : in out GL.Objects.Programs.Program);
    procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4);
    procedure Set_Perspective_Matrix (Perspective_Matrix : Singles.Matrix4);
    procedure Set_View_Matrix (View_Matrix : Singles.Matrix4);
    procedure Set_Time (Time : Single);

end Title_Shader_Manager;
