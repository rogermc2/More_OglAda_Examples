
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package Cursor_Shader_Manager is

    type Shader_Uniforms is record
        Model_Matrix_ID       : GL.Uniforms.Uniform := 0;
        Perspective_Matrix_ID : GL.Uniforms.Uniform := 0;
        View_Matrix_ID        : GL.Uniforms.Uniform := 0;
        Diff_Map_ID           : GL.Uniforms.Uniform := 0;
    end record;

    procedure Init (Shader_Program : in out GL.Objects.Programs.Program);
    procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4);
    procedure Set_Perspective_Matrix (Perspective_Matrix : Singles.Matrix4);
    procedure Set_View_Matrix (View_Matrix : Singles.Matrix4);
    procedure Set_Diff_Map (Diff_Map : UInt);

end Cursor_Shader_Manager;
