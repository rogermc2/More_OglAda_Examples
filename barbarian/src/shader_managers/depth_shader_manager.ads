
with GL.Types; use GL.Types;
with GL.Objects.Programs;

package Depth_Shader_Manager is

    procedure Init (Shader_Program : in out GL.Objects.Programs.Program);
    procedure Set_Light_Position (Light_Position : Singles.Vector3);
    procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4);
    procedure Set_Projection_Matrix (Projection_Matrix : Singles.Matrix4);
    procedure Set_View_Matrix (View_Matrix : Singles.Matrix4);

end Depth_Shader_Manager;
