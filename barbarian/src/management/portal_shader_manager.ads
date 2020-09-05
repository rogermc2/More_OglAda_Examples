
with GL.Objects.Programs;
with GL.Types; use GL.Types;

package Portal_Shader_Manager is

    procedure Init_Portal_Shader
      (Portal_Shader : out GL.Objects.Programs.Program);
    procedure Set_DM (DM : UInt);
    procedure Set_Model (Model_Matrix : Singles.Matrix4);
    procedure Set_Perspective (Perspective_Matrix : Singles.Matrix4);
    procedure Set_View (View_Matrix : Singles.Matrix4);

end Portal_Shader_Manager;
