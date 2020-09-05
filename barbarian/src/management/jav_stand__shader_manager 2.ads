
with GL.Objects.Programs;
with GL.Types; use GL.Types;

package Jav_Stand_Shader_Manager is

    procedure Init_Jav_Stand_Shader
      (Jav_Stand_Shader : out GL.Objects.Programs.Program);
    procedure Set_Caster_Pos_World (Position : Singles.Vector3);
    procedure Set_Cube_Texture (Texture : UInt);
    procedure Set_DM (DM : UInt);
    procedure Set_Model (Model_Matrix : Singles.Matrix4);
    procedure Set_Ol_Pass (Ol_Pass : Single);
    procedure Set_Perspective (Perspective_Matrix : Singles.Matrix4);
    procedure Set_Shadow_Enabled (Enable : Single);
    procedure Set_View (View_Matrix : Singles.Matrix4);

end Jav_Stand_Shader_Manager;
