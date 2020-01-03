
with GL.Objects.Programs;
with GL.Types; use GL.Types;
with GL.Uniforms;

package Shader_Manager is
       type Light_Uniform_IDs is record
        MVP_Matrix_ID  : GL.Uniforms.Uniform := 0;
    end record;

    type Shader_Uniforms is record
        Light_Ambient_ID      : GL.Uniforms.Uniform := 0;
        Light_Diffuse_ID      : GL.Uniforms.Uniform := 0;
        Light_Specular_ID     : GL.Uniforms.Uniform := 0;
        Light_Position_ID     : GL.Uniforms.Uniform := 0;
        Material_Ambient_ID   : GL.Uniforms.Uniform := 0;
        Material_Diffuse_ID   : GL.Uniforms.Uniform := 0;
        Material_Specular_ID  : GL.Uniforms.Uniform := 0;
        Material_Shininess_ID : GL.Uniforms.Uniform := 0;
        Map0_ID               : GL.Uniforms.Uniform := 0;
        Texture_Used_ID       : GL.Uniforms.Uniform := 0;
        Matrix_Normal_ID      : GL.Uniforms.Uniform := 0;
        Matrix_Model_View_ID  : GL.Uniforms.Uniform := 0;
        Matrix_Model_View_Projection_ID  : GL.Uniforms.Uniform := 0;
    end record;

    procedure Init (Render_Program : in out GL.Objects.Programs.Program);
    procedure Set_Light_Ambient (Ambient_Colour : Singles.Vector4);
    procedure Set_Light_Diffuse (Diffuse_Colour : Singles.Vector4);
    procedure Set_Light_Specular (Specular_Colour : Singles.Vector4);
    procedure Set_Light_Position_Vector (Light_Position : Singles.Vector3);
    procedure Set_Material_Ambient (Ambient_Colour : Singles.Vector4);
    procedure Set_Material_Diffuse (Diffuse_Colour : Singles.Vector4);
    procedure Set_Material_Specular (Specular_Colour : Singles.Vector4);
    procedure Set_Material_Shininess (Shininess : Single);
--      procedure Set_Map0 (Map0 : UInt);
    procedure Set_Matrix_Normal (Normal_Matrix : Singles.Matrix4);
    procedure Set_Matrix_Model_View (Model_View_Matrix : Singles.Matrix4);
    procedure Set_Matrix_Model_View_Projection (MVP_Matrix : Singles.Matrix4);
    procedure Set_Texture_Used (Used : Boolean);

end Shader_Manager;
