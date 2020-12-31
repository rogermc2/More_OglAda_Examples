
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package Water_Shader_Manager is

    type Shader_Uniforms is record
        Ambient_Light_ID        : GL.Uniforms.Uniform := 0;
        Animation_Time_ID       : GL.Uniforms.Uniform := 0;
        Caster_Position_ID      : GL.Uniforms.Uniform := 0;
        Cube_Texture_ID         : GL.Uniforms.Uniform := 0;
        Diff_Map_ID             : GL.Uniforms.Uniform := 0;
        Dynamic_Light_Pos_ID    : GL.Uniforms.Uniform := 0;
        Dynamic_Light_Diff_ID   : GL.Uniforms.Uniform := 0;
        Dynamic_Light_Spec_ID   : GL.Uniforms.Uniform := 0;
        Dynamic_Light_Range_ID  : GL.Uniforms.Uniform := 0;
        K_Diff_ID               : GL.Uniforms.Uniform := 0;
        K_Spec_ID               : GL.Uniforms.Uniform := 0;
        Light_Diffuse_ID        : GL.Uniforms.Uniform := 0;
        Light_Position_ID       : GL.Uniforms.Uniform := 0;
        Light_Range_ID          : GL.Uniforms.Uniform := 0;
        Light_Specular_ID       : GL.Uniforms.Uniform := 0;
        Model_Matrix_ID         : GL.Uniforms.Uniform := 0;
        Projection_Matrix_ID    : GL.Uniforms.Uniform := 0;
        Shadow_Enabled_ID       : GL.Uniforms.Uniform := 0;
        Spec_Map_ID             : GL.Uniforms.Uniform := 0;
        Static_Light_Indices_ID : GL.Uniforms.Uniform := 0;
        View_Matrix_ID          : GL.Uniforms.Uniform := 0;

    end record;

    procedure Init (Shader_Program : in out GL.Objects.Programs.Program);
    procedure Set_Ambient_Light (Level : Singles.Vector3);
    procedure Set_Animation_Time (theTime : Single);
    procedure Set_Caster_Position (Position : Singles.Vector3);
    procedure Set_Cube_Texture (Texture : Int);
    procedure Set_Diff_Map (Map : Int);
    procedure Set_Dynamic_Light_Pos (Position : Singles.Vector3);
    procedure Set_Dynamic_Light_Diff (Diff : Singles.Vector3);
    procedure Set_Dynamic_Light_Spec (Spec : Singles.Vector3);
    procedure Set_Dynamic_Light_Range (Light_Range : Single);
    procedure Set_K_Diff (K_Diff : Singles.Vector4);
    procedure Set_K_Spec (K_Spec : Singles.Vector4);
    procedure Set_Light_Diffuse (Diffuse : Singles.Vector3_Array);
    procedure Set_Light_Position (Position : Singles.Vector3_Array);
    procedure Set_Light_Range (Light_Range : Single_Array);
    procedure Set_Light_Specular (Specular : Singles.Vector3_Array);
    procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4);
    procedure Set_Projection_Matrix (Projection_Matrix : Singles.Matrix4);
    procedure Set_Shadow_Enabled (Shadow : Single);
    procedure Set_Spec_Map (Map : Int);
    procedure Set_Static_Light_Indices (Indices : Ints.Vector2);
    procedure Set_View_Matrix (View_Matrix : Singles.Matrix4);

end Water_Shader_Manager;
