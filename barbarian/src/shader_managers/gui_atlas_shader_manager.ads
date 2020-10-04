
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package GUI_Atlas_Shader_Manager is

    type Shader_Uniforms is record
        Alpha_ID          : GL.Uniforms.Uniform := 0;
        Atlas_ID          : GL.Uniforms.Uniform := 0;
        Columns_ID        : GL.Uniforms.Uniform := 0;
        Current_Sprite_ID : GL.Uniforms.Uniform := 0;
        Model_Matrix_ID   : GL.Uniforms.Uniform := 0;
    end record;

    procedure Init (Shader_Program : in out GL.Objects.Programs.Program);
    procedure Set_Alpha (Alpha : Single);
    procedure Set_Atlas (Atlas : Int);
    procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4);
    procedure Set_Columns (Columns : Single);
    procedure Set_Current_Sprite (Current_Sprite : Single);

end GUI_Atlas_Shader_Manager;
