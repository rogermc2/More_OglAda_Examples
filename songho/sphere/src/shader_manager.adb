
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;

with Program_Loader;

package body Shader_Manager is

    Render_Uniforms : Shader_Uniforms;

    procedure Init (Render_Program  : in out GL.Objects.Programs.Program) is
        use GL.Objects.Programs;
        use GL.Objects.Shaders;
        use GL.Types.Singles;
        use Program_Loader;
        Light_Position    : constant Singles.Vector4 := (0.0, 0.0, -3.0, 0.0);
        Ambient           : constant Singles.Vector4 := (0.3, 0.3, 0.3, 1.0);
        Diffuse           : constant Singles.Vector4 := (0.7, 0.7, 0.7, 1.0);
        Specula           : constant Singles.Vector4 := (1.0, 1.0, 1.0, 1.0);
        Material_Ambient  : constant Singles.Vector4 := (0.5, 0.5, 0.5, 1.0);
        Material_Diffuse  : constant Singles.Vector4 := (0.7, 0.7, 0.7, 1.0);
        Material_Specular : constant Singles.Vector4 := (0.4, 0.4, 0.4, 1.0);
        Shininess         : constant Single := 16.0;
    begin
        Render_Program := Program_From
          ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));

        Use_Program (Render_Program);
        Render_Uniforms.Matrix_Model_View_ID :=
          Uniform_Location (Render_Program, "matrixModelView");
        Render_Uniforms.Matrix_Model_View_Projection_ID :=
          Uniform_Location (Render_Program, "matrixModelViewProjection");
        Render_Uniforms.Matrix_Normal_ID :=
          Uniform_Location (Render_Program, "matrixNormal");
        Render_Uniforms.Light_Position_ID :=
          Uniform_Location (Render_Program, "lightPosition");

        Render_Uniforms.Light_Ambient_ID :=
          Uniform_Location (Render_Program, "lightAmbient");
        Render_Uniforms.Light_Diffuse_ID :=
          Uniform_Location (Render_Program, "lightDiffuse");
        Render_Uniforms.Light_Specular_ID :=
          Uniform_Location (Render_Program, "lightSpecular");
        Render_Uniforms.Material_Ambient_ID :=
          Uniform_Location (Render_Program, "materialAmbient");
        Render_Uniforms.Material_Diffuse_ID :=
          Uniform_Location (Render_Program, "materialDiffuse");
        Render_Uniforms.Material_Specular_ID :=
          Uniform_Location (Render_Program, "materialSpecular");
        Render_Uniforms.Material_Shininess_ID :=
          Uniform_Location (Render_Program, "materialShininess");

        Render_Uniforms.Map0_ID :=
          Uniform_Location (Render_Program, "map0");
        Render_Uniforms.Texture_Used_ID :=
          Uniform_Location (Render_Program, "textureUsed");

        GL.Uniforms.Set_Single (Render_Uniforms.Light_Position_ID, Light_Position);
        GL.Uniforms.Set_Single (Render_Uniforms.Matrix_Normal_ID, Identity4);
        GL.Uniforms.Set_Single (Render_Uniforms.Matrix_Model_View_ID, Identity4);
        GL.Uniforms.Set_Single (Render_Uniforms.Matrix_Model_View_Projection_ID, Identity4);

        GL.Uniforms.Set_Single (Render_Uniforms.Light_Ambient_ID, Ambient);
        GL.Uniforms.Set_Single (Render_Uniforms.Light_Diffuse_ID, Diffuse);
        GL.Uniforms.Set_Single (Render_Uniforms.Light_Specular_ID, Specula);

        GL.Uniforms.Set_Single (Render_Uniforms.Material_Ambient_ID, Material_Ambient);
        GL.Uniforms.Set_Single (Render_Uniforms.Material_Diffuse_ID, Material_Diffuse);
        GL.Uniforms.Set_Single (Render_Uniforms.Material_Specular_ID, Material_Specular);
        GL.Uniforms.Set_Single (Render_Uniforms.Material_Shininess_ID, Shininess);

        GL.Uniforms.Set_Int (Render_Uniforms.Map0_ID, 0);
        GL.Uniforms.Set_Int (Render_Uniforms.Texture_Used_ID, 0);

    exception
        when others =>
            Put_Line ("An exception occurred in Shader_Manager.Init.");
            raise;
    end Init;

    --  -------------------------------------------------------------------------

    procedure Set_Light_Ambient (Ambient_Colour : Singles.Vector4) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Light_Ambient_ID, Ambient_Colour);
    end Set_Light_Ambient;

    --  -------------------------------------------------------------------------

    procedure Set_Light_Diffuse (Diffuse_Colour : Singles.Vector4) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Light_Diffuse_ID, Diffuse_Colour);
    end Set_Light_Diffuse;

    --  -------------------------------------------------------------------------

    procedure Set_Light_Specular (Specular_Colour : Singles.Vector4) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Light_Specular_ID, Specular_Colour);
    end Set_Light_Specular;

    --  -------------------------------------------------------------------------

    procedure Set_Light_Position_Vector (Light_Position : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Light_Position_ID, Light_Position);
    end Set_Light_Position_Vector;

    --  -------------------------------------------------------------------------

    procedure Set_Map0 (Map0 : Int)  is
    begin
        GL.Uniforms.Set_Int (Render_Uniforms.Map0_ID, Map0);
    end Set_Map0;

    --  -------------------------------------------------------------------------
    procedure Set_Material_Ambient (Ambient_Colour : Singles.Vector4) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Material_Ambient_ID, Ambient_Colour);
    end Set_Material_Ambient;

    --  -------------------------------------------------------------------------

    procedure Set_Material_Diffuse (Diffuse_Colour : Singles.Vector4) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Material_Diffuse_ID, Diffuse_Colour);
    end Set_Material_Diffuse;

    --  -------------------------------------------------------------------------

    procedure Set_Material_Specular (Specular_Colour : Singles.Vector4) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Material_Specular_ID, Specular_Colour);
    end Set_Material_Specular;

    --  -------------------------------------------------------------------------

    procedure Set_Material_Shininess (Shininess : Single) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Material_Shininess_ID, Shininess);
    end Set_Material_Shininess;

    --  -------------------------------------------------------------------------

    procedure Set_Matrix_Normal (Normal_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Matrix_Normal_ID, Normal_Matrix);
    end Set_Matrix_Normal;

    --  -------------------------------------------------------------------------

    procedure Set_Matrix_Model_View (Model_View_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Matrix_Model_View_ID, Model_View_Matrix);
    end Set_Matrix_Model_View;

    --  -------------------------------------------------------------------------

    procedure Set_Matrix_Model_View_Projection (MVP_Matrix  : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Matrix_Model_View_Projection_ID, MVP_Matrix);
    end Set_Matrix_Model_View_Projection;

    --  -------------------------------------------------------------------------

    procedure Set_Texture_Used (Used : Boolean) is
    begin
        if Used then
            GL.Uniforms.Set_Int (Render_Uniforms.Texture_Used_ID, 1);
        else
            GL.Uniforms.Set_Int (Render_Uniforms.Texture_Used_ID, 0);
        end if;
    end Set_Texture_Used;

    --  -------------------------------------------------------------------------

end Shader_Manager;
