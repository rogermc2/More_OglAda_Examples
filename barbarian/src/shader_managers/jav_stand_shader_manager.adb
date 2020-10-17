
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Uniforms;

with Program_Loader;

with Shader_Attributes;

package body Jav_Stand_Shader_Manager is
    use GL.Objects.Programs;

    type Jav_Stand_Uniform is record
        Perspective_ID          : GL.Uniforms.Uniform := 0;
        View_ID                 : GL.Uniforms.Uniform := 0;
        Model_ID                : GL.Uniforms.Uniform := 0;
        Ol_Pass_ID              : GL.Uniforms.Uniform := 0;
        Time_ID                 : GL.Uniforms.Uniform := 0;
        DM_ID                   : GL.Uniforms.Uniform := 0;
    end record;

    Jav_Stand_Uniforms : Jav_Stand_Uniform;

    --  -------------------------------------------------------------------------

    procedure Init_Jav_Stand_Shader
      (Jav_Stand_Shader : out GL.Objects.Programs.Program) is
        use GL.Objects.Shaders;
        use GL.Types.Singles;
        use Program_Loader;
    begin
        Jav_Stand_Shader := Program_From
          ((Src ("src/shaders_3_2/jav_stand.vert", Vertex_Shader),
           Src ("src/shaders_3_2/jav_stand.frag", Fragment_Shader)));

        Jav_Stand_Uniforms.Perspective_ID := Uniform_Location (Jav_Stand_Shader, "P");
        Jav_Stand_Uniforms.View_ID := Uniform_Location (Jav_Stand_Shader, "V");
        Jav_Stand_Uniforms.Model_ID :=  Uniform_Location (Jav_Stand_Shader, "M");
        Jav_Stand_Uniforms.Ol_Pass_ID :=  Uniform_Location (Jav_Stand_Shader, "ol_pass");
        Jav_Stand_Uniforms.DM_ID :=  Uniform_Location (Jav_Stand_Shader, "dm");
        Jav_Stand_Uniforms.Time_ID :=  Uniform_Location (Jav_Stand_Shader, "time");

        Use_Program (Jav_Stand_Shader);
        GL.Uniforms.Set_Int (Jav_Stand_Uniforms.DM_ID, 0);
        GL.Uniforms.Set_Single (Jav_Stand_Uniforms.Model_ID, Identity4);
        GL.Uniforms.Set_Single (Jav_Stand_Uniforms.Ol_Pass_ID, 0.0);
        GL.Uniforms.Set_Single (Jav_Stand_Uniforms.Perspective_ID, Identity4);
        GL.Uniforms.Set_Single (Jav_Stand_Uniforms.Time_ID, 0.0);
        GL.Uniforms.Set_Single (Jav_Stand_Uniforms.View_ID, Identity4);

    exception
        when others =>
            Put_Line ("An exception occurred in Coins_Shader_Manager.Init_Java_Stand_Shader.");
            raise;
    end Init_Jav_Stand_Shader;

    --  -------------------------------------------------------------------------

    procedure Set_DM (DM : Int) is
    begin
        GL.Uniforms.Set_Int (Jav_Stand_Uniforms.DM_ID, DM);
    end Set_DM;

    --  -------------------------------------------------------------------------

    procedure Set_Model (Model_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Jav_Stand_Uniforms.Model_ID, Model_Matrix);
    end Set_Model;

    --  -------------------------------------------------------------------------

    procedure Set_Perspective (Perspective_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Jav_Stand_Uniforms.Perspective_ID,
                                Perspective_Matrix);
    end Set_Perspective;

    --  -------------------------------------------------------------------------

    procedure Set_Ol_Pass (Ol_Pass : Single) is
    begin
        GL.Uniforms.Set_Single (Jav_Stand_Uniforms.Ol_Pass_ID, Ol_Pass);
    end Set_Ol_Pass;

    --  -------------------------------------------------------------------------

    procedure Set_Time (Time : Single) is
    begin
        GL.Uniforms.Set_Single (Jav_Stand_Uniforms.Time_ID, Time);
    end Set_Time;

    --  -------------------------------------------------------------------------

    procedure Set_View (View_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Jav_Stand_Uniforms.View_ID, View_Matrix);
    end Set_View;

    --  -------------------------------------------------------------------------

end Jav_Stand_Shader_Manager;
