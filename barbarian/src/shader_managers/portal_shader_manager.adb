
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Uniforms;

with Program_Loader;

with Shader_Attributes;

package body Portal_Shader_Manager is
    use GL.Objects.Programs;

    type Portal_Uniform is record
        Perspective_ID          : GL.Uniforms.Uniform := 0;
        View_ID                 : GL.Uniforms.Uniform := 0;
        Model_ID                : GL.Uniforms.Uniform := 0;
        Time_ID                 : GL.Uniforms.Uniform := 0;
        DM_ID                   : GL.Uniforms.Uniform := 0;
    end record;

    Portal_Uniforms : Portal_Uniform;

    --  -------------------------------------------------------------------------

    procedure Init_Portal_Shader
      (Portal_Shader : out GL.Objects.Programs.Program) is
        use GL.Objects.Shaders;
        use GL.Types.Singles;
        use Program_Loader;
    begin
        Portal_Shader := Program_From
          ((Src ("src/shaders_3_2/portal.vert", Vertex_Shader),
           Src ("src/shaders_3_2/portal.frag", Fragment_Shader)));

        Bind_Attrib_Location (Portal_Shader, Shader_Attributes.Attrib_VP, "vp");

        Portal_Uniforms.Perspective_ID := Uniform_Location (Portal_Shader, "P");
        Portal_Uniforms.View_ID := Uniform_Location (Portal_Shader, "V");
        Portal_Uniforms.Model_ID :=  Uniform_Location (Portal_Shader, "M");
        Portal_Uniforms.DM_ID :=  Uniform_Location (Portal_Shader, "dm");
        Portal_Uniforms.Time_ID :=  Uniform_Location (Portal_Shader, "time");

        Use_Program (Portal_Shader);
        GL.Uniforms.Set_Int (Portal_Uniforms.DM_ID, 0);
        GL.Uniforms.Set_Single (Portal_Uniforms.Model_ID, Identity4);
        GL.Uniforms.Set_Single (Portal_Uniforms.Perspective_ID, Identity4);
        GL.Uniforms.Set_Single (Portal_Uniforms.Time_ID, 0.0);
        GL.Uniforms.Set_Single (Portal_Uniforms.View_ID, Identity4);

    exception
        when others =>
            Put_Line ("An exception occurred in Coins_Shader_Manager.Init_Portal_Shader.");
            raise;
    end Init_Portal_Shader;

    --  -------------------------------------------------------------------------

    procedure Set_DM (DM : Int) is
    begin
        GL.Uniforms.Set_Int (Portal_Uniforms.DM_ID, DM);
    end Set_DM;

    --  -------------------------------------------------------------------------

    procedure Set_Model (Model_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Portal_Uniforms.Model_ID, Model_Matrix);
    end Set_Model;

    --  -------------------------------------------------------------------------

    procedure Set_Perspective (Perspective_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Portal_Uniforms.Perspective_ID,
                                Perspective_Matrix);
    end Set_Perspective;

    --  -------------------------------------------------------------------------

    procedure Set_Time (theTime : Single) is
    begin
        GL.Uniforms.Set_Single (Portal_Uniforms.Time_ID, theTime);
    end Set_Time;

   --  -------------------------------------------------------------------------

    procedure Set_View (View_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Portal_Uniforms.View_ID, View_Matrix);
    end Set_View;

    --  -------------------------------------------------------------------------

end Portal_Shader_Manager;
