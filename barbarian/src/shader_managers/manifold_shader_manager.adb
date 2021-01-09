
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;

with Maths;
with Program_Loader;

with Shader_Attributes;

package body Manifold_Shader_Manager is

    Render_Uniforms : Shader_Uniforms;

    procedure Init (Shader_Program : in out GL.Objects.Programs.Program) is
        use GL.Objects.Programs;
        use GL.Objects.Shaders;
        use GL.Types.Singles;
        use Program_Loader;
    begin
        Shader_Program := Program_From
          ((Src ("src/shaders_3_2/manifold.vert", Vertex_Shader),
           Src ("src/shaders_3_2/manifold.frag", Fragment_Shader)));

        Render_Uniforms.Ambient_Light_ID :=
          Uniform_Location (Shader_Program, "L_a");
        Render_Uniforms.Caster_Position_ID :=
          Uniform_Location (Shader_Program, "caster_pos_wor");
        Render_Uniforms.Cube_Texture_ID :=
          Uniform_Location (Shader_Program, "cube_texture");
        Render_Uniforms.Diff_Map_ID :=
          Uniform_Location (Shader_Program, "diff_map");
        Render_Uniforms.Dynamic_Light_Pos_ID :=
          Uniform_Location (Shader_Program, "dyn_light_pos_wor");
        Render_Uniforms.Dynamic_Light_Diff_ID :=
          Uniform_Location (Shader_Program, "dyn_light_diff");
        Render_Uniforms.Dynamic_Light_Spec_ID :=
          Uniform_Location (Shader_Program, "dyn_light_spec");
        Render_Uniforms.Dynamic_Light_Range_ID :=
          Uniform_Location (Shader_Program, "dyn_light_range");
        Render_Uniforms.Light_Specular_ID :=
          Uniform_Location (Shader_Program, "light_spec");
        Render_Uniforms.Model_Matrix_ID :=
          Uniform_Location (Shader_Program, "model_mat");
        Render_Uniforms.Outline_Pass_ID :=
          Uniform_Location (Shader_Program, "ol_pass");
        Render_Uniforms.Projection_Matrix_ID :=
          Uniform_Location (Shader_Program, "P");
        Render_Uniforms.Shadow_Enabled_ID :=
          Uniform_Location (Shader_Program, "model_mat");
        Render_Uniforms.Spec_Map_ID :=
          Uniform_Location (Shader_Program, "spec_map");
        Render_Uniforms.Static_Light_Indices_ID :=
          Uniform_Location (Shader_Program, "static_light_indices");
        Render_Uniforms.View_Matrix_ID :=
          Uniform_Location (Shader_Program, "V");
--          Game_Utils.Game_Log ("Manifold_Shader_Manager Render_Uniforms initialized.");

        Use_Program (Shader_Program);
        GL.Uniforms.Set_Single (Render_Uniforms.Ambient_Light_ID, Maths.Vec3_0);
        GL.Uniforms.Set_Single (Render_Uniforms.Caster_Position_ID, Maths.Vec3_0);
        GL.Uniforms.Set_Int (Render_Uniforms.Cube_Texture_ID, 0);
        GL.Uniforms.Set_Int (Render_Uniforms.Diff_Map_ID, 0);
        GL.Uniforms.Set_Single (Render_Uniforms.Dynamic_Light_Pos_ID, Maths.Vec3_0);
        GL.Uniforms.Set_Single (Render_Uniforms.Dynamic_Light_Diff_ID, Maths.Vec3_0);
        GL.Uniforms.Set_Single (Render_Uniforms.Dynamic_Light_Spec_ID, Maths.Vec3_0);
        GL.Uniforms.Set_Single (Render_Uniforms.Dynamic_Light_Range_ID, 0.0);
        GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Identity4);
        GL.Uniforms.Set_Single (Render_Uniforms.Outline_Pass_ID, 0.0);
        GL.Uniforms.Set_Single (Render_Uniforms.Projection_Matrix_ID, Identity4);
        GL.Uniforms.Set_Single (Render_Uniforms.Shadow_Enabled_ID, 0.0);
        GL.Uniforms.Set_Int (Render_Uniforms.Spec_Map_ID, 0);
--          GL.Uniforms.Set_Single (Render_Uniforms.Static_Light_Indices_ID, Maths.Vec3_0);
        GL.Uniforms.Set_Single (Render_Uniforms.View_Matrix_ID, Identity4);
--          Game_Utils.Game_Log ("Manifold_Shader_Manager Uniforms initialized.");

    exception
        when others =>
            Put_Line ("An exception occurred in Manifold_Shader_Manager.Init.");
            raise;
    end Init;

    --  -------------------------------------------------------------------------

    procedure Set_Ambient_Light (Level : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Ambient_Light_ID, Level);
    end Set_Ambient_Light;

    --  -------------------------------------------------------------------------

    procedure Set_Caster_Position (Position : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Caster_Position_ID, Position);
    end Set_Caster_Position;

    --  -------------------------------------------------------------------------

    procedure Set_Cube_Texture (Texture : Int) is
    begin
        GL.Uniforms.Set_Int (Render_Uniforms.Cube_Texture_ID, Texture);
    end Set_Cube_Texture;

    --  -------------------------------------------------------------------------

    procedure Set_Diff_Map (Map : Int) is
    begin
        GL.Uniforms.Set_Int (Render_Uniforms.Diff_Map_ID, Map);
    end Set_Diff_Map;

    --  -------------------------------------------------------------------------

    procedure Set_Dynamic_Light_Pos (Position : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Dynamic_Light_Pos_ID, Position);
    end Set_Dynamic_Light_Pos;

    --  -------------------------------------------------------------------------

    procedure Set_Dynamic_Light_Diff (Diff : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Dynamic_Light_Diff_ID, Diff);
    end Set_Dynamic_Light_Diff;

    --  -------------------------------------------------------------------------

    procedure Set_Dynamic_Light_Spec (Spec : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Dynamic_Light_Spec_ID, Spec);
    end Set_Dynamic_Light_Spec;

    --  -------------------------------------------------------------------------

    procedure Set_Dynamic_Light_Range (Light_Range : Single) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Dynamic_Light_Range_ID, Light_Range);
    end Set_Dynamic_Light_Range;

    --  -------------------------------------------------------------------------

    procedure Set_Lights_Diffuse (Diffuse : Singles.Vector3_Array) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Light_Diffuse_ID, Diffuse);
    end Set_Lights_Diffuse;

    --  -------------------------------------------------------------------------

    procedure Set_Light_Positions (Positions : Singles.Vector3_Array) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Light_Position_ID, Positions);
    end Set_Light_Positions;

    --  -------------------------------------------------------------------------

    procedure Set_Light_Ranges (Light_Range : Single_Array) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Light_Range_ID, Light_Range);
    end Set_Light_Ranges;

    --  -------------------------------------------------------------------------

    procedure Set_Lights_Specular (Specular : Singles.Vector3_Array) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Light_Specular_ID, Specular);
    end Set_Lights_Specular;

    --  -------------------------------------------------------------------------

    procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Model_Matrix);
    end Set_Model_Matrix;

    --  -------------------------------------------------------------------------

    procedure Set_Outline_Pass (Pass : Single) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Outline_Pass_ID, Pass);
    end Set_Outline_Pass;

    --  -------------------------------------------------------------------------

    procedure Set_Projection_Matrix (Projection_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single
          (Render_Uniforms.Projection_Matrix_ID, Projection_Matrix);
    end Set_Projection_Matrix;

    --  -------------------------------------------------------------------------

    procedure Set_Shadow_Enabled (Shadow : Single) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.Shadow_Enabled_ID, Shadow);
    end Set_Shadow_Enabled;

    --  -------------------------------------------------------------------------

    procedure Set_Spec_Map (Map : Int) is
    begin
        GL.Uniforms.Set_Int (Render_Uniforms.Spec_Map_ID, Map);
    end Set_Spec_Map;

    --  -------------------------------------------------------------------------

    procedure Set_Static_Light_Indices (Indices : Ints.Vector2) is
    begin
        GL.Uniforms.Set_Int (Render_Uniforms.Static_Light_Indices_ID, Indices);
    end Set_Static_Light_Indices;

    --  ----------------------------------------------------------

    procedure Set_View_Matrix (View_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Render_Uniforms.View_Matrix_ID, View_Matrix);
    end Set_View_Matrix;

    --  -------------------------------------------------------------------------

end Manifold_Shader_Manager;
