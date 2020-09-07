
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Uniforms;

with Program_Loader;

with Shader_Attributes;

package body Properties_Skinned_Shader_Manager is
    use GL.Objects.Programs;

    type Skinned_Properties_Uniforms is record
        Perspective_ID          : GL.Uniforms.Uniform := 0;
        View_ID                 : GL.Uniforms.Uniform := 0;
        Model_ID                : GL.Uniforms.Uniform := 0;
        Bone_Matrices_ID        : GL.Uniforms.Uniform := 0;
        Ol_Pass_ID              : GL.Uniforms.Uniform := 0;
        Dyn_Light_Pos_World_ID  : GL.Uniforms.Uniform := 0;
        Dyn_Light_Diff_ID       : GL.Uniforms.Uniform := 0;
        Dyn_Light_Spec_ID       : GL.Uniforms.Uniform := 0;
        Dyn_Light_Range_ID      : GL.Uniforms.Uniform := 0;
        L_A_ID                  : GL.Uniforms.Uniform := 0;
        Static_Light_Indices_ID : GL.Uniforms.Uniform := 0;
        Light_Pos_ID            : GL.Uniforms.Uniform := 0;
        Light_Diff_ID           : GL.Uniforms.Uniform := 0;
        Light_Spec_ID           : GL.Uniforms.Uniform := 0;
        Light_Range_ID          : GL.Uniforms.Uniform := 0;
        Diff_Map_ID             : GL.Uniforms.Uniform := 0;
        Spec_Map_ID             : GL.Uniforms.Uniform := 0;
        Shadow_Enabled_ID       : GL.Uniforms.Uniform := 0;
        Cube_Texture_ID         : GL.Uniforms.Uniform := 0;
        Caster_Pos_World_ID     : GL.Uniforms.Uniform := 0;
    end record;

    Property_Uniforms : Skinned_Properties_Uniforms;
    Vec2_Init         : constant GL.Types.Singles.Vector2 := (0.0, 0.0);
    Vec3_Init         : constant GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
    Light_Init        : constant Singles.Vector3_Array (1 .. 32) :=
                          (others => (0.0, 0.0, 0.0));
    Range_Init        : constant Single_Array (1 .. 32) := (others => 0.0);

    --  -------------------------------------------------------------------------

    procedure Init_Prop_Skinned_Shader
      (Prop_Shader : out GL.Objects.Programs.Program) is
        use GL.Objects.Shaders;
        use GL.Types.Singles;
        use Program_Loader;
    begin
        Prop_Shader := Program_From
          ((Src ("src/shaders_3_2/prop.vert", Vertex_Shader),
           Src ("src/shaders_3_2/prop.frag", Fragment_Shader)));

        Bind_Attrib_Location (Prop_Shader, Shader_Attributes.Attrib_VP, "vp");

        Property_Uniforms.Perspective_ID := Uniform_Location (Prop_Shader, "P");
        Property_Uniforms.View_ID := Uniform_Location (Prop_Shader, "V");
        Property_Uniforms.Model_ID :=
          Uniform_Location (Prop_Shader, "M");
        Property_Uniforms.Ol_Pass_ID := Uniform_Location (Prop_Shader, "ol_pass");
        Property_Uniforms.Dyn_Light_Pos_World_ID :=
          Uniform_Location (Prop_Shader, "dyn_light_pos_wor");
        Property_Uniforms.Dyn_Light_Diff_ID :=
          Uniform_Location (Prop_Shader, "dyn_light_diff");
        Property_Uniforms.Dyn_Light_Spec_ID :=
          Uniform_Location (Prop_Shader, "dyn_light_spec");
        Property_Uniforms.Dyn_Light_Range_ID :=
          Uniform_Location (Prop_Shader, "dyn_light_range");
        Property_Uniforms.L_A_ID :=
          Uniform_Location (Prop_Shader, "L_a");
        Property_Uniforms.Static_Light_Indices_ID :=
          Uniform_Location (Prop_Shader, "static_light_indices");
        Property_Uniforms.Light_Pos_ID :=
          Uniform_Location (Prop_Shader, "light_pos");
        Property_Uniforms.Light_Diff_ID :=
          Uniform_Location (Prop_Shader, "light_diff");
        Property_Uniforms.Light_Spec_ID :=
          Uniform_Location (Prop_Shader, "light_spec");
        Property_Uniforms.Light_Range_ID :=
          Uniform_Location (Prop_Shader, "light_range");
        Property_Uniforms.Diff_Map_ID :=
          Uniform_Location (Prop_Shader, "diff_map");
        Property_Uniforms.Spec_Map_ID :=
          Uniform_Location (Prop_Shader, "spec_map");
        Property_Uniforms.Shadow_Enabled_ID :=
          Uniform_Location (Prop_Shader, "shadow_enabled");
        Property_Uniforms.Cube_Texture_ID :=
          Uniform_Location (Prop_Shader, "cube_texture");
        Property_Uniforms.Caster_Pos_World_ID :=
          Uniform_Location (Prop_Shader, "caster_pos_wor");

        Use_Program (Prop_Shader);
        GL.Uniforms.Set_Single (Property_Uniforms.Caster_Pos_World_ID, Vec3_Init);
        GL.Uniforms.Set_UInt (Property_Uniforms.Cube_Texture_ID, 0);
        GL.Uniforms.Set_UInt (Property_Uniforms.Diff_Map_ID, 0);
        GL.Uniforms.Set_Single (Property_Uniforms.Dyn_Light_Pos_World_ID, Vec3_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.Dyn_Light_Diff_ID, Vec3_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.Dyn_Light_Spec_ID, Vec3_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.L_A_ID, Vec3_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.Light_Pos_ID, Light_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.Light_Diff_ID, Light_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.Light_Spec_ID, Light_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.Light_Range_ID, Range_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.Model_ID, Identity4);
        GL.Uniforms.Set_Single (Property_Uniforms.Ol_Pass_ID, 0.0);
        GL.Uniforms.Set_Single (Property_Uniforms.Perspective_ID, Identity4);
        GL.Uniforms.Set_Single (Property_Uniforms.Shadow_Enabled_ID, 0.0);
        GL.Uniforms.Set_UInt (Property_Uniforms.Spec_Map_ID, 0);
        GL.Uniforms.Set_Single (Property_Uniforms.Static_Light_Indices_ID, Vec2_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.View_ID, Identity4);

    exception
        when others =>
            Put_Line ("An exception occurred in GUI_Atlas_Shader_Manager.Init.");
            raise;
    end Init_Prop_Skinned_Shader;

    --  -------------------------------------------------------------------------

    procedure Set_Bone_Matrices (Bone_Matrices : Singles.Matrix4_Array)  is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Cube_Texture_ID, Bone_Matrices);
    end Set_Bone_Matrices;

    --  -------------------------------------------------------------------------

    procedure Set_Caster_Position (Position : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Caster_Pos_World_ID, Position);
    end Set_Caster_Position;

    --  -------------------------------------------------------------------------

    procedure Set_Cube_Texture (Texture : UInt)  is
    begin
        GL.Uniforms.Set_UInt (Property_Uniforms.Cube_Texture_ID, Texture);
    end Set_Cube_Texture;

    --  -------------------------------------------------------------------------

    procedure Set_Diff_Map (Diff_Map : UInt) is
    begin
        GL.Uniforms.Set_UInt (Property_Uniforms.Diff_Map_ID, Diff_Map);
    end Set_Diff_Map;

    --  -------------------------------------------------------------------------

    procedure Set_Dyn_Light_Pos (Position  : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Dyn_Light_Pos_World_ID, Position);
    end Set_Dyn_Light_Pos;

    --  -------------------------------------------------------------------------

    procedure Set_Dyn_Light_Diff (Diff  : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Dyn_Light_Diff_ID, Diff);
    end Set_Dyn_Light_Diff;

    --  -------------------------------------------------------------------------

    procedure Set_Dyn_Light_Spec (Spec : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Dyn_Light_Spec_ID, Spec);
    end Set_Dyn_Light_Spec;

    --  -------------------------------------------------------------------------

    procedure Set_Dyn_Light_Range (Light_Range : Single) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Dyn_Light_Range_ID, Light_Range);
    end Set_Dyn_Light_Range;

    --  -------------------------------------------------------------------------

    procedure Set_L_A (L_A : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.L_A_ID, L_A);
    end Set_L_A;

    --  -------------------------------------------------------------------------

    procedure Set_Light_Pos (Position : Light_Array) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Light_Pos_ID,
                                Singles.Vector3_Array (Position));
    end Set_Light_Pos;

    --  -------------------------------------------------------------------------

    procedure Set_Light_Diff (Diff : Light_Array) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Light_Diff_ID,
                                Singles.Vector3_Array (Diff));
    end Set_Light_Diff;

    --  -------------------------------------------------------------------------

    procedure Set_Light_Spec (Spec : Light_Array) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Light_Spec_ID,
                                Singles.Vector3_Array (Spec));
    end Set_Light_Spec;

    --  -------------------------------------------------------------------------

    procedure Set_Light_Range (Light_Range :  Light_Range_Array) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Light_Range_ID,
                                Single_Array (Light_Range));
    end Set_Light_Range;

    --  -------------------------------------------------------------------------

    procedure Set_Model (Model_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Model_ID, Model_Matrix);
    end Set_Model;

    --  -------------------------------------------------------------------------

    procedure Set_Ol_Pass (Ol_Pass : Single) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Ol_Pass_ID, Ol_Pass);
    end Set_Ol_Pass;

    --  -------------------------------------------------------------------------
    procedure Set_Perspective (Perspective_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Perspective_ID,
                                Perspective_Matrix);
    end Set_Perspective;

    --  -------------------------------------------------------------------------

    procedure Set_Shadow_Enabled (Enable : Single) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Shadow_Enabled_ID, Enable);
    end Set_Shadow_Enabled;

    --  -------------------------------------------------------------------------

    procedure Set_Spec_Map (Spec_Map : UInt) is
    begin
        GL.Uniforms.Set_UInt (Property_Uniforms.Spec_Map_ID, Spec_Map);
    end Set_Spec_Map;

    --  -------------------------------------------------------------------------

    procedure Set_Static_Light_Indices (Indices : Singles.Vector2) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Static_Light_Indices_ID, Indices);
    end Set_Static_Light_Indices;

    --  -------------------------------------------------------------------------

    procedure Set_View (View_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.View_ID, View_Matrix);
    end Set_View;

    --  -------------------------------------------------------------------------

end Properties_Skinned_Shader_Manager;