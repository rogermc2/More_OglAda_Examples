
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Uniforms;

with Program_Loader;

with Shader_Attributes;

package body Sprite_Shader_Manager is
    use GL.Objects.Programs;

    type Sprite_Uniforms is record
        Perspective_ID          : GL.Uniforms.Uniform := 0;
        View_ID                 : GL.Uniforms.Uniform := 0;
        Model_ID                : GL.Uniforms.Uniform := 0;
        Sprite_St_ID            : GL.Uniforms.Uniform := 0;
        Columns_ID              : GL.Uniforms.Uniform := 0;
        Rows_ID                 : GL.Uniforms.Uniform := 0;
        Current_Sprite_ID       : GL.Uniforms.Uniform := 0;
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
        Opacity_ID              : GL.Uniforms.Uniform := 0;
        Diff_Map_ID             : GL.Uniforms.Uniform := 0;
        Spec_Map_ID             : GL.Uniforms.Uniform := 0;
        Shadow_Enabled_ID       : GL.Uniforms.Uniform := 0;
        Cube_Texture_ID         : GL.Uniforms.Uniform := 0;
        Caster_Pos_World_ID     : GL.Uniforms.Uniform := 0;
    end record;

    Sprite_Uniform    : Sprite_Uniforms;
    Sprite_Shader     : Program;  --  Basic shader

    Vec2_Init         : constant GL.Types.Singles.Vector2 := (0.0, 0.0);
    Vec3_Init         : constant GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
    Light_Init        : constant Singles.Vector3_Array (1 .. 32) :=
                          (others => (0.0, 0.0, 0.0));
    Range_Init        : constant Single_Array (1 .. 32) := (others => 0.0);

    --  -------------------------------------------------------------------------

    procedure Init_Sprite_Shader is
        use GL.Objects.Shaders;
        use Program_Loader;
    begin
        Sprite_Shader := Program_From
          ((Src ("src/shaders_3_2/sprite.vert", Vertex_Shader),
           Src ("src/shaders_3_2/sprite.frag", Fragment_Shader)));

        Bind_Attrib_Location (Sprite_Shader, Shader_Attributes.Attrib_VP, "vp");

        Sprite_Uniform.Perspective_ID := Uniform_Location (Sprite_Shader, "P");
        Sprite_Uniform.View_ID := Uniform_Location (Sprite_Shader, "V");
        Sprite_Uniform.Model_ID :=  Uniform_Location (Sprite_Shader, "M");
        Sprite_Uniform.Sprite_St_ID := Uniform_Location (Sprite_Shader, "sprite_st");
        Sprite_Uniform.Columns_ID :=  Uniform_Location (Sprite_Shader, "columns");
        Sprite_Uniform.Rows_ID :=  Uniform_Location (Sprite_Shader, "rows");
        Sprite_Uniform.Current_Sprite_ID :=
          Uniform_Location (Sprite_Shader, "current_sprite");
        Sprite_Uniform.Dyn_Light_Pos_World_ID :=
          Uniform_Location (Sprite_Shader, "dyn_light_pos_wor");
        Sprite_Uniform.Dyn_Light_Diff_ID :=
          Uniform_Location (Sprite_Shader, "dyn_light_diff");
        Sprite_Uniform.Dyn_Light_Spec_ID :=
          Uniform_Location (Sprite_Shader, "dyn_light_spec");
        Sprite_Uniform.Dyn_Light_Range_ID :=
          Uniform_Location (Sprite_Shader, "dyn_light_range");
        Sprite_Uniform.L_A_ID :=
          Uniform_Location (Sprite_Shader, "L_a");
        Sprite_Uniform.Static_Light_Indices_ID :=
          Uniform_Location (Sprite_Shader, "static_light_indices");
        Sprite_Uniform.Light_Pos_ID :=
          Uniform_Location (Sprite_Shader, "light_pos");
        Sprite_Uniform.Light_Diff_ID :=
          Uniform_Location (Sprite_Shader, "light_diff");
        Sprite_Uniform.Light_Spec_ID :=
          Uniform_Location (Sprite_Shader, "light_spec");
        Sprite_Uniform.Light_Range_ID :=
          Uniform_Location (Sprite_Shader, "light_range");
        Sprite_Uniform.Diff_Map_ID :=
          Uniform_Location (Sprite_Shader, "diff_map");
        Sprite_Uniform.Spec_Map_ID :=
          Uniform_Location (Sprite_Shader, "spec_map");
        Sprite_Uniform.Opacity_ID :=
          Uniform_Location (Sprite_Shader, "opacity");
        Sprite_Uniform.Shadow_Enabled_ID :=
          Uniform_Location (Sprite_Shader, "shadow_enabled");
        Sprite_Uniform.Cube_Texture_ID :=
          Uniform_Location (Sprite_Shader, "cube_texture");
        Sprite_Uniform.Caster_Pos_World_ID :=
          Uniform_Location (Sprite_Shader, "caster_pos_wor");

    exception
        when others =>
            Put_Line ("An exception occurred in Properties_Shader_Manager.Init_Sprite_Shader.");
            raise;
    end Init_Sprite_Shader;

    --  -------------------------------------------------------------------------

    procedure Load_Sprite_Shaders is
        use GL.Types.Singles;
    begin
        Init_Sprite_Shader;
        Use_Program (Sprite_Shader);
        GL.Uniforms.Set_Single (Sprite_Uniform.Caster_Pos_World_ID, Vec3_Init);
        GL.Uniforms.Set_Single (Sprite_Uniform.Columns_ID, 1.0);
        GL.Uniforms.Set_UInt (Sprite_Uniform.Cube_Texture_ID, 0);
        GL.Uniforms.Set_Single (Sprite_Uniform.Current_Sprite_ID, 0.0);
        GL.Uniforms.Set_UInt (Sprite_Uniform.Diff_Map_ID, 0);
        GL.Uniforms.Set_Single (Sprite_Uniform.Dyn_Light_Pos_World_ID, Vec3_Init);
        GL.Uniforms.Set_Single (Sprite_Uniform.Dyn_Light_Diff_ID, Vec3_Init);
        GL.Uniforms.Set_Single (Sprite_Uniform.Dyn_Light_Spec_ID, Vec3_Init);
        GL.Uniforms.Set_Single (Sprite_Uniform.L_A_ID, Vec3_Init);
        GL.Uniforms.Set_Single (Sprite_Uniform.Light_Pos_ID, Light_Init);
        GL.Uniforms.Set_Single (Sprite_Uniform.Light_Diff_ID, Light_Init);
        GL.Uniforms.Set_Single (Sprite_Uniform.Light_Spec_ID, Light_Init);
        GL.Uniforms.Set_Single (Sprite_Uniform.Light_Range_ID, Range_Init);
        GL.Uniforms.Set_Single (Sprite_Uniform.Model_ID, Identity4);
        GL.Uniforms.Set_Single (Sprite_Uniform.Opacity_ID, 1.0);
        GL.Uniforms.Set_Single (Sprite_Uniform.Perspective_ID, Identity4);
        GL.Uniforms.Set_Single (Sprite_Uniform.Rows_ID, 1.0);
        GL.Uniforms.Set_Single (Sprite_Uniform.Shadow_Enabled_ID, 0.0);
        GL.Uniforms.Set_UInt (Sprite_Uniform.Spec_Map_ID, 1);
        GL.Uniforms.Set_Single (Sprite_Uniform.Static_Light_Indices_ID, Vec2_Init);
        GL.Uniforms.Set_Single (Sprite_Uniform.View_ID, Identity4);
    end Load_Sprite_Shaders;

    --  -------------------------------------------------------------------------

    procedure Set_Caster_Position (Position : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.Caster_Pos_World_ID, Position);
    end Set_Caster_Position;

    --  -------------------------------------------------------------------------

    procedure Set_Columns (Columns : Single) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.Columns_ID, Columns);
    end Set_Columns;

    --  -------------------------------------------------------------------------

    procedure Set_Cube_Texture (Texture : UInt)  is
    begin
        GL.Uniforms.Set_UInt (Sprite_Uniform.Cube_Texture_ID, Texture);
    end Set_Cube_Texture;

    --  -------------------------------------------------------------------------

    procedure Set_Current_Sprite (Sprite_ID : Single) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.Current_Sprite_ID, Sprite_ID);
    end Set_Current_Sprite;

    --  -------------------------------------------------------------------------
    procedure Set_Diff_Map (Diff_Map : UInt) is
    begin
        GL.Uniforms.Set_UInt (Sprite_Uniform.Diff_Map_ID, Diff_Map);
    end Set_Diff_Map;

    --  -------------------------------------------------------------------------

    procedure Set_Dyn_Light_Pos (Position  : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.Dyn_Light_Pos_World_ID, Position);
    end Set_Dyn_Light_Pos;

    --  -------------------------------------------------------------------------

    procedure Set_Dyn_Light_Diff (Diff  : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.Dyn_Light_Diff_ID, Diff);
    end Set_Dyn_Light_Diff;

    --  -------------------------------------------------------------------------

    procedure Set_Dyn_Light_Spec (Spec : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.Dyn_Light_Spec_ID, Spec);
    end Set_Dyn_Light_Spec;

    --  -------------------------------------------------------------------------

    procedure Set_Dyn_Light_Range (Light_Range : Single) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.Dyn_Light_Range_ID, Light_Range);
    end Set_Dyn_Light_Range;

    --  -------------------------------------------------------------------------

    procedure Set_L_A (L_A : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.L_A_ID, L_A);
    end Set_L_A;

    --  -------------------------------------------------------------------------

    procedure Set_Light_Pos (Position : Light_Array) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.Light_Pos_ID,
                                Singles.Vector3_Array (Position));
    end Set_Light_Pos;

    --  -------------------------------------------------------------------------

    procedure Set_Light_Diff (Diff : Light_Array) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.Light_Diff_ID,
                                Singles.Vector3_Array (Diff));
    end Set_Light_Diff;

    --  -------------------------------------------------------------------------

    procedure Set_Light_Spec (Spec : Light_Array) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.Light_Spec_ID,
                                Singles.Vector3_Array (Spec));
    end Set_Light_Spec;

    --  -------------------------------------------------------------------------

    procedure Set_Light_Range (Light_Range :  Light_Range_Array) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.Light_Range_ID,
                                Single_Array (Light_Range));
    end Set_Light_Range;

    --  -------------------------------------------------------------------------

    procedure Set_Model (Model_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.Model_ID, Model_Matrix);
    end Set_Model;

    --  -------------------------------------------------------------------------

    procedure Set_Opacity (Opacity : Single) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.Opacity_ID, Opacity);
    end Set_Opacity;

    --  -------------------------------------------------------------------------

    procedure Set_Perspective (Perspective_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.Perspective_ID,
                                Perspective_Matrix);
    end Set_Perspective;

    --  -------------------------------------------------------------------------

    procedure Set_Rows (Rows : Single) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.Rows_ID, Rows);
    end Set_Rows;

    --  -------------------------------------------------------------------------

    procedure Set_Shadow_Enabled (Enable : Single) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.Shadow_Enabled_ID, Enable);
    end Set_Shadow_Enabled;

    --  -------------------------------------------------------------------------

    procedure Set_Spec_Map (Spec_Map : UInt) is
    begin
        GL.Uniforms.Set_UInt (Sprite_Uniform.Spec_Map_ID, Spec_Map);
    end Set_Spec_Map;

    --  -------------------------------------------------------------------------

    procedure Set_Static_Light_Indices (Indices : Singles.Vector2) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.Static_Light_Indices_ID, Indices);
    end Set_Static_Light_Indices;

    --  -------------------------------------------------------------------------

    procedure Set_View (View_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Sprite_Uniform.View_ID, View_Matrix);
    end Set_View;

    --  -------------------------------------------------------------------------

    procedure Use_Sprite_Shader is
    begin
        Use_Program (Sprite_Shader);
    end Use_Sprite_Shader;

    --  -------------------------------------------------------------------------

end Sprite_Shader_Manager;
