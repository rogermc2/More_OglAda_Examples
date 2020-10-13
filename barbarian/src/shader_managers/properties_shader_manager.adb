
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Uniforms;

with Program_Loader;

with Coins_Shader_Manager;
with Game_Utils;
with Jav_Stand_Shader_Manager;
with Mesh_Loader;
with Portal_Shader_Manager;
with Properties_Skinned_Shader_Manager;
with Shader_Attributes;
with Sound_Sphere_Shader_Manager;

package body Properties_Shader_Manager is
    use GL.Objects.Programs;

    type Properties_Uniforms is record
        Perspective_ID          : GL.Uniforms.Uniform := 0;
        View_ID                 : GL.Uniforms.Uniform := 0;
        Model_ID                : GL.Uniforms.Uniform := 0;
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
        Norm_Map_ID             : GL.Uniforms.Uniform := 0;
        Inv_M_ID                : GL.Uniforms.Uniform := 0;
        Camera_Pos_World_ID     : GL.Uniforms.Uniform := 0;
        Shadow_Enabled_ID       : GL.Uniforms.Uniform := 0;
        Cube_Texture_ID         : GL.Uniforms.Uniform := 0;
        Caster_Pos_World_ID     : GL.Uniforms.Uniform := 0;
    end record;

    Property_Uniforms      : Properties_Uniforms;
    Prop_Shader            : Program;  --  Basic shader
    Prop_Skinned_Shader    : Program;  --  Skinned shader
    Coins_Shader           : Program;  --  Shiny treasure shader
    Jav_Stand_Shader       : Program;  --  Pulsing 'look at me' lighting shader
    Portal_Shader          : Program;  --  Wobbly portal shader
    Bounding_Sphere_Shader : Program;  --  Bounding sphere shader

    Sphere_Mesh_Index           : Integer := 0;
    Bounding_Sphere_VAO         : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Bounding_Sphere_Point_Count : Integer := 0;

    Vec2_Init         : constant GL.Types.Singles.Vector2 := (0.0, 0.0);
    IVec2_Init        : constant GL.Types.Ints.Vector2 := (0, 0);
    Vec3_Init         : constant GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
    Light_Init        : constant Singles.Vector3_Array (1 .. 32) :=
                          (others => (0.0, 0.0, 0.0));
    Range_Init        : constant Single_Array (1 .. 32) := (others => 0.0);

    --  -------------------------------------------------------------------------

    procedure Init_Prop_Shader is
        use GL.Objects.Shaders;
        use GL.Types.Singles;
        use Program_Loader;
    begin
        Game_Utils.Game_Log ("___INITIALIZING PROP SHADER---");
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
        Property_Uniforms.Norm_Map_ID :=
          Uniform_Location (Prop_Shader, "norm_map");
        Property_Uniforms.Inv_M_ID :=
          Uniform_Location (Prop_Shader, "inv_M");
        Property_Uniforms.Camera_Pos_World_ID :=
          Uniform_Location (Prop_Shader, "cam_pos_wor");
        Property_Uniforms.Shadow_Enabled_ID :=
          Uniform_Location (Prop_Shader, "shadow_enabled");
        Property_Uniforms.Cube_Texture_ID :=
          Uniform_Location (Prop_Shader, "cube_texture");
        Property_Uniforms.Caster_Pos_World_ID :=
          Uniform_Location (Prop_Shader, "caster_pos_wor");

        Use_Program (Prop_Shader);
        GL.Uniforms.Set_Single (Property_Uniforms.Camera_Pos_World_ID, Vec3_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.Caster_Pos_World_ID, Vec3_Init);
        GL.Uniforms.Set_Int (Property_Uniforms.Cube_Texture_ID, 0);
        GL.Uniforms.Set_Int (Property_Uniforms.Diff_Map_ID, 0);
        GL.Uniforms.Set_Single (Property_Uniforms.Dyn_Light_Pos_World_ID, Vec3_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.Dyn_Light_Diff_ID, Vec3_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.Dyn_Light_Spec_ID, Vec3_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.Inv_M_ID, Identity4);
        GL.Uniforms.Set_Single (Property_Uniforms.L_A_ID, Vec3_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.Light_Pos_ID, Light_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.Light_Diff_ID, Light_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.Light_Spec_ID, Light_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.Light_Range_ID, Range_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.Model_ID, Identity4);
        GL.Uniforms.Set_Int (Property_Uniforms.Norm_Map_ID, 0);
        GL.Uniforms.Set_Single (Property_Uniforms.Ol_Pass_ID, 0.0);
        GL.Uniforms.Set_Single (Property_Uniforms.Perspective_ID, Identity4);
        GL.Uniforms.Set_Single (Property_Uniforms.Shadow_Enabled_ID, 0.0);
        GL.Uniforms.Set_Int (Property_Uniforms.Spec_Map_ID, 0);
        GL.Uniforms.Set_Int (Property_Uniforms.Static_Light_Indices_ID, IVec2_Init);
        GL.Uniforms.Set_Single (Property_Uniforms.View_ID, Identity4);
        Game_Utils.Game_Log ("___PROP SHADER INITIALIZED---");

    exception
        when others =>
            Put_Line ("An exception occurred in Properties_Shader_Manager.Init_Prop_Shader.");
            raise;
    end Init_Prop_Shader;

    --  -------------------------------------------------------------------------

    procedure Load_Prop_Shaders is
        Identity4_Array : constant Singles.Matrix4_Array
          (1 .. Mesh_Loader.Max_Bones) := (others => Singles.Identity4);
        VAO             : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    begin
        Game_Utils.Game_Log ("___INITIALIZING PROP SHADERS---");
        Init_Prop_Shader;
        Set_Diff_Map (0);
        Set_Spec_Map (1);
        Set_Norm_Map (2);
        Set_Cube_Texture (3);

        Properties_Skinned_Shader_Manager.Init_Prop_Skinned_Shader
          (Prop_Skinned_Shader);
        Properties_Skinned_Shader_Manager.Set_Diff_Map (0);
        Properties_Skinned_Shader_Manager.Set_Spec_Map (1);
        Properties_Skinned_Shader_Manager.Set_Cube_Texture (3);
        Properties_Skinned_Shader_Manager.Set_Bone_Matrices (Identity4_Array);

        Coins_Shader_Manager.Init_Coins_Shader (Coins_Shader);
        Coins_Shader_Manager.Set_DM (0);
        Coins_Shader_Manager.Set_Cube_Texture (1);

        Jav_Stand_Shader_Manager.Init_Jav_Stand_Shader (Jav_Stand_Shader);
        Portal_Shader_Manager.Init_Portal_Shader (Portal_Shader);

        Sound_Sphere_Shader_Manager.Init_Sound_Sphere_Shader (Bounding_Sphere_Shader);
        Sphere_Mesh_Index := Mesh_Loader.Load_Managed_Mesh
          ("src/meshes/unit_sphere.apg", True, False, False, False, False);
        if Mesh_Loader.Loaded_Mesh_VAO (Sphere_Mesh_Index, VAO) then
            Bounding_Sphere_VAO := VAO;
            Bounding_Sphere_Point_Count := Mesh_Loader.Point_Count (Sphere_Mesh_Index);
        else
            Put ("Properties_Shader_Manager.Load_Prop_Shaders ");
            Put_Line ("failed to load Bounding_Sphere_VAO");
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in Properties_Shader_Manager.Load_Prop_Shaders.");
            raise;

    end Load_Prop_Shaders;

    --  -------------------------------------------------------------------------

    procedure Set_Camera_Position (Position : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Camera_Pos_World_ID, Position);
    end Set_Camera_Position;

    --  -------------------------------------------------------------------------

    procedure Set_Caster_Position (Position : Singles.Vector3) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Caster_Pos_World_ID, Position);
    end Set_Caster_Position;

    --  -------------------------------------------------------------------------

    procedure Set_Cube_Texture (Texture : Int)  is
    begin
        GL.Uniforms.Set_Int (Property_Uniforms.Cube_Texture_ID, Texture);
    end Set_Cube_Texture;

    --  -------------------------------------------------------------------------

    procedure Set_Diff_Map (Diff_Map : Int) is
    begin
        GL.Uniforms.Set_Int (Property_Uniforms.Diff_Map_ID, Diff_Map);
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

    procedure Set_Inverse_Map (Inverse_Map : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Inv_M_ID, Inverse_Map);
    end Set_Inverse_Map;

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

    procedure Set_Norm_Map (Norm_Map : Int) is
    begin
        GL.Uniforms.Set_Int (Property_Uniforms.Norm_Map_ID, Norm_Map);
    end Set_Norm_Map;

    --  -------------------------------------------------------------------------

    procedure Set_Perspective (Perspective_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Perspective_ID,
                                Perspective_Matrix);
    end Set_Perspective;

    --  -------------------------------------------------------------------------

    procedure Set_Ol_Pass (Ol_Pass : Single) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Ol_Pass_ID, Ol_Pass);
    end Set_Ol_Pass;

    --  -------------------------------------------------------------------------

    procedure Set_Shadow_Enabled (Enable : Single) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.Shadow_Enabled_ID, Enable);
    end Set_Shadow_Enabled;

    --  -------------------------------------------------------------------------

    procedure Set_Spec_Map (Spec_Map : Int) is
    begin
        GL.Uniforms.Set_Int (Property_Uniforms.Spec_Map_ID, Spec_Map);
    end Set_Spec_Map;

    --  -------------------------------------------------------------------------

    procedure Set_Static_Light_Indices (Indices : Ints.Vector2) is
    begin
        GL.Uniforms.Set_Int (Property_Uniforms.Static_Light_Indices_ID, Indices);
    end Set_Static_Light_Indices;

    --  -------------------------------------------------------------------------

    procedure Set_View (View_Matrix : Singles.Matrix4) is
    begin
        GL.Uniforms.Set_Single (Property_Uniforms.View_ID, View_Matrix);
    end Set_View;

    --  -------------------------------------------------------------------------

end Properties_Shader_Manager;
