
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Uniforms;

with Program_Loader;

with Coins_Shader_Manager;
with Game_Utils;
with Jav_Stand_Shader_Manager;
with Mesh_Loader;
with Portal_Shader_Manager;
with Properties_Basic_Shader_Manager;
with Properties_Skinned_Shader_Manager;
with Shader_Attributes;
with Sound_Sphere_Shader_Manager;

package body Properties_Shader_Manager is
    use GL.Objects.Programs;

    Sphere_Mesh_Index           : Integer := 0;
    Bounding_Sphere_VAO         : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Bounding_Sphere_Point_Count : Integer := 0;
    Light_Init        : constant Singles.Vector3_Array (1 .. 32) :=
                          (others => (0.0, 0.0, 0.0));
    Range_Init        : constant Single_Array (1 .. 32) := (others => 0.0);

    --  -------------------------------------------------------------------------

    procedure Load_Prop_Shaders is
        Identity4_Array : constant Singles.Matrix4_Array
          (1 .. Mesh_Loader.Max_Bones) := (others => Singles.Identity4);
        VAO             : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    begin
        Game_Utils.Game_Log ("___INITIALIZING PROP SHADERS---");
        Properties_Basic_Shader_Manager.Init_Prop_Shader (Prop_Shader);
        Properties_Basic_Shader_Manager.Set_Diff_Map (0);
        Properties_Basic_Shader_Manager.Set_Spec_Map (1);
        Properties_Basic_Shader_Manager.Set_Norm_Map (2);
        Properties_Basic_Shader_Manager.Set_Cube_Texture (3);

        Properties_Skinned_Shader_Manager.Init_Skinned_Shader (Prop_Skinned_Shader);
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

end Properties_Shader_Manager;
