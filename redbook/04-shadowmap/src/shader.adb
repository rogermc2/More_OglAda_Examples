
with GL.Objects.Shaders;
with Program_Loader;

package body Shader is
   procedure Init (Light_Program, Scene_Program  : in out GL.Objects.Programs.Program;
                   Light_Uniforms : out Light_Uniform_IDs;
                   Scene_Uniforms : out Scene_Uniform_IDs) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use Program_Loader;
   begin
       Light_Program := Program_From
        ((
         Src ("src/shaders/shadowmap_shadow.vs", Vertex_Shader),
         Src ("src/shaders/shadowmap_shadow.fs", Fragment_Shader)));
       Scene_Program := Program_From
        ((Src ("src/shaders/shadowmap_scene.vs", Vertex_Shader),
         Src ("src/shaders/shadowmap_scene.fs", Fragment_Shader)));

      Light_Uniforms.MVP_Matrix_ID :=
        Uniform_Location (Light_Program, "model_view_projection_matrix");

      Scene_Uniforms.View_Matrix_ID :=
        Uniform_Location (Scene_Program, "view_matrix");
      Scene_Uniforms.Model_Matrix_ID :=
        Uniform_Location (Scene_Program, "model_matrix");
      Scene_Uniforms.Projection_Matrix_ID :=
        Uniform_Location (Scene_Program, "projection_matrix");
      Scene_Uniforms.Shadow_Matrix_ID :=
        Uniform_Location (Scene_Program, "shadow_matrix");
      Scene_Uniforms.Light_Position_Matrix_ID :=
        Uniform_Location (Scene_Program, "light_position");
      Scene_Uniforms.Ambient_Matrix_ID :=
        Uniform_Location (Scene_Program, "material_ambient");
      Scene_Uniforms.Diffuse_Matrix_ID :=
        Uniform_Location (Scene_Program, "material_diffuse");
      Scene_Uniforms.Specular_Matrix_ID :=
        Uniform_Location (Scene_Program, "material_specular");
      Scene_Uniforms.Specular_Power_ID :=
        Uniform_Location (Scene_Program, "material_specular_power");
      Scene_Uniforms.Depth_Texture :=
        Uniform_Location (Scene_Program, "depth_texture");
   end Init;

   --  -------------------------------------------------------------------------
end Shader;
