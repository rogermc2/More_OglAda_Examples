
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with Program_Loader;

with Shader_Attributes;

package body Health_Shader_Manager is

   Render_Uniforms : Shader_Uniforms;

   procedure Init (Shader_Program : in out GL.Objects.Programs.Program) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use GL.Types.Singles;
      use Program_Loader;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders_3_2/health.vert", Vertex_Shader),
         Src ("src/shaders_3_2/health.frag", Fragment_Shader)));

      Render_Uniforms.Base_Texture_ID :=
          Uniform_Location (Shader_Program, "tex_base");
      Render_Uniforms.Model_Matrix_ID :=
        Uniform_Location (Shader_Program, "model_matrix");
      Render_Uniforms.Red_Texture_ID :=
          Uniform_Location (Shader_Program, "tex_red");
      Render_Uniforms.Health_Factor_ID :=
          Uniform_Location (Shader_Program, "health_factor");

      Use_Program (Shader_Program);
      GL.Uniforms.Set_Int (Render_Uniforms.Base_Texture_ID, 0);
      GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Identity4);
      GL.Uniforms.Set_Int (Render_Uniforms.Red_Texture_ID, 0);
      GL.Uniforms.Set_Single (Render_Uniforms.Health_Factor_ID, 0.0);

   exception
      when others =>
         Put_Line ("An exception occurred in Health_Shader_Manager.Init.");
         raise;
   end Init;

  --  -------------------------------------------------------------------------

   procedure Set_Base_Texture (Texture : Int) is
   begin
      GL.Uniforms.Set_Int (Render_Uniforms.Base_Texture_ID, Texture);
   end Set_Base_Texture;

   --  -------------------------------------------------------------------------

   procedure Set_Health_Factor (Health_Factor : Single) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Health_Factor_ID, Health_Factor);
   end Set_Health_Factor;

   --  -------------------------------------------------------------------------

   procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Model_Matrix);
   end Set_Model_Matrix;

   --  -------------------------------------------------------------------------

   procedure Set_Red_Texture (Texture : Int) is
   begin
      GL.Uniforms.Set_Int (Render_Uniforms.Red_Texture_ID, Texture);
   end Set_Red_Texture;

   --  -------------------------------------------------------------------------

end Health_Shader_Manager;
