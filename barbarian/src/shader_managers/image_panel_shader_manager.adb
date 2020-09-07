
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with Program_Loader;

with Shader_Attributes;

package body Image_Panel_Shader_Manager is

   Render_Uniforms : Shader_Uniforms;

   procedure Init (Shader_Program : in out GL.Objects.Programs.Program) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use GL.Types.Singles;
      use Program_Loader;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders_3_2/image_panel.vert", Vertex_Shader),
         Src ("src/shaders_3_2/image_panel.frag", Fragment_Shader)));

      Bind_Attrib_Location (Shader_Program, Shader_Attributes.Attrib_VP, "vp");

      Render_Uniforms.Model_Matrix_ID :=
        Uniform_Location (Shader_Program, "model_matrix");
      Render_Uniforms.Texture_Unit_ID := Uniform_Location (Shader_Program, "tex");

      Use_Program (Shader_Program);
      GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Identity4);
      GL.Uniforms.Set_Int (Render_Uniforms.Texture_Unit_ID, 0);

   exception
      when others =>
         Put_Line ("An exception occurred in Image_Panel_Shader_Manager.Init.");
         raise;
   end Init;

  --  -------------------------------------------------------------------------

   procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Model_Matrix);
   end Set_Model_Matrix;

   --  -------------------------------------------------------------------------

   procedure Set_Texture_Unit (Texture_Unit : Int) is
   begin
      GL.Uniforms.Set_Int (Render_Uniforms.Texture_Unit_ID, Texture_Unit);
   end Set_Texture_Unit;

   --  -------------------------------------------------------------------------

end Image_Panel_Shader_Manager;
