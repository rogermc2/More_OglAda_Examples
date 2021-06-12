
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Uniforms;

with Program_Loader;

package body Shader_Manager_UI is

   UI_Program          : GL.Objects.Programs.Program;
   Texture_Uniform     : GL.Uniforms.Uniform;
   Model_Uniform       : GL.Uniforms.Uniform;
   View_Uniform        : GL.Uniforms.Uniform;
   Projection_Uniform  : GL.Uniforms.Uniform;

   procedure Init_Shaders is
      use GL.Objects.Shaders;
      use Program_Loader;
   begin
      UI_Program := Program_From
        ((Src ("src/shaders/ui_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/ui_fragment_shader.glsl", Fragment_Shader)));
      GL.Objects.Programs.Use_Program (UI_Program);

      Texture_Uniform :=
        GL.Objects.Programs.Uniform_Location (UI_Program, "texture2d");
      Model_Uniform :=
        GL.Objects.Programs.Uniform_Location (UI_Program, "model_matrix");
      Projection_Uniform :=
        GL.Objects.Programs.Uniform_Location (UI_Program, "projection_matrix");
      View_Uniform :=
        GL.Objects.Programs.Uniform_Location (UI_Program, "view_matrix");

      GL.Uniforms.Set_Single (Model_Uniform, GL.Types.Singles.Identity4);
      GL.Uniforms.Set_Single (Projection_Uniform, GL.Types.Singles.Identity4);
      GL.Uniforms.Set_Single (View_Uniform, GL.Types.Singles.Identity4);

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Shader_Manager.Init_Shaders.");
         Put_Line (Exception_Information (anError));
         raise;
   end Init_Shaders;

   --  ------------------------------------------------------------------------

   function Program_UI return GL.Objects.Programs.Program is
   begin
      return UI_Program;
   end Program_UI;

   --   ---------------------------------------------------------------------------------

   procedure Set_Model_Matrix (Model_Matrix : GL.Types.Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Model_Uniform, Model_Matrix);
   end Set_Model_Matrix;

   --   ---------------------------------------------------------------------------------

   procedure Set_Projection_Matrix (Projection_Matrix : GL.Types.Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Projection_Uniform, Projection_Matrix);
   end Set_Projection_Matrix;

   --   ---------------------------------------------------------------------------------

   procedure Set_Texture_Unit (Unit  : GL.Types.Int) is
   begin
      GL.Uniforms.Set_Int (Texture_Uniform, Unit);
   end Set_Texture_Unit;

   --   -----------------------------------------------------------------------

   procedure Set_View_Matrix (View_Matrix : GL.Types.Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (View_Uniform, View_Matrix);
   end Set_View_Matrix;

   --   ---------------------------------------------------------------------------------

   procedure Use_2D_Program is
   begin
      GL.Objects.Programs.Use_Program (UI_Program);
   end Use_2D_Program;

   --   ---------------------------------------------------------------------------------

end Shader_Manager_UI;
