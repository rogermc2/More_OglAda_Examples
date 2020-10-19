
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with Program_Loader;

with Shader_Attributes;

package body Title_Shader_Manager is

   Render_Uniforms : Shader_Uniforms;

   procedure Init (Shader_Program : in out GL.Objects.Programs.Program) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use GL.Types.Singles;
      use Program_Loader;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders_3_2/title.vert", Vertex_Shader),
         Src ("src/shaders_3_2/title.frag", Fragment_Shader)));

      Render_Uniforms.Model_Matrix_ID := Uniform_Location (Shader_Program, "M");
      Render_Uniforms.Perspective_Matrix_ID :=
          Uniform_Location (Shader_Program, "P");
      Render_Uniforms.View_Matrix_ID := Uniform_Location (Shader_Program, "V");
      Render_Uniforms.Time_ID := Uniform_Location (Shader_Program, "time");

      Use_Program (Shader_Program);
      GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Identity4);
      GL.Uniforms.Set_Single (Render_Uniforms.Perspective_Matrix_ID, Identity4);
      GL.Uniforms.Set_Single (Render_Uniforms.View_Matrix_ID, Identity4);
      GL.Uniforms.Set_Single (Render_Uniforms.Time_ID, 0.0);

   exception
      when others =>
         Put_Line ("An exception occurred in Title_Shader_Manager.Init.");
         raise;
   end Init;

  --  -------------------------------------------------------------------------

   procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Model_Matrix);
   end Set_Model_Matrix;

   --  -------------------------------------------------------------------------

   procedure Set_Perspective_Matrix (Perspective_Matrix : Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single
          (Render_Uniforms.Perspective_Matrix_ID, Perspective_Matrix);
   end Set_Perspective_Matrix;

   --  -------------------------------------------------------------------------

   procedure Set_View_Matrix (View_Matrix : Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.View_Matrix_ID, View_Matrix);
   end Set_View_Matrix;

   --  -------------------------------------------------------------------------

   procedure Set_Time (Time : Single) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Time_ID, Time);
   end Set_Time;

   --  -------------------------------------------------------------------------

 end  Title_Shader_Manager;
