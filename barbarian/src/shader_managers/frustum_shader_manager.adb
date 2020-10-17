
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with Program_Loader;

with Maths;

with Shader_Attributes;

package body Frustum_Shader_Manager is

   Render_Uniforms : Shader_Uniforms;

   procedure Init (Shader_Program : in out GL.Objects.Programs.Program) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use GL.Types.Singles;
      use Program_Loader;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders_3_2/frustum_debug.vert", Vertex_Shader),
         Src ("src/shaders_3_2/frustum_debug.frag", Fragment_Shader)));

      Render_Uniforms.Model_Matrix_ID := Uniform_Location (Shader_Program, "PV");
      Render_Uniforms.Colour_ID := Uniform_Location (Shader_Program, "colour");

      Use_Program (Shader_Program);
      GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Identity4);
      GL.Uniforms.Set_Single (Render_Uniforms.Colour_ID, Maths.Vec4_0);

   exception
      when others =>
         Put_Line ("An exception occurred in Cursor_Shader_Manager.Init.");
         raise;
   end Init;

  --  -------------------------------------------------------------------------

   procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Model_Matrix);
   end Set_Model_Matrix;

   --  -------------------------------------------------------------------------

   procedure Set_Colour (Colour : Singles.Vector4) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Colour_ID, Colour);
   end Set_Colour;

   --  -------------------------------------------------------------------------

end Frustum_Shader_Manager;
