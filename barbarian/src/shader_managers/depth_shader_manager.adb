
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Uniforms;

with Maths;

with Program_Loader;
with Shader_Attributes;

package body Depth_Shader_Manager is

    type Shader_Uniforms is record
        Light_Position_ID  : GL.Uniforms.Uniform := 0;
        Model_Matrix_ID    : GL.Uniforms.Uniform := 0;
        Projection_ID      : GL.Uniforms.Uniform := 0;
        View_ID            : GL.Uniforms.Uniform := 0;
    end record;

   Render_Uniforms : Shader_Uniforms;

   procedure Init (Shader_Program : in out GL.Objects.Programs.Program) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use GL.Types.Singles;
      use Program_Loader;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders_3_2/depth.vert", Vertex_Shader),
         Src ("src/shaders_3_2/depth.frag", Fragment_Shader)));

      Render_Uniforms.Light_Position_ID :=
          Uniform_Location (Shader_Program, "light_pos_wor");
      Render_Uniforms.Model_Matrix_ID :=
        Uniform_Location (Shader_Program, "M");
      Render_Uniforms.Projection_ID :=
          Uniform_Location (Shader_Program, "P");
      Render_Uniforms.View_ID :=
          Uniform_Location (Shader_Program, "V");

      Use_Program (Shader_Program);
      GL.Uniforms.Set_Single (Render_Uniforms.Light_Position_ID, Maths.Vec3_0);
      GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Identity4);
      GL.Uniforms.Set_Single (Render_Uniforms.Projection_ID, Identity4);
      GL.Uniforms.Set_Single (Render_Uniforms.View_ID, Identity4);

   exception
      when others =>
         Put_Line ("An exception occurred in Health_Shader_Manager.Init.");
         raise;
   end Init;

  --  -------------------------------------------------------------------------

   procedure Set_Light_Position (Light_Position : Singles.Vector3) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Light_Position_ID, Light_Position);
   end Set_Light_Position;

   --  -------------------------------------------------------------------------

   procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Model_Matrix);
   end Set_Model_Matrix;

   --  -------------------------------------------------------------------------

   procedure Set_Projection_Matrix (Projection_Matrix : Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Projection_ID, Projection_Matrix);
   end Set_Projection_Matrix;

   --  -------------------------------------------------------------------------

   procedure Set_View_Matrix (View_Matrix : Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.View_ID, View_Matrix);
   end Set_View_Matrix;

   --  -------------------------------------------------------------------------

end Depth_Shader_Manager;
