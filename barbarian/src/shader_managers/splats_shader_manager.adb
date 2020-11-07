
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with Program_Loader;

with Maths;
with Shader_Attributes;

package body Splats_Shader_Manager is

   Render_Uniforms : Shader_Uniforms;

   procedure Init (Shader_Program : in out GL.Objects.Programs.Program) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use GL.Types.Singles;
      use Maths;
      use Program_Loader;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders_3_2/splats.vert", Vertex_Shader),
         Src ("src/shaders_3_2/splats.frag", Fragment_Shader)));

      Render_Uniforms.Ambient_Light_ID :=
          Uniform_Location (Shader_Program, "L_a");
      Render_Uniforms.Caster_Pos_ID :=
          Uniform_Location (Shader_Program, "caster_pos_wor");
      Render_Uniforms.Cube_Texture_ID :=
          Uniform_Location (Shader_Program, "cube_texture");
      Render_Uniforms.Dyn_Light_Pos_ID :=
          Uniform_Location (Shader_Program, "dyn_light_pos_wor");
      Render_Uniforms.Dyn_Light_Diff_ID :=
          Uniform_Location (Shader_Program, "dyn_light_diff");
      Render_Uniforms.Dyn_Light_Spec_ID :=
          Uniform_Location (Shader_Program, "dyn_light_spec");
      Render_Uniforms.Dyn_Light_Range_ID :=
          Uniform_Location (Shader_Program, "dyn_light_range");
      Render_Uniforms.Projection_Matrix_ID :=
        Uniform_Location (Shader_Program, "P");
      Render_Uniforms.Shadow_Enabled_ID :=
        Uniform_Location (Shader_Program, "shadow_enabled");
      Render_Uniforms.Texture_ID :=
          Uniform_Location (Shader_Program, "tex");
      Render_Uniforms.View_Matrix_ID :=
        Uniform_Location (Shader_Program, "V");

      Use_Program (Shader_Program);
      GL.Uniforms.Set_Single (Render_Uniforms.Ambient_Light_ID, Vec3_0);
      GL.Uniforms.Set_Int (Render_Uniforms.Cube_Texture_ID, 0);
      GL.Uniforms.Set_Single (Render_Uniforms.Dyn_Light_Pos_ID, Vec3_0);
      GL.Uniforms.Set_Single (Render_Uniforms.Dyn_Light_Diff_ID, Vec3_0);
      GL.Uniforms.Set_Single (Render_Uniforms.Dyn_Light_Spec_ID, Vec3_0);
      GL.Uniforms.Set_Single (Render_Uniforms.Dyn_Light_Range_ID, 0.0);
      GL.Uniforms.Set_Single (Render_Uniforms.Projection_Matrix_ID, Identity4);
      GL.Uniforms.Set_Single (Render_Uniforms.Shadow_Enabled_ID, 0.0);
      GL.Uniforms.Set_Int (Render_Uniforms.Texture_ID, 0);
      GL.Uniforms.Set_Single (Render_Uniforms.View_Matrix_ID, Identity4);

   exception
      when others =>
         Put_Line ("An exception occurred in Splats_Shader_Manager.Init.");
         raise;
   end Init;

  --  -------------------------------------------------------------------------

   procedure Set_Ambient_Light (Ambient : Singles.Vector3) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Ambient_Light_ID, Ambient);
   end Set_Ambient_Light;

   --  -------------------------------------------------------------------------

   procedure Set_Caster_Position (Pos : Singles.Vector3) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Caster_Pos_ID, Pos);
   end Set_Caster_Position;

   --  -------------------------------------------------------------------------

   procedure Set_Cube_Texture (Texture : Int) is
   begin
      GL.Uniforms.Set_Int (Render_Uniforms.Cube_Texture_ID, Texture);
   end Set_Cube_Texture;

   --  -------------------------------------------------------------------------

   procedure Set_Dyn_Light_Pos (Pos : Singles.Vector3) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Dyn_Light_Pos_ID, Pos);
   end Set_Dyn_Light_Pos;

   --  -------------------------------------------------------------------------

   procedure Set_Dyn_Light_Diff (Diff : Singles.Vector3) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Dyn_Light_Diff_ID, Diff);
   end Set_Dyn_Light_Diff;

   --  -------------------------------------------------------------------------

   procedure Set_Dyn_Light_Spec (Spec : Singles.Vector3) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Dyn_Light_Spec_ID, Spec);
   end Set_Dyn_Light_Spec;

   --  -------------------------------------------------------------------------

   procedure Set_Dyn_Light_Range (Light_Range  : Single) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Dyn_Light_Range_ID, Light_Range);
   end Set_Dyn_Light_Range;

   --  -------------------------------------------------------------------------

   procedure Set_Projection_Matrix (Projection_Matrix : Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Projection_Matrix_ID, Projection_Matrix);
   end Set_Projection_Matrix;

   --  -------------------------------------------------------------------------

   procedure Set_Shadow_Enabled (Enabled : Single) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Shadow_Enabled_ID, Enabled);
   end Set_Shadow_Enabled;

   --  -------------------------------------------------------------------------

   procedure Set_Texture (Texture : Int) is
   begin
      GL.Uniforms.Set_Int (Render_Uniforms.Texture_ID, Texture);
   end Set_Texture;

   --  -------------------------------------------------------------------------

   procedure Set_View_Matrix (View_Matrix : Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Projection_Matrix_ID, View_Matrix);
   end Set_View_Matrix;

   --  -------------------------------------------------------------------------

end Splats_Shader_Manager;
