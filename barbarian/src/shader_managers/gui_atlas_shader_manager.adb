
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with Program_Loader;

with Game_Utils;
with Shader_Attributes;

package body GUI_Atlas_Shader_Manager is

   Render_Uniforms : Shader_Uniforms;

   procedure Init (Shader_Program : in out GL.Objects.Programs.Program) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use GL.Types.Singles;
      use Program_Loader;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders_3_2/gui_atlas.vert", Vertex_Shader),
         Src ("src/shaders_3_2/gui_atlas.frag", Fragment_Shader)));

      Bind_Attrib_Location (Shader_Program, Shader_Attributes.Attrib_VP, "vp");

      Render_Uniforms.Alpha_ID := Uniform_Location (Shader_Program, "alpha");
      Render_Uniforms.Atlas_ID := Uniform_Location (Shader_Program, "atlas");
      Render_Uniforms.Model_Matrix_ID :=
        Uniform_Location (Shader_Program, "model_mat");
      Render_Uniforms.Columns_ID := Uniform_Location (Shader_Program, "columns");
      Render_Uniforms.Current_Sprite_ID :=
        Uniform_Location (Shader_Program, "current_sprite");

      Use_Program (Shader_Program);
      GL.Uniforms.Set_Single (Render_Uniforms.Alpha_ID, 0.0);
      GL.Uniforms.Set_Int (Render_Uniforms.Atlas_ID, 0);
      GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Identity4);
      GL.Uniforms.Set_Single (Render_Uniforms.Columns_ID, 0.0);
      GL.Uniforms.Set_Single (Render_Uniforms.Current_Sprite_ID, 0.0);

   exception
      when others =>
         Put_Line ("An exception occurred in GUI_Atlas_Shader_Manager.Init.");
         raise;
   end Init;

  --  -------------------------------------------------------------------------

   procedure Set_Alpha (Alpha  : Single) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Alpha_ID, Alpha);
   end Set_Alpha;

   --  -------------------------------------------------------------------------

   procedure Set_Atlas (Atlas : Int) is
   begin
      GL.Uniforms.Set_Int (Render_Uniforms.Atlas_ID, Atlas);
   end Set_Atlas;

   --  -------------------------------------------------------------------------

   procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Model_Matrix_ID, Model_Matrix);
   end Set_Model_Matrix;

   --  -------------------------------------------------------------------------

   procedure Set_Columns (Columns  : Single) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Columns_ID, Columns);
   end Set_Columns;

   --  -------------------------------------------------------------------------

   procedure Set_Current_Sprite (Current_Sprite  : Single) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Current_Sprite_ID, Current_Sprite);
   end Set_Current_Sprite;

   --  -------------------------------------------------------------------------

end GUI_Atlas_Shader_Manager;
