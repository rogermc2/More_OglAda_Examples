
with GL.Types; use GL.Types;
with GL.Objects.Programs;
with GL.Uniforms;

package Splats_Shader_Manager is

   type Shader_Uniforms is record
      Ambient_Colour_ID    : GL.Uniforms.Uniform := 0;
      Dyn_Light_Pos_ID     : GL.Uniforms.Uniform := 0;
      Dyn_Light_Diff_ID    : GL.Uniforms.Uniform := 0;
      Dyn_Light_Spec_ID    : GL.Uniforms.Uniform := 0;
      Dyn_Light_Range_ID   : GL.Uniforms.Uniform := 0;
      Model_Matrix_ID      : GL.Uniforms.Uniform := 0;
      Projection_Matrix_ID : GL.Uniforms.Uniform := 0;
      Texture_ID           : GL.Uniforms.Uniform := 0;
      View_Matrix_ID       : GL.Uniforms.Uniform := 0;
   end record;

   procedure Init (Shader_Program : in out GL.Objects.Programs.Program);
   procedure Set_Ambient_Colour (Ambient : Singles.Vector3);
   procedure Set_Dyn_Light_Pos (Pos : Singles.Vector3);
   procedure Set_Dyn_Light_Diff (Diff : Singles.Vector3);
   procedure Set_Dyn_Light_Spec (Spec : Singles.Vector3);
   procedure Set_Dyn_Light_Range (Light_Range  : Single);
   procedure Set_Model_Matrix (Model_Matrix : Singles.Matrix4);
   procedure Set_Projection_Matrix (Projection_Matrix : Singles.Matrix4);
   procedure Set_Texture (Texture : Int);
   procedure Set_View_Matrix (View_Matrix : Singles.Matrix4);

end Splats_Shader_Manager;
