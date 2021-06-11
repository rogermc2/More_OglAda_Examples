
with GL.Objects.Programs;
with GL.Types;

package Shader_Manager_UI is

   procedure Init_Shaders;
   function Program_UI return GL.Objects.Programs.Program;
   procedure Set_Texture (Texture  : GL.Types.UInt);
   procedure Set_Model_Matrix (Model_Matrix : GL.Types.Singles.Matrix4);
   procedure Set_Projection_Matrix (Projection_Matrix : GL.Types.Singles.Matrix4);
   procedure Set_View_Matrix (View_Matrix  : GL.Types.Singles.Matrix4);
   procedure Use_2D_Program;

end Shader_Manager_UI;
