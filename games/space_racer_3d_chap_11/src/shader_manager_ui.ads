
with GL.Types;

package Shader_Manager_UI is

   procedure Init_Shaders;
   procedure Set_Texture (Texture  : GL.Types.UInt);
   procedure Set_Model_Matrix (Model_Matrix : GL.Types.Singles.Matrix4);
   procedure Set_Projection_Matrix (Projection_Matrix : GL.Types.Singles.Matrix4);
   procedure Set_View_Matrix (View_Matrix  : GL.Types.Singles.Matrix4);

end Shader_Manager_UI;
