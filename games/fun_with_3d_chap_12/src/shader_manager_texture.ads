
with GL.Objects.Programs;
with GL.Types;

package Shader_Manager_Texture is

   procedure Init_Shaders;
   function Program_Texture return GL.Objects.Programs.Program;
   procedure Set_Ambient_Light (Colour : GL.Types.Singles.Vector4);
   procedure Set_Texture_Unit (Unit  : GL.Types.Int);
   procedure Set_Model_Matrix (Model_Matrix : GL.Types.Singles.Matrix4);
   procedure Set_Projection_Matrix (Projection_Matrix : GL.Types.Singles.Matrix4);
   procedure Set_View_Matrix (View_Matrix  : GL.Types.Singles.Matrix4);
   procedure Use_Texture_Program;

end Shader_Manager_Texture;
