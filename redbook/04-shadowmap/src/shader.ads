
with GL.Objects.Programs;
with GL.Uniforms;

package Shader is

   type Light_Uniform_IDs is record
      MVP_Matrix_ID  : GL.Uniforms.Uniform := 0;
   end record;

   type Scene_Uniform_IDs is record
      View_Matrix_ID           : GL.Uniforms.Uniform := 0;
      Model_Matrix_ID          : GL.Uniforms.Uniform := 0;
      Projection_Matrix_ID     : GL.Uniforms.Uniform := 0;
      Shadow_Matrix_ID         : GL.Uniforms.Uniform := 0;
      Light_Position_Matrix_ID : GL.Uniforms.Uniform := 0;
      Ambient_Matrix_ID        : GL.Uniforms.Uniform := 0;
      Diffuse_Matrix_ID        : GL.Uniforms.Uniform := 0;
      Specular_Matrix_ID       : GL.Uniforms.Uniform := 0;
      Specular_Power_ID        : GL.Uniforms.Uniform := 0;
      Depth_Texture            : GL.Uniforms.Uniform := 0;
   end record;

   procedure Init (Light_Program, Scene_Program : in out GL.Objects.Programs.Program;
                   Light_Uniforms : out Light_Uniform_IDs;
                   Scene_Uniforms : out Scene_Uniform_IDs);
end Shader;
