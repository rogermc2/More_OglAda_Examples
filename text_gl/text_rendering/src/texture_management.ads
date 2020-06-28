
with GL.Objects.Programs;
with GL.Text;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

package Texture_Management is
   procedure Initialize;
   function Initialize_Font_Data (Font_File : String)
                                  return GL.Text.Renderer_Reference;
   procedure Render_Text (Render_Program : GL.Objects.Programs.Program;
                          Text   : String; X, Y, Scale : GL.Types.Single;
                          Colour : GL.Types.Colors.Basic_Color;
                          Texture_ID, Projection_Matrix_ID : GL.Uniforms.Uniform;
                          Colour_ID : GL.Uniforms.Uniform;
                          Projection_Matrix : GL.Types.Singles.Matrix4);
end Texture_Management;
