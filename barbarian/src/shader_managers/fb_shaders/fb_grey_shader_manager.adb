
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with Program_Loader;

with Game_Utils;

package body FB_Grey_Shader_Manager is

   Render_Uniforms : Shader_Uniforms;

   procedure Init (Shader_Program : in out GL.Objects.Programs.Program) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use GL.Types.Singles;
      use Program_Loader;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders_3_2/fb_grey.vert", Vertex_Shader),
         Src ("src/shaders_3_2/fb_grey.frag", Fragment_Shader)));

      Render_Uniforms.Tex_ID := Uniform_Location (Shader_Program, "Tex");

      Use_Program (Shader_Program);
      GL.Uniforms.Set_Int (Render_Uniforms.Tex_ID, 0);

   exception
      when others =>
         Put_Line ("An exception occurred in FB_Grey_Shader_Manager.Init.");
         raise;
   end Init;

  --  -------------------------------------------------------------------------

   procedure Set_Tex (Tex : Int) is
   begin
      GL.Uniforms.Set_Int (Render_Uniforms.Tex_ID, Tex);
   end Set_Tex;

   --  -------------------------------------------------------------------------

end FB_Grey_Shader_Manager;
