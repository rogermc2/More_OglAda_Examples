
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with Maths;
with Program_Loader;

with Shader_Attributes;

package body Menu_Credits_Shader_Manager is

   Render_Uniforms : Shader_Uniforms;

   procedure Init (Shader_Program : in out GL.Objects.Programs.Program) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use GL.Types.Singles;
      use Program_Loader;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders_3_2/mmenu_credits.vert", Vertex_Shader),
         Src ("src/shaders_3_2/mmenu_credits.frag", Fragment_Shader)));

      Render_Uniforms.Position_ID := Uniform_Location (Shader_Program, "pos");
      Render_Uniforms.Scale_ID := Uniform_Location (Shader_Program, "scale");
      Render_Uniforms.Texture_ID := Uniform_Location (Shader_Program, "tex");

      Use_Program (Shader_Program);
      GL.Uniforms.Set_Single (Render_Uniforms.Position_ID, Maths.Vec2_0);
      GL.Uniforms.Set_Single (Render_Uniforms.Scale_ID,  Maths.Vec2_0);
      GL.Uniforms.Set_Int (Render_Uniforms.Texture_ID, 0);

   exception
      when others =>
         Put_Line ("An exception occurred in Title_Shader_Manager.Init.");
         raise;
   end Init;

  --  -------------------------------------------------------------------------

    procedure Set_Position (Position : Singles.Vector2) is
   begin
      GL.Uniforms.Set_Single (Render_Uniforms.Position_ID, Position);
   end Set_Position;

   --  -------------------------------------------------------------------------

    procedure Set_Scale (Scale : Singles.Vector2) is
   begin
      GL.Uniforms.Set_Single
          (Render_Uniforms.Scale_ID, Scale);
   end Set_Scale;

   --  -------------------------------------------------------------------------

    procedure Set_Texture (Tex : Int) is
   begin
      GL.Uniforms.Set_Int (Render_Uniforms.Texture_ID, Tex);
   end Set_Texture;

   --  -------------------------------------------------------------------------

 end  Menu_Credits_Shader_Manager;
