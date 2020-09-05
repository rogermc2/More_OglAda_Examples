
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with Program_Loader;

with Shader_Attributes;

package body Text_Shader_Manager is

   Text_Uniforms : Shader_Uniforms;

   procedure Init (Shader_Program : in out GL.Objects.Programs.Program) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use Program_Loader;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders_3_2/text.vert", Vertex_Shader),
         Src ("src/shaders_3_2/text.frag", Fragment_Shader)));

      Bind_Attrib_Location (Shader_Program, Shader_Attributes.Attrib_VP, "vp");
      Bind_Attrib_Location (Shader_Program, Shader_Attributes.Attrib_VT, "vt");

      Text_Uniforms.Position_ID :=
        Uniform_Location (Shader_Program, "pos");
      Text_Uniforms.Text_Colour_ID :=
          Uniform_Location (Shader_Program, "text_colour");
      Text_Uniforms.Texture_ID := Uniform_Location (Shader_Program, "tex");

   exception
      when others =>
         Put_Line ("An exception occurred in Text_Shader_Manager.Init.");
         raise;
   end Init;

  --  -------------------------------------------------------------------------

   procedure Set_Text_Colour_ID (Colour : Singles.Vector4) is
   begin
      GL.Uniforms.Set_Single (Text_Uniforms.Position_ID, Colour);
   end Set_Text_Colour_ID;

   --  -------------------------------------------------------------------------

   procedure Set_Position_ID (Position : Singles.Vector2) is
   begin
      GL.Uniforms.Set_Single (Text_Uniforms.Position_ID, Position);
   end Set_Position_ID;

   --  -------------------------------------------------------------------------

   procedure Set_Texture_Unit (Texture_Unit : Int) is
   begin
      GL.Uniforms.Set_Int (Text_Uniforms.Texture_ID, Texture_Unit);
   end Set_Texture_Unit;

   --  -------------------------------------------------------------------------

end Text_Shader_Manager;
