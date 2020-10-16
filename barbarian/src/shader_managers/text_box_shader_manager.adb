
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Uniforms;

with Program_Loader;

with Shader_Attributes;

package body Text_Box_Shader_Manager is

    type Text_Box_Uniforms is record
        Colour_ID        : GL.Uniforms.Uniform := 0;
        Position_ID      : GL.Uniforms.Uniform := 0;
        Scale_ID         : GL.Uniforms.Uniform := 0;
        Viewport_Dims_ID : GL.Uniforms.Uniform := 0;
    end record;

   Text_Box : Text_Box_Uniforms;

   procedure Init (Shader_Program : in out GL.Objects.Programs.Program) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use Program_Loader;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders_3_2/text_box.vert", Vertex_Shader),
         Src ("src/shaders_3_2/text_box.frag", Fragment_Shader)));

      Bind_Attrib_Location (Shader_Program, Shader_Attributes.Attrib_VP, "vp");

      Use_Program (Shader_Program);
      Text_Box.Colour_ID := Uniform_Location (Shader_Program, "colour");
      Text_Box.Position_ID := Uniform_Location (Shader_Program, "pos");
      Text_Box.Scale_ID := Uniform_Location (Shader_Program, "scale");
      Text_Box.Viewport_Dims_ID :=
          Uniform_Location (Shader_Program, "viewportdims");

   exception
      when others =>
         Put_Line ("An exception occurred in Text_Box_Shader_Manager.Init.");
         raise;
   end Init;

  --  -------------------------------------------------------------------------

   procedure Set_Colour_ID (Colour : GL.Types.Colors.Color) is
      use GL.Types.Colors;
      Col : Singles.Vector4;
   begin
      Col (GL.X) := Colour (R);
      Col (GL.Y) := Colour (G);
      Col (GL.Z) := Colour (B);
      Col (GL.W) := Colour (A);
      GL.Uniforms.Set_Single (Text_Box.Colour_ID, Col);
   end Set_Colour_ID;

   --  -------------------------------------------------------------------------

   procedure Set_Position_ID (Position : Singles.Vector2) is
   begin
      GL.Uniforms.Set_Single (Text_Box.Position_ID, Position);
   end Set_Position_ID;

   --  -------------------------------------------------------------------------

   procedure Set_Scale (Scale : Singles.Vector2) is
   begin
      GL.Uniforms.Set_Single (Text_Box.Scale_ID, Scale);
   end Set_Scale;

   --  -------------------------------------------------------------------------

    procedure Set_Viewport_Dimensions (Dimensions : Singles.Vector2) is
   begin
      GL.Uniforms.Set_Single (Text_Box.Viewport_Dims_ID, Dimensions);
   end Set_Viewport_Dimensions;

   --  -------------------------------------------------------------------------

end Text_Box_Shader_Manager;
