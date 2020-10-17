
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with GL.Uniforms;

with Maths;
with Program_Loader;

with Game_Utils;
with Shader_Attributes;

package body Coins_Shader_Manager is
   use GL.Objects.Programs;

   type Coin_Uniform is record
      Perspective_ID          : GL.Uniforms.Uniform := 0;
      View_ID                 : GL.Uniforms.Uniform := 0;
      Model_ID                : GL.Uniforms.Uniform := 0;
      Ol_Pass_ID              : GL.Uniforms.Uniform := 0;
      Time_ID                 : GL.Uniforms.Uniform := 0;
      DM_ID                   : GL.Uniforms.Uniform := 0;
      Shadow_Enabled_ID       : GL.Uniforms.Uniform := 0;
      Cube_Texture_ID         : GL.Uniforms.Uniform := 0;
      Caster_Pos_World_ID     : GL.Uniforms.Uniform := 0;
   end record;

   Property_Uniforms : Coin_Uniform;

   --  -------------------------------------------------------------------------

   procedure Init_Coins_Shader
     (Coins_Shader : out GL.Objects.Programs.Program) is
      use GL.Objects.Shaders;
      use GL.Types.Singles;
      use Program_Loader;
   begin
      Coins_Shader := Program_From
        ((Src ("src/shaders_3_2/coins.vert", Vertex_Shader),
         Src ("src/shaders_3_2/coins.frag", Fragment_Shader)));

      Property_Uniforms.Perspective_ID := Uniform_Location (Coins_Shader, "P");
      Property_Uniforms.View_ID := Uniform_Location (Coins_Shader, "V");
      Property_Uniforms.Model_ID :=  Uniform_Location (Coins_Shader, "M");
      Property_Uniforms.DM_ID :=  Uniform_Location (Coins_Shader, "dm");
      Property_Uniforms.Ol_Pass_ID :=  Uniform_Location (Coins_Shader, "ol_pass");
      Property_Uniforms.Shadow_Enabled_ID :=
        Uniform_Location (Coins_Shader, "shadow_enabled");
      Property_Uniforms.Time_ID :=  Uniform_Location (Coins_Shader, "time");
      Property_Uniforms.Cube_Texture_ID :=
        Uniform_Location (Coins_Shader, "cube_texture");
      Property_Uniforms.Caster_Pos_World_ID :=
        Uniform_Location (Coins_Shader, "caster_pos_wor");

      Use_Program (Coins_Shader);
      GL.Uniforms.Set_Single (Property_Uniforms.Caster_Pos_World_ID, Maths.Vec3_0);
      GL.Uniforms.Set_Int (Property_Uniforms.Cube_Texture_ID, 1);
      GL.Uniforms.Set_Int (Property_Uniforms.DM_ID, 0);
      GL.Uniforms.Set_Single (Property_Uniforms.Model_ID, Identity4);
      GL.Uniforms.Set_Single (Property_Uniforms.Ol_Pass_ID, 0.0);
      GL.Uniforms.Set_Single (Property_Uniforms.Perspective_ID, Identity4);
      GL.Uniforms.Set_Single (Property_Uniforms.Shadow_Enabled_ID, 0.0);
      GL.Uniforms.Set_Single (Property_Uniforms.Time_ID, 0.0);
      GL.Uniforms.Set_Single (Property_Uniforms.View_ID, Identity4);

   exception
      when others =>
         Put_Line ("An exception occurred in Coins_Shader_Manager.Init_Prop_Skinned_Shader.");
         raise;
   end Init_Coins_Shader;

   --  -------------------------------------------------------------------------

   procedure Set_Caster_Pos_World (Position : Singles.Vector3) is
   begin
      GL.Uniforms.Set_Single (Property_Uniforms.Caster_Pos_World_ID, Position);
   end Set_Caster_Pos_World;

   --  -------------------------------------------------------------------------

   procedure Set_Cube_Texture (Texture : Int)  is
   begin
      GL.Uniforms.Set_Int (Property_Uniforms.Cube_Texture_ID, Texture);
   end Set_Cube_Texture;

   --  -------------------------------------------------------------------------

   procedure Set_DM (DM : Int) is
   begin
      GL.Uniforms.Set_Int (Property_Uniforms.DM_ID, DM);
   end Set_DM;

   --  -------------------------------------------------------------------------


   procedure Set_Model (Model_Matrix : Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Property_Uniforms.Model_ID, Model_Matrix);
   end Set_Model;

   --  -------------------------------------------------------------------------

   procedure Set_Perspective (Perspective_Matrix : Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Property_Uniforms.Perspective_ID,
                              Perspective_Matrix);
   end Set_Perspective;

   --  -------------------------------------------------------------------------

   procedure Set_Ol_Pass (Ol_Pass : Single) is
   begin
      GL.Uniforms.Set_Single (Property_Uniforms.Ol_Pass_ID, Ol_Pass);
   end Set_Ol_Pass;

   --  -------------------------------------------------------------------------

   procedure Set_Shadow_Enabled (Enable : Single) is
   begin
      GL.Uniforms.Set_Single (Property_Uniforms.Shadow_Enabled_ID, Enable);
   end Set_Shadow_Enabled;

   --  -------------------------------------------------------------------------

   procedure Set_View (View_Matrix : Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Property_Uniforms.View_ID, View_Matrix);
   end Set_View;

   --  -------------------------------------------------------------------------

end Coins_Shader_Manager;
