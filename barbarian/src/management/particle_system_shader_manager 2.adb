
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Shaders;
with Program_Loader;

with Maths;

--  with Shader_Attributes;

package body Particle_System_Shader_Manager is

   Particle_Uniforms : Basic_Particle_Uniforms;

   procedure Init (Shader_Program : in out GL.Objects.Programs.Program) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use GL.Types.Singles;
      use Program_Loader;
   begin
      Shader_Program := Program_From
        ((Src ("src/shaders_3_2/basic_particle.vert", Vertex_Shader),
         Src ("src/shaders_3_2/basic_particle.frag", Fragment_Shader)));

--        Bind_Attrib_Location (Shader_Program, Shader_Attributes.Attrib_WP,
--                              "particle_world_pos");
--        Bind_Attrib_Location (Shader_Program, Shader_Attributes.Attrib_Age,
--                              "particle_age");

      Particle_Uniforms.Degrees_ID := Uniform_Location (Shader_Program, "degs");
      Particle_Uniforms.Final_Colour_ID :=
          Uniform_Location (Shader_Program, "final_colour");
      Particle_Uniforms.Final_Scale_ID :=
        Uniform_Location (Shader_Program, "final_scale");
      Particle_Uniforms.Initial_Colour_ID :=
          Uniform_Location (Shader_Program, "initial_colour");
      Particle_Uniforms.Initial_Scale_ID :=
          Uniform_Location (Shader_Program, "fiinitial_scalenal_colour");
      Particle_Uniforms.Lifetime_ID :=
          Uniform_Location (Shader_Program, "lifetime");
      Particle_Uniforms.Perspective_View_ID :=
          Uniform_Location (Shader_Program, "PV");
      Particle_Uniforms.Pixel_Width_ID :=
          Uniform_Location (Shader_Program, "width_px");
      Particle_Uniforms.Texture_Map_ID :=
          Uniform_Location (Shader_Program, "texture_map");

      Use_Program (Shader_Program);
      GL.Uniforms.Set_Single (Particle_Uniforms.Degrees_ID, 0.0);
      GL.Uniforms.Set_Single (Particle_Uniforms.Final_Colour_ID, Maths.Vec4_0);
      GL.Uniforms.Set_Single (Particle_Uniforms.Final_Scale_ID, 0.0);
      GL.Uniforms.Set_Single (Particle_Uniforms.Initial_Colour_ID, Maths.Vec4_0);
      GL.Uniforms.Set_Single (Particle_Uniforms.Initial_Scale_ID, 0.0);
      GL.Uniforms.Set_Single (Particle_Uniforms.Lifetime_ID, 0.0);
      GL.Uniforms.Set_Single (Particle_Uniforms.Perspective_View_ID, Identity4);
      GL.Uniforms.Set_Single (Particle_Uniforms.Pixel_Width_ID, 0.0);
      GL.Uniforms.Set_UInt (Particle_Uniforms.Texture_Map_ID, 0);

   exception
      when others =>
         Put_Line ("An exception occurred in Particle_System_Shader_Manager.Init.");
         raise;
   end Init;

  --  -------------------------------------------------------------------------

   procedure Set_Degrees (Degrees : Single) is
   begin
      GL.Uniforms.Set_Single (Particle_Uniforms.Degrees_ID, Degrees);
   end Set_Degrees;

   --  -------------------------------------------------------------------------

   procedure Set_Final_Colour (Colour : Singles.Vector4) is
   begin
      GL.Uniforms.Set_Single (Particle_Uniforms.Final_Colour_ID, Colour);
   end Set_Final_Colour;

   --  -------------------------------------------------------------------------

   procedure Set_Final_Scale (Scale : Single)  is
   begin
      GL.Uniforms.Set_Single (Particle_Uniforms.Final_Scale_ID, Scale);
   end Set_Final_Scale;

   --  -------------------------------------------------------------------------

   procedure Set_Initial_Colour (Colour : Singles.Vector4) is
   begin
      GL.Uniforms.Set_Single (Particle_Uniforms.Initial_Colour_ID, Colour);
   end Set_Initial_Colour;

   --  -------------------------------------------------------------------------

   procedure Set_Initial_Scale (Scale  : Single) is
   begin
      GL.Uniforms.Set_Single (Particle_Uniforms.Initial_Scale_ID, Scale);
   end Set_Initial_Scale;

   --  -------------------------------------------------------------------------

   procedure Set_Lifetime (Lifetime : Single) is
   begin
      GL.Uniforms.Set_Single (Particle_Uniforms.Lifetime_ID, Lifetime);
   end Set_Lifetime;

   --  -------------------------------------------------------------------------
   procedure Set_Perspective_View (PV : Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Particle_Uniforms.Perspective_View_ID, PV);
   end Set_Perspective_View;

   --  -------------------------------------------------------------------------

   procedure Set_Pixel_Width (Width : Single) is
   begin
      GL.Uniforms.Set_Single (Particle_Uniforms.Pixel_Width_ID, Width);
   end Set_Pixel_Width;

   --  -------------------------------------------------------------------------

   procedure Set_Texture_Map (Map : UInt) is
   begin
      GL.Uniforms.Set_UInt (Particle_Uniforms.Texture_Map_ID, Map);
   end Set_Texture_Map;

   --  -------------------------------------------------------------------------

end Particle_System_Shader_Manager;
