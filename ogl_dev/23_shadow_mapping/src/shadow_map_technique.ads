
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Types;
with GL.Uniforms;

Package Shadow_Map_Technique is

   type Technique is private;

   function Light_Program (theTechnique : Technique)
                            return GL.Objects.Programs.Program;
   procedure Init (theTechnique : out Technique);
   procedure Set_Shadow_Map_Texture_Unit (theTechnique : Technique;
                                          Texture_Unit : GL.Types.Int);
   procedure Set_WVP (theTechnique : Technique;
                      WVP          : GL.Types.Singles.Matrix4);
   procedure Use_Program (theTechnique : Technique);

private

  type Technique is record
      Lighting_Program   : GL.Objects.Programs.Program;
      WVP_Location       : GL.Uniforms.Uniform := 0;
      Texture_Location   : GL.Uniforms.Uniform := 0;
   end record;

end Shadow_Map_Technique;
