
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Types;
with GL.Uniforms;

Package Shadow_Map_Technique is

   type Technique is private;

   function Display_Program (theTechnique : Technique)
                            return GL.Objects.Programs.Program;
   function Shadow_Program (theTechnique : Technique)
                            return GL.Objects.Programs.Program;
   procedure Init (theTechnique : out Technique);
   procedure Set_Shadow_Map_Texture_Units (theTechnique : Technique;
                                           Texture_Unit : GL.Types.Int);
   procedure Set_Display_WVP (theTechnique : Technique;
                              WVP          : GL.Types.Singles.Matrix4);
   procedure Set_Shadow_WVP (theTechnique : Technique;
                              WVP          : GL.Types.Singles.Matrix4);
   procedure Use_Display_Program (theTechnique : Technique);
   procedure Use_Shadow_Program (theTechnique : Technique);

private

  type Technique is record
      Display_Program       : GL.Objects.Programs.Program;
      Shadow_Program        : GL.Objects.Programs.Program;
      Display_WVP_Location  : GL.Uniforms.Uniform := 0;
      Shadow_WVP_Location   : GL.Uniforms.Uniform := 0;
      Display_Texture_Unit_Location : GL.Uniforms.Uniform := 0;
      Shadow_Texture_Unit_Location  : GL.Uniforms.Uniform := 0;
   end record;

end Shadow_Map_Technique;
