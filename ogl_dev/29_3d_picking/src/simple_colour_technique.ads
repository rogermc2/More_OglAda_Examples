
with Ada.Strings.Unbounded;

with GL.Objects.Programs;
with GL.Types;
with GL.Uniforms;

Package Simple_Colour_Technique is

   type Colour_Technique is private;

   function Get_WVP_Location (theTechnique : Colour_Technique)
                              return GL.Uniforms.Uniform;
   procedure Init (theTechnique : in out Colour_Technique);
   function Simple_Colour_Program  (theTechnique : Colour_Technique)
                                    return GL.Objects.Programs.Program;
   procedure Set_WVP (theTechnique : Colour_Technique;
                      WVP          : GL.Types.Singles.Matrix4);
   procedure Use_Program (theTechnique : Colour_Technique);

private
   use GL.Uniforms;
   type Colour_Technique is record
      Colour_Program : GL.Objects.Programs.Program;
      WVP_Location   : GL.Uniforms.Uniform := -1;
   end record;

end Simple_Colour_Technique;
