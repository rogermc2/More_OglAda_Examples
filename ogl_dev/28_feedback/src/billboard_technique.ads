
with GL.Objects.Programs;
with GL.Types;
with GL.Uniforms;

Package Billboard_Technique is

   type Technique is private;

   function Active_Attributes (theTechnique : Technique) return GL.Types.Size;
   function Billboard_Program (theTechnique : Technique)
                               return GL.Objects.Programs.Program;
   procedure Init (theTechnique : out Technique);
   procedure Set_Billboard_Size (theTechnique : Technique;
                                 Size : GL.Types.Single);
   procedure Set_Camera_Position (theTechnique : Technique;
                                  Position : GL.Types.Singles.Vector3);
   procedure Set_Colour_Texture_Unit (theTechnique : Technique;
                                      Texture_Unit : GL.Types.Int);
   procedure Set_View_Point (theTechnique : Technique;
                             View_Point : GL.Types.Singles.Matrix4);
   procedure Use_Program (theTechnique : Technique);

private

   type Technique is record
      Program                  : GL.Objects.Programs.Program;
      View_Point_Location      : GL.Uniforms.Uniform;
      Camera_Position_Location : GL.Uniforms.Uniform;
      Colour_Map_Location      : GL.Uniforms.Uniform;
      Billboard_Size_Location  : GL.Uniforms.Uniform;
   end record;

end Billboard_Technique;
