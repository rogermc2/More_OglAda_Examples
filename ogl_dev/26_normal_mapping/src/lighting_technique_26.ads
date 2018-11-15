
with GL.Objects.Programs;
with GL.Types;
with GL.Uniforms;

Package Lighting_Technique_26 is

   Max_Point_Lights : constant GL.Types.UInt := 2;
   Max_Spot_Lights  : constant GL.Types.UInt := 2;

   type Technique is private;

   function Light_Program (theTechnique : Technique)
                               return GL.Objects.Programs.Program;
   procedure Init (theTechnique : out Technique);
   procedure Set_Billboard_Size (theTechnique : Technique;
                                 Size : GL.Types.Single);
   procedure Set_Light_WVP_Position (theTechnique : Technique;
                                  Position : GL.Types.Singles.Vector3);
   procedure Set_Colour_Texture_Unit (theTechnique : Technique;
                                      Texture_Unit : GL.Types.Int);
   procedure Set_Shadow_Map_Texture_Unit (theTechnique : Technique;
                                      Texture_Unit : GL.Types.Int);
   procedure Set_Normal_Map_Texture_Unit (theTechnique : Technique;
                                      Texture_Unit : GL.Types.Int);
   procedure Set_View_Point (theTechnique : Technique;
                             View_Point : GL.Types.Singles.Matrix4);
   procedure Use_Program (theTechnique : Technique);
   procedure Set_World_Matrix (theTechnique : Technique;
                               WVP : GL.Types.Singles.Matrix4);
   procedure Set_World_View_Point (theTechnique : Technique;
                                   WVP   : GL.Types.Singles.Matrix4);

private

   type Atten_Location is record
      Constant_Atten : GL.Uniforms.Uniform := 0;
      Linear         : GL.Uniforms.Uniform := 0;
      Exp            : GL.Uniforms.Uniform := 0;
   end record;

   type Point_Lights_Location is record
      Color             : GL.Uniforms.Uniform := 0;
      Ambient_Intensity : GL.Uniforms.Uniform := 0;
      Diffuse_Intensity : GL.Uniforms.Uniform := 0;
      Position          : GL.Uniforms.Uniform := 0;
      Atten             : Atten_Location;
   end record;

   type Spot_Lights_Location is record
      Color             : GL.Uniforms.Uniform := 0;
      Ambient_Intensity : GL.Uniforms.Uniform := 0;
      Diffuse_Intensity : GL.Uniforms.Uniform := 0;
      Position          : GL.Uniforms.Uniform := 0;
      Direction         : GL.Uniforms.Uniform := 0;
      Cutoff            : GL.Uniforms.Uniform := 0;
      Atten             : Atten_Location;
   end record;

   type Direct_Light_Location is record
      Color             : GL.Uniforms.Uniform := 0;
      Ambient_Intensity : GL.Uniforms.Uniform := 0;
      Diffuse_Intensity : GL.Uniforms.Uniform := 0;
      Direction         : GL.Uniforms.Uniform := 0;
   end record;

   type Technique is record
      Lighting_Program                : GL.Objects.Programs.Program;
      WVP_Location                    : GL.Uniforms.Uniform := 0;
      World_Matrix_Location           : GL.Uniforms.Uniform := 0;
      Colour_Map_Location             : GL.Uniforms.Uniform := 0;
      Shadow_Map_Location             : GL.Uniforms.Uniform := 0;
      Normal_Map_Location             : GL.Uniforms.Uniform := 0;
      Eye_World_Pos_Location          : GL.Uniforms.Uniform := 0;
      Mat_Specular_Intensity_Location : GL.Uniforms.Uniform := 0;
      Mat_Specular_Power_Location     : GL.Uniforms.Uniform := 0;
      Num_Point_Lights_Location       : GL.Uniforms.Uniform := 0;
      Num_Spot_Lights_Location        : GL.Uniforms.Uniform := 0;
   end record;

end Lighting_Technique_26;
