
with GL.Objects.Programs;
with GL.Types; use GL.Types;
with GL.Uniforms;

with Ogldev_Lights_Common; use Ogldev_Lights_Common;

package Ogldev_Basic_Lighting is

   type Basic_Lighting_Technique is private;

   Max_Point_Lights : constant Int := 2;
   Max_Spot_Lights  : constant Int := 2;

   type Point_Light_Location_Array is private;
   type Spot_Light_Location_Array  is private;

   function Init (Lighting_Technique : in out Basic_Lighting_Technique)
                    return Boolean;

   function Lighting_Program (Technique : Basic_Lighting_Technique)
                              return GL.Objects.Programs.Program;

   procedure Set_Color_Texture_Unit (Technique : Basic_Lighting_Technique;
                   Texture_Unit : GL.Types.UInt);
   procedure Set_Directional_Light (Technique : Basic_Lighting_Technique;
                                    Light : Directional_Light);
   procedure Set_Point_Lights (Technique : Basic_Lighting_Technique;
                               Lights : Point_Light_Array);
   procedure Set_Spot_Lights (Technique : Basic_Lighting_Technique;
                              Spots : Spot_Light_Array);
   procedure Set_Spot_Light (Technique : Basic_Lighting_Technique;
                              Spot : Spot_Light);
   procedure Set_Eye_World_Pos (Technique : Basic_Lighting_Technique;
                                Eye_Position : Singles.Vector3);
   procedure Set_Mat_Specular_Intensity (Technique : Basic_Lighting_Technique;
                                         Intensity : Single);
   procedure Set_Mat_Specular_Power (Technique : Basic_Lighting_Technique;
                                     Power : UInt);
   procedure Set_World_Matrix (Technique : Basic_Lighting_Technique;
                               World_Inverse : Singles.Matrix4);
   procedure Set_WVP (Technique : Basic_Lighting_Technique;
                      WVP : Singles.Matrix4);

private

   type Direct_Light is record
      Colour            : GL.Uniforms.Uniform := 0;
      Ambient_Intensity : GL.Uniforms.Uniform := 0;
      Diffuse_Intensity : GL.Uniforms.Uniform := 0;
      Direction         : GL.Uniforms.Uniform := 0;
   end record;

   type Atten is record
      Atten_Constant    : GL.Uniforms.Uniform := 0;
      Linear            : GL.Uniforms.Uniform := 0;
      Exp               : GL.Uniforms.Uniform := 0;
   end record;

   type Point_Light_Locations is record
      Colour            : GL.Uniforms.Uniform := 0;
      Ambient_Intensity : GL.Uniforms.Uniform := 0;
      Diffuse_Intensity : GL.Uniforms.Uniform := 0;
      Position          : GL.Uniforms.Uniform := 0;
      Attenuation       : Atten;
   end record;

   type Spot_Light_Locations is record
      Colour            : GL.Uniforms.Uniform := 0;
      Ambient_Intensity : GL.Uniforms.Uniform := 0;
      Diffuse_Intensity : GL.Uniforms.Uniform := 0;
      Position          : GL.Uniforms.Uniform := 0;
      Direction         : GL.Uniforms.Uniform := 0;
      Cut_Off           : GL.Uniforms.Uniform := 0;
      Attenuation       : Atten;
   end record;

   type Basic_Lighting_Technique is record
      Lighting_Program                : GL.Objects.Programs.Program;
      WVP_Location                    : GL.Uniforms.Uniform := 0;
      World_Matrix_Location           : GL.Uniforms.Uniform := 0;
      Colour_Texture_Location         : GL.Uniforms.Uniform := 0;
      Eye_World_Pos_Location          : GL.Uniforms.Uniform := 0;
      Mat_Specular_Intensity_Location : GL.Uniforms.Uniform := 0;
      Mat_Specular_Power_Location     : GL.Uniforms.Uniform := 0;
      Num_Point_Lights_Location       : GL.Uniforms.Uniform := 0;
      Num_Spot_Lights_Location        : GL.Uniforms.Uniform := 0;
      Dir_Light_Location              : Direct_Light;
      Point_Lights_Location           : Point_Light_Location_Array;
      Spot_Lights_Location            : Spot_Light_Location_Array;
   end record;

   type Point_Light_Location_Array is array (1 .. Max_Point_Lights) of Point_Light_Locations;
   type Spot_Light_Location_Array  is array (1 .. Max_Spot_Lights) of Spot_Light_Locations;

end Ogldev_Basic_Lighting;
