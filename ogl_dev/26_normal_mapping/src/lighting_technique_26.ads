
with Maths;

with GL.Objects.Programs;
with GL.Types;
with GL.Uniforms;

Package Lighting_Technique_26 is

   Max_Point_Lights : constant GL.Types.UInt := 2;
   Max_Spot_Lights  : constant GL.Types.UInt := 2;

   type Direct_Light is private;
   type Technique is private;
   type Point_Lights_Array is private;
   type Spot_Lights_Array is private;

   function Light_Program (theTechnique : Technique)
                            return GL.Objects.Programs.Program;
   function Init (theTechnique : out Technique) return Boolean;

   procedure Init_Directional_Light (Light : out Direct_Light);
   procedure Set_Colour_Texture_Unit (theTechnique : in out Technique;
                                      Texture_Unit : GL.Types.Int);
   procedure Set_Directional_Light (theTechnique : in out Technique;
                                    Light        : Direct_Light);
   procedure Set_Eye_World_Position (theTechnique : in out Technique;
                                     Position     : GL.Types.Singles.Vector3);
   procedure Set_Light_WVP_Position (theTechnique : in out Technique;
                                     Position     : GL.Types.Singles.Vector3);
   procedure Set_Mat_Specular_Intensity (theTechnique : in out Technique;
                                         Intensity    : GL.Types.Single);
   procedure Set_Mat_Specular_Power (theTechnique : in out Technique;
                                     Power        : GL.Types.Single);
   procedure Set_Normal_Map_Texture_Unit (theTechnique : in out Technique;
                                          Texture_Unit : GL.Types.Int);
   procedure Set_Point_Lights (theTechnique : in out Technique;
                               Lights       : Point_Lights_Array);
   procedure Set_Shadow_Map_Texture_Unit (theTechnique : in out Technique;
                                          Texture_Unit : GL.Types.Int);
   procedure Set_Spot_Lights (theTechnique : in out Technique;
                              Lights       : Spot_Lights_Array);
   procedure Set_World_Matrix (theTechnique : in out Technique;
                               WVP          : GL.Types.Singles.Matrix4);
   procedure Set_WVP (theTechnique : in out Technique;
                      WVP          : GL.Types.Singles.Matrix4);
   procedure Use_Program (theTechnique : Technique);

private

   type Attenuation is record
      Constant_Atten : GL.Types.Single := 0.0;
      Linear         : GL.Types.Single := 0.0;
      Exp            : GL.Types.Single := 0.0;
   end record;

   type Base_Light is record
      Colour            : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
      Ambient_Intensity : GL.Types.Single := 0.0;
      Diffuse_Intensity : GL.Types.Single := 0.0;
   end record;

   type Direct_Light is record
      Base      : Base_Light;
      Direction : GL.Types.Singles.Vector3 := (1.0, 0.0, 0.0);
   end record;

   type Point_Light is record
      Base     : Base_Light;
      Position : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
      Atten    : Attenuation;
   end record;

   type Spot_Light is record
      Base      : Point_Light;
      Direction : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
      Cutoff    : Maths.Degree := 0.0;
   end record;

   type Atten_Location is record
      Constant_Atten : GL.Uniforms.Uniform := 0;
      Linear         : GL.Uniforms.Uniform := 0;
      Exp            : GL.Uniforms.Uniform := 0;
   end record;

   type Point_Light_Locations is record
      Color             : GL.Uniforms.Uniform := 0;
      Ambient_Intensity : GL.Uniforms.Uniform := 0;
      Diffuse_Intensity : GL.Uniforms.Uniform := 0;
      Position          : GL.Uniforms.Uniform := 0;
      Atten             : Atten_Location;
   end record;

   type Spot_Light_Locations is record
      Color             : GL.Uniforms.Uniform := 0;
      Ambient_Intensity : GL.Uniforms.Uniform := 0;
      Diffuse_Intensity : GL.Uniforms.Uniform := 0;
      Position          : GL.Uniforms.Uniform := 0;
      Direction         : GL.Uniforms.Uniform := 0;
      Cutoff            : GL.Uniforms.Uniform := 0;
      Atten             : Atten_Location;
   end record;

   type Direct_Light_Locations is record
      Color             : GL.Uniforms.Uniform := 0;
      Ambient_Intensity : GL.Uniforms.Uniform := 0;
      Diffuse_Intensity : GL.Uniforms.Uniform := 0;
      Direction         : GL.Uniforms.Uniform := 0;
   end record;

   type Point_Lights_Array is array (1 .. Max_Point_Lights) of
     Point_Light;
   type Spot_Lights_Array is array (1 .. Max_Spot_Lights) of
     Spot_Light;

   type Point_Lights_Location_Array is array (1 .. Max_Point_Lights) of
     Point_Light_Locations;
   type Spot_Lights_Location_Array is array (1 .. Max_Spot_Lights) of
     Spot_Light_Locations;

   type Technique is record
      Lighting_Program                : GL.Objects.Programs.Program;
      WVP_Location                    : GL.Uniforms.Uniform := 0;
      Light_WVP_Location              : GL.Uniforms.Uniform := 0;
      World_Matrix_Location           : GL.Uniforms.Uniform := 0;
      Colour_Map_Location             : GL.Uniforms.Uniform := 0;
      Shadow_Map_Location             : GL.Uniforms.Uniform := 0;
      Normal_Map_Location             : GL.Uniforms.Uniform := 0;
      Eye_World_Pos_Location          : GL.Uniforms.Uniform := 0;
      Direct_Light_Location           : Direct_Light_Locations;
      Mat_Specular_Intensity_Location : GL.Uniforms.Uniform := 0;
      Mat_Specular_Power_Location     : GL.Uniforms.Uniform := 0;
      Num_Point_Lights_Location       : GL.Uniforms.Uniform := 0;
      Num_Spot_Lights_Location        : GL.Uniforms.Uniform := 0;
      Point_Lights_Locations          : Point_Lights_Location_Array;
      Spot_Lights_Locations           : Spot_Lights_Location_Array;
   end record;

end Lighting_Technique_26;
