
with GL.Objects.Programs;
with GL.Types; use GL.Types;
with GL.Uniforms;

package Lighting_Technique_21 is

    MAX_POINT_LIGHTS : GL.Types.Int := 2;
    MAX_SPOT_LIGHTS  : GL.Types.Int := 2;

    type Directional_Light is private;
    type Point_Light is private;
    type Spot_Light is private;
    type Technique is private;

    type Attenuation is record
        Constant_Atten : Single := 1.0;
        Linear         : Single := 0.0;
        EXP            : Single := 0.0;
    end record;

   type Point_Lights_Array is array (Int range <>) of
     Point_Light;
   type Spot_Lights_Array is array (Int range <>) of
     Spot_Light;

    function Init (theTechnique : out Technique) return Boolean;
   procedure Init_Directional_Light (Light : out Directional_Light);
    procedure Set_Point_Light (Light : in out Point_Light; Diffuse : Single;
                               Colour : Singles.Vector3;
                               Pos : Singles.Vector3; Atten : Attenuation);
    procedure Set_WVP (theTechnique : Technique; WVP : Singles.Matrix4);
    procedure Set_World_Matrix (theTechnique : Technique; World_Inverse : Singles.Matrix4);
    procedure Set_Texture_Unit (theTechnique : Technique; Texture_Unit : Int);
    procedure Set_Directional_Light (theTechnique : Technique; Light : Directional_Light);
    procedure Set_Eye_World_Pos (theTechnique : Technique; Eye_World_Pos : Singles.Vector3);
    procedure Set_Mat_Specular_Intensity (theTechnique : Technique; Intensity : Single);
    procedure Set_Mat_Specular_Power (theTechnique : Technique; Power : Single);
    procedure Set_Point_Light_Locations (theTechnique : Technique; Lights : Point_Lights_Array);
    procedure Set_Spot_Light_Locations (theTechnique : Technique; Lights : Spot_Lights_Array);
    procedure Use_Program (theTechnique : Technique);

 private

    --  Light records must conform with those of fragment shader
    type Base_Light is record
        Colour            : Singles.Vector3 := (0.0, 0.0, 0.0);
        Ambient_Intensity : Single := 0.0;
        Diffuse_Intensity : Single := 0.0;
    end record;

    type Point_Light is record
        Base     : Base_Light;
        Position : Singles.Vector3 := (0.0, 0.0, 0.0);
        Atten    : Attenuation;
    end record;

    type Spot_Light is record
        Point     : Point_Light;
        Direction : Singles.Vector3 := (0.0, 0.0, 0.0);
        Cutoff    : Single := 0.0;
    end record;

    type Directional_Light is record
        Base      : Base_Light;
        Direction : Singles.Vector3 := (0.0, 0.0, 0.0);
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

   type Point_Lights_Location_Array is array (1 .. Max_Point_Lights) of
     Point_Light_Locations;
   type Spot_Lights_Location_Array is array (1 .. Max_Spot_Lights) of
     Spot_Light_Locations;

   type Direct_Light_Locations is record
      Color             : GL.Uniforms.Uniform := 0;
      Ambient_Intensity : GL.Uniforms.Uniform := 0;
      Diffuse_Intensity : GL.Uniforms.Uniform := 0;
      Direction         : GL.Uniforms.Uniform := 0;
   end record;

   type Technique is record
      Lighting_Program                : GL.Objects.Programs.Program;
      WVP_Location                    : GL.Uniforms.Uniform := 0;
      World_Matrix_Location           : GL.Uniforms.Uniform := 0;
      Sampler_Location                : GL.Uniforms.Uniform := 0;
      Eye_World_Pos_Location          : GL.Uniforms.Uniform := 0;
      Direct_Light_Location           : Direct_Light_Locations;
      Mat_Specular_Intensity_Location : GL.Uniforms.Uniform := 0;
      Mat_Specular_Power_Location     : GL.Uniforms.Uniform := 0;
      Num_Point_Lights_Location       : GL.Uniforms.Uniform := 0;
      Num_Spot_Lights_Location        : GL.Uniforms.Uniform := 0;
      Point_Lights_Locations          : Point_Lights_Location_Array;
      Spot_Lights_Locations           : Spot_Lights_Location_Array;
   end record;


end Lighting_Technique_21;
