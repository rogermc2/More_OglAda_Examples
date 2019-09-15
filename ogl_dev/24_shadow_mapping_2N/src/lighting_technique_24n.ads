
with GL.Objects.Programs;
with GL.Types; use GL.Types;
with GL.Uniforms;

with Maths;

package Lighting_Technique_24N is

    Max_Point_Lights : GL.Types.Int := 2;
    Max_Spot_Lights  : GL.Types.Int := 2;

    type Directional_Light is private;
    type Point_Light is private;
    type Spot_Light is private;
    type Technique is private;

    type Attenuation is record
        Constant_Atten : Single := 0.0;
        Linear         : Single := 0.0;
        EXP            : Single := 0.0;
    end record;

   type Point_Lights_Array is array (Int range <>) of
     Point_Light;
   type Spot_Lights_Array is array (Int range <>) of
     Spot_Light;

    function Get_Directional_Ambient (Light : Directional_Light) return Single;
    function Get_Directional_Diffuse (Light : Directional_Light) return Single;
    function Get_Position (Light : Spot_Light) return Singles.Vector3;
    function Get_Direction (Light : Spot_Light) return Singles.Vector3;
    function Get_Spot_Ambient (Light : Spot_Light) return Single;
    function Get_Spot_Diffuse (Light : Spot_Light) return Single;
    function Init (theTechnique : out Technique) return Boolean;
    procedure Init_Directional_Light (Light : out Directional_Light);
    procedure Set_Ambient_Intensity (theTechnique : Technique; Intensity : Single);
    procedure Set_Directional_Ambient (Light : in out Directional_Light;
                                       Ambient: Single);
    procedure Set_Directional_Diffuse (Light : in out Directional_Light;
                                       Diffuse : Single);
    procedure Set_Directional_Light_Uniform (theTechnique : Technique; Light : Directional_Light);
    procedure Set_Eye_World_Pos_Uniform (theTechnique : Technique; Eye_World_Pos : Singles.Vector3);
    procedure Set_Light_WVP_Uniform (theTechnique : Technique; WVP : Singles.Matrix4);
    procedure Set_Mat_Specular_Intensity (theTechnique : Technique; Intensity : Single);
    procedure Set_Mat_Specular_Power (theTechnique : Technique; Power : Single);
    procedure Set_Point_Light (Light : in out Point_Light; Ambient, Diffuse : Single;
                               Colour : Singles.Vector3;
                               Pos : Singles.Vector3; Atten : Attenuation);
    procedure Set_Point_Light_Uniforms (theTechnique : Technique; Lights : Point_Lights_Array);
    procedure Set_Shadow_Texture_Unit (theTechnique : Technique; Texture_Unit : Int);
    procedure Set_Spot_Ambient (Light : in out Spot_Light; Ambient : Single);
    procedure Set_Spot_Diffuse (Light : in out Spot_Light; Diffuse : Single);
    procedure Set_Spot_Light_Uniforms (theTechnique : Technique; Lights : Spot_Lights_Array);
    procedure Set_Spot_Light (Light : in out Spot_Light; Ambient, Diffuse : Single;
                               Colour, Pos, Direction : Singles.Vector3;
                               Atten : Attenuation; Cut_Off : Maths.Degree);
    procedure Set_Texture_Unit (theTechnique : Technique; Texture_Unit : Int);
    procedure Set_World_Matrix_Uniform (theTechnique : Technique; World_Inverse : Singles.Matrix4);
    procedure Set_WVP_Uniform (theTechnique : Technique; WVP : Singles.Matrix4);
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
        Cutoff    : Maths.Degree := 0.0;
    end record;

    type Directional_Light is record
        Base      : Base_Light;
        Direction : Singles.Vector3 := (0.0, 0.0, 0.0);
    end record;

   type Atten_Uniform is record
      Constant_Atten : GL.Uniforms.Uniform := 0;
      Linear         : GL.Uniforms.Uniform := 0;
      Exp            : GL.Uniforms.Uniform := 0;
   end record;

   type Point_Light_Uniforms is record
      Colour            : GL.Uniforms.Uniform := 0;
      Ambient_Intensity : GL.Uniforms.Uniform := 0;
      Diffuse_Intensity : GL.Uniforms.Uniform := 0;
      Position          : GL.Uniforms.Uniform := 0;
      Atten             : Atten_Uniform;
   end record;

   type Spot_Light_Uniforms is record
      Colour            : GL.Uniforms.Uniform := 0;
      Ambient_Intensity : GL.Uniforms.Uniform := 0;
      Diffuse_Intensity : GL.Uniforms.Uniform := 0;
      Position          : GL.Uniforms.Uniform := 0;
      Direction         : GL.Uniforms.Uniform := 0;
      Cutoff            : GL.Uniforms.Uniform := 0;
      Atten             : Atten_Uniform;
   end record;

   type Point_Lights_Uniform_Array is array (1 .. Max_Point_Lights) of
     Point_Light_Uniforms;
   type Spot_Lights_Uniform_Array is array (1 .. Max_Spot_Lights) of
     Spot_Light_Uniforms;

   type Direct_Light_Uniforms is record
      Colour            : GL.Uniforms.Uniform := 0;
      Ambient_Intensity : GL.Uniforms.Uniform := 0;
      Diffuse_Intensity : GL.Uniforms.Uniform := 0;
      Direction         : GL.Uniforms.Uniform := 0;
   end record;

   type Technique is record
      Lighting_Program                : GL.Objects.Programs.Program;
      Direct_Light_Uniform           : Direct_Light_Uniforms;
      WVP_Uniform                    : GL.Uniforms.Uniform := 0;
      Light_WVP_Uniform              : GL.Uniforms.Uniform := 0;
      World_Matrix_Uniform           : GL.Uniforms.Uniform := 0;
      Sampler_Uniform                : GL.Uniforms.Uniform := 0;
      Shadow_Map_Uniform             : GL.Uniforms.Uniform := 0;
      Eye_World_Pos_Uniform          : GL.Uniforms.Uniform := 0;
      Mat_Specular_Intensity_Uniform : GL.Uniforms.Uniform := 0;
      Mat_Specular_Power_Uniform     : GL.Uniforms.Uniform := 0;
      Num_Point_Lights_Uniform       : GL.Uniforms.Uniform := 0;
      Num_Spot_Lights_Uniform        : GL.Uniforms.Uniform := 0;
      Point_Uniforms                 : Point_Lights_Uniform_Array;
      Spot_Uniforms                  : Spot_Lights_Uniform_Array;
   end record;

end Lighting_Technique_24N;
