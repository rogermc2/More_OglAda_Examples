
with GL.Objects.Programs;
with GL.Types; use GL.Types;
with GL.Uniforms;

with Ant_Tweak_Bar;
with Ogldev_Engine_Common;
with Ogldev_Lights_Common; use  Ogldev_Lights_Common;

package Ogldev_Basic_Lighting is

   type Basic_Lighting_Technique is private;
   type Point_Light is private;
   type Spot_Light is private;

   Max_Point_Lights : constant Int := 2;
   Max_Spot_Lights  : constant Int := 2;

   type Point_Lights is private;
   type Spot_Lights  is private;

   type Point_Light_Array is array (Int range <>) of Ogldev_Lights_Common.Point_Light;
   type Spot_Light_Array is array (Int range <>) of Ogldev_Lights_Common.Spot_Light;

   procedure Add_To_ATB (Base : Base_Light; Bar : Ant_Tweak_Bar.TW_Bar);
   procedure Add_Directional_To_ATB (theLight : Directional_Light;
                                     Bar : Ant_Tweak_Bar.TW_Bar);
   procedure Add_Point_To_ATB (theLight : Point_Lights; Bar : Ant_Tweak_Bar.TW_Bar);
   procedure Add_Spot_To_ATB (theLight : Spot_Lights; Bar : Ant_Tweak_Bar.TW_Bar);
   function Init (Lighting_Technique : in out Basic_Lighting_Technique) return Boolean;

   function Lighting_Program (Technique : Basic_Lighting_Technique) return GL.Objects.Programs.Program;

--     function Point_Light_Ambient_ID (Technique : Basic_Lighting_Technique;
--                                      Index : Int) return GL.Uniforms.Uniform;
--     function Spot_Light_Ambient_ID (Technique : Basic_Lighting_Technique;
--                                     Index : Int) return GL.Uniforms.Uniform;
--     function Point_Light_Attenuation_Const_ID (Technique : Basic_Lighting_Technique;
--                                                Index : Int) return GL.Uniforms.Uniform;
--     function Point_Light_Attenuation_Exp_ID (Technique : Basic_Lighting_Technique;
--                                                Index : Int) return GL.Uniforms.Uniform;
--     function Point_Light_Attenuation_Linear_ID (Technique : Basic_Lighting_Technique;
--                                                Index : Int) return GL.Uniforms.Uniform;
--    function Point_Light_Diffuse_ID (Technique : Basic_Lighting_Technique;
--                                       Index : Int) return GL.Uniforms.Uniform;
--     function Point_Light_Direction_ID (Technique : Basic_Lighting_Technique;
--                                        Index : Int) return GL.Uniforms.Uniform;
--     function Point_Light_Colour_ID (Technique : Basic_Lighting_Technique;
--                                     Index : Int) return GL.Uniforms.Uniform;
--     function Spot_Light_Colour_ID (Technique : Basic_Lighting_Technique;
--                                    Index : Int) return GL.Uniforms.Uniform;
   procedure Set_Color_Texture_Unit (Technique : Basic_Lighting_Technique;
                   Texture_Unit : Ogldev_Engine_Common.Texture_Unit_Index);
   procedure Set_Directional_Light (Technique : Basic_Lighting_Technique;
                                    Light : Directional_Light);
   procedure Set_Point_Lights (Technique : Basic_Lighting_Technique;
                               Lights : Point_Lights);
   procedure Set_Spot_Lights (Technique : Basic_Lighting_Technique;
                              Spot : Spot_Lights);
   procedure Set_EyeWorld_Pos (Technique : Basic_Lighting_Technique;
                               EyeWorldPos : Singles.Vector3);
   procedure Set_Mat_Specular_Intensity (Technique : Basic_Lighting_Technique;
                                         Intensity : Single);
   procedure Set_Mat_Specular_Power (Technique : Basic_Lighting_Technique;
                                     Power : Single);
   procedure Set_World_Matrix (Technique : Basic_Lighting_Technique;
                               World_Inverse : Singles.Matrix4);
   procedure Set_WVP (Technique : Basic_Lighting_Technique;
                      WVP : Singles.Matrix4);

private

   type Direct_Light is record
      Colour            : GL.Uniforms.Uniform;
      Ambient_Intensity : GL.Uniforms.Uniform;
      Diffuse_Intensity : GL.Uniforms.Uniform;
      Direction         : GL.Uniforms.Uniform;
   end record;

   type Atten is record
      Atten_Constant    : GL.Uniforms.Uniform;
      Linear            : GL.Uniforms.Uniform;
      Exp               : GL.Uniforms.Uniform;
   end record;

   type Point_Light is record
      Colour            : GL.Uniforms.Uniform;
      Ambient_Intensity : GL.Uniforms.Uniform;
      Diffuse_Intensity : GL.Uniforms.Uniform;
      Position          : GL.Uniforms.Uniform;
      Attenuation       : Atten;
   end record;

   type Spot_Light is record
      Colour            : GL.Uniforms.Uniform;
      Ambient_Intensity : GL.Uniforms.Uniform;
      Diffuse_Intensity : GL.Uniforms.Uniform;
      Position          : GL.Uniforms.Uniform;
      Direction         : GL.Uniforms.Uniform;
      Cut_Off           : GL.Uniforms.Uniform;
      Attenuation       : Atten;
   end record;

   type Point_Lights is array (1 .. Max_Point_Lights) of Point_Light;
   type Spot_Lights  is array (1 .. Max_Spot_Lights) of Spot_Light;

   type Basic_Lighting_Technique is record
      Lighting_Program                : GL.Objects.Programs.Program;
      WVP_Location                    : GL.Uniforms.Uniform;
      World_Matrix_Location           : GL.Uniforms.Uniform;
      Colour_Texture_Location         : GL.Uniforms.Uniform;
      Eye_World_Pos_Location          : GL.Uniforms.Uniform;
      Mat_Specular_Intensity_Location : GL.Uniforms.Uniform;
      Mat_Specular_Power_Location     : GL.Uniforms.Uniform;
      Num_Point_Lights_Location       : GL.Uniforms.Uniform;
      Num_Spot_Lights_Location        : GL.Uniforms.Uniform;
      Dir_Light_Location              : Direct_Light;
      Point_Lights_Location           : Point_Lights;
      Spot_Lights_Location            : Spot_Lights;
   end record;

end Ogldev_Basic_Lighting;
