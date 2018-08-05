
with Ada.Strings.Unbounded;

with GL.Types; use GL.Types;

package Ogldev_Lights_Common is
   type Base_Light is private;
   type Directional_Light is private;
   type Light_Attenuation is private;
   type Point_Light is private;
   type Spot_Light is private;

   Colour_White : constant Singles.Vector3 := (1.0, 1.0, 1.0);
   Colour_Red : constant Singles.Vector3 := (1.0, 0.0, 0.0);
   Colour_Green : constant Singles.Vector3 := (0.0, 1.0, 0.0);
   Colour_Cyan : constant Singles.Vector3 := (0.0, 1.0, 1.0);
   Colour_Blue : constant Singles.Vector3 := (0.0, 0.0, 1.0);

   function Ambient_Intensity (Light : Directional_Light) return Single;
   function Ambient_Intensity (Light : Point_Light) return Single;
   function Ambient_Intensity (Light : Spot_Light) return Single;
   function Attenuation_Constant (Light : Point_Light) return Single;
   function Attenuation_Constant (Light : Spot_Light) return Single;
   function Attenuation_Exponent (Light : Point_Light) return Single;
   function Attenuation_Linear (Light : Point_Light) return Single;
   function Attenuation_Linear (Light : Spot_Light) return Single;
   function Colour (Light : Point_Light) return Singles.Vector3;
   function Colour (Light : Spot_Light) return Singles.Vector3;
   function Cut_Off (Light : Spot_Light) return Single;
   function Diffuse_Intensity (Light : Directional_Light) return Single;
   function Diffuse_Intensity (Light : Point_Light) return Single;
   function Diffuse_Intensity (Light : Spot_Light) return Single;
   function Direction (Light : Directional_Light) return Singles.Vector3;
   function Direction (Light : Spot_Light) return Singles.Vector3;
   function Exponent (Light : Spot_Light) return Single;
   function Position (Light : Point_Light) return Singles.Vector3;

   procedure Set_Ambient_Intensity (Light : in out Point_Light; Intensity : Single);
   procedure Set_Attenuation_Constant (Light : in out Point_Light; Attenuation : Single);
   procedure Set_Exp_Attenuation (Light : in out Point_Light; Attenuation : Single);
   procedure Set_Linear_Attenuation (Light : in out Point_Light; Attenuation : Single);
   procedure Set_Cut_Off (Light : in out Spot_Light; Cut_Off : Single);
   procedure Set_Diffuse_Intensity (Light : in out Point_Light; Intensity : Single);
   procedure Set_Diffuse_Intensity (Light : in out Spot_Light; Intensity : Single);

   procedure Set_Point_Light (Light : in out Point_Light; Position : Singles.Vector3;
                              Colour : Singles.Vector3);
   procedure Set_Spot_Light (Light : in out Spot_Light; Position : Singles.Vector3;
                              Colour : Singles.Vector3);
private

   type Base_Light is record
      Name              : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String ("");
      Colour            : Singles.Vector3 := (0.0, 0.0, 0.0);
      Ambient_Intensity : Single := 0.0;
      Diffuse_Intensity : Single := 0.0;
   end record;

   type Directional_Light is record
      Ambient_Intensity  : Single := 0.2;
      Diffuse_Intensity  : Single := 0.8;
      Colure             : Singles.Vector3 := (1.0, 1.0, 1.0);
      Direction          : Singles.Vector3 := (1.0, 0.0, 0.0);
   end record;

   type Light_Attenuation is record
      Atten_Constant : Single := 1.0;
      Linear         : Single := 0.0;
      Exp            : Single := 0.0;
   end record;

   type Point_Light is record
      Base         : Base_Light;
      Position     : Singles.Vector3 := (1.0, 1.0, 1.0);
      Attenuation  : Light_Attenuation;
   end record;

   type Spot_Light is record
      Point     : Point_Light;
      Direction : Singles.Vector3 := (1.0, 1.0, 1.0);
      Cut_Off   : Single := 0.0;
   end record;

end Ogldev_Lights_Common;
