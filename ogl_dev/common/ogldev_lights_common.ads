
with Ada.Strings.Unbounded;

with GL.Types; use GL.Types;
with GL.Types.Colors;

with Ant_Tweak_Bar;

package Ogldev_Lights_Common is
   type Base_Light is private;
   type Directional_Light is private;
   type Light_Attenuation is private;
   type Point_Light is private;
   type Spot_Light is private;

   type Point_Light_Array is array (UInt range <>) of Point_Light;
   type Spot_Light_Array  is array (UInt range <>) of Spot_Light;

   Light_Exception : Exception;

   Colour_White : constant GL.Types.Colors.Basic_Color := (1.0, 1.0, 1.0);
   Colour_Red   : constant GL.Types.Colors.Basic_Color := (1.0, 0.0, 0.0);
   Colour_Green : constant GL.Types.Colors.Basic_Color := (0.0, 1.0, 0.0);
   Colour_Cyan  : constant GL.Types.Colors.Basic_Color := (0.0, 1.0, 1.0);
   Colour_Blue  : constant GL.Types.Colors.Basic_Color := (0.0, 0.0, 1.0);

   procedure Add_To_ATB (Base : Base_Light; Bar : Ant_Tweak_Bar.TW_Bar);
   procedure Add_Directional_To_ATB (theLight : Directional_Light;
                                     Bar      : Ant_Tweak_Bar.TW_Bar);
   procedure Add_Point_To_ATB (theLight : Point_Light_Array; Bar : Ant_Tweak_Bar.TW_Bar);
   procedure Add_Spot_To_ATB (theLight : Spot_Light_Array; Bar : Ant_Tweak_Bar.TW_Bar);

   function Ambient_Intensity (Light : Directional_Light) return Single;
   function Ambient_Intensity (Light : Point_Light) return Single;
   function Ambient_Intensity (Light : Spot_Light) return Single;
   function Attenuation_Constant (Light : Point_Light) return Single;
   function Attenuation_Constant (Light : Spot_Light) return Single;
   function Attenuation_Exponent (Light : Point_Light) return Single;
   function Attenuation_Linear (Light : Point_Light) return Single;
   function Attenuation_Linear (Light : Spot_Light) return Single;
   function Colour (Light : Directional_Light) return Colors.Basic_Color;
   function Colour (Light : Point_Light) return Colors.Basic_Color;
   function Colour (Light : Spot_Light) return Colors.Basic_Color;
   function Cut_Off (Light : Spot_Light) return Single;
   function Diffuse_Intensity (Light : Directional_Light) return Single;
   function Diffuse_Intensity (Light : Point_Light) return Single;
   function Diffuse_Intensity (Light : Spot_Light) return Single;
   function Direction (Light : Directional_Light) return Singles.Vector3;
   function Direction (Light : Spot_Light) return Singles.Vector3;
   function Exponent (Light : Spot_Light) return Single;
   function Position (Light : Point_Light) return Singles.Vector3;
   function Position (Light : Spot_Light) return Singles.Vector3;

   procedure Init_Directional_Light (Light                         : in out Directional_Light;
                                     Amb_Intensity, Diff_Intensity : Single;
                                     theColour : Colors.Basic_Color; Dir : Singles.Vector3);
   procedure Set_Ambient_Intensity (Light : in out Point_Light; Intensity : Single := 1.0);
   procedure Set_Ambient_Intensity (Light     : in out Spot_Light;
                                    Intensity : Single := 1.0);
   procedure Set_Attenuation_Constant (Light : in out Point_Light; Attenuation : Single := 0.0);
   procedure Set_Attenuation_Constant (Light : in out Spot_Light; Attenuation : Single := 0.0);
   procedure Set_Direction (Light : in out Spot_Light; Dir : Singles.Vector3);
   procedure Set_Exp_Attenuation (Light : in out Point_Light; Attenuation : Single := 0.0);
   procedure Set_Linear_Attenuation (Light       : in out Point_Light;
                                     Attenuation : Single := 1.0);
   procedure Set_Linear_Attenuation (Light       : in out Spot_Light;
                                     Attenuation : Single := 1.0);
   procedure Set_Cut_Off (Light : in out Spot_Light; Cut_Off_Val : Single);
   procedure Set_Diffuse_Intensity (Light     : in out Point_Light;
                                    Intensity : Single := 0.1);
   procedure Set_Diffuse_Intensity (Light     : in out Spot_Light;
                                    Intensity : Single := 0.1);

   procedure Set_Point_Light (Light     : in out Point_Light; Pos : Singles.Vector3;
                              theColour : Colors.Basic_Color);
   procedure Set_Spot_Light (Light     : in out Spot_Light; Pos : Singles.Vector3;
                             theColour : Colors.Basic_Color);
private

   type Base_Light is record
      Name              : Ada.Strings.Unbounded.Unbounded_String :=
                            Ada.Strings.Unbounded.To_Unbounded_String ("");
      Colour            : Colors.Basic_Color := (0.0, 0.0, 0.0);
      Ambient_Intensity : Single := 0.0;
      Diffuse_Intensity : Single := 0.0;
   end record;

   type Directional_Light is record
      Base       : Base_Light;
      Direction  : Singles.Vector3 := (1.0, -1.0, 0.0);
   end record;

   type Light_Attenuation is record
      Atten_Constant : Single := 0.0;
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
