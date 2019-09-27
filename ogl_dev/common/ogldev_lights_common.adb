
with Ada.Text_IO; use Ada.Text_IO;

package body Ogldev_Lights_Common is

--     generic
--        type In_Type  is (<>);
--        type Out_Type is (<>);
--     function Parameter (In_Param : In_Type) return Out_Type;
--     function Parameter (In_Param : In_Type) return Out_Type is
--        Light : Point_Light;
--     begin
--        return Light.Base.In_Param;
--     end Parameter;

   --  -------------------------------------------------------------------------

   procedure Add_To_ATB (Base : Base_Light; Bar : Ant_Tweak_Bar.TW_Bar) is
   begin
      null;
   end Add_To_ATB;

   --  -------------------------------------------------------------------------

   procedure Add_Directional_To_ATB (theLight : Directional_Light;
                         Bar : Ant_Tweak_Bar.TW_Bar) is
   begin
      null;
   end Add_Directional_To_ATB;

   --  -------------------------------------------------------------------------

   procedure Add_Point_To_ATB (theLight : Point_Light_Array; Bar : Ant_Tweak_Bar.TW_Bar) is
   begin
      null;
   end Add_Point_To_ATB;

   --  -------------------------------------------------------------------------

   procedure Add_Spot_To_ATB (theLight : Spot_Light_Array; Bar : Ant_Tweak_Bar.TW_Bar) is
   begin
      null;
   end Add_Spot_To_ATB;

   --  -------------------------------------------------------------------------

   function Ambient_Intensity (Light : Directional_Light) return Single is
   begin
      return Light.Base.Ambient_Intensity;
   end Ambient_Intensity;

   --  -------------------------------------------------------------------------

   function Ambient_Intensity (Light : Point_Light) return Single is
   begin
      return Light.Base.Ambient_Intensity;
   end Ambient_Intensity;

   --  -------------------------------------------------------------------------

   function Ambient_Intensity (Light : Spot_Light) return Single is
   begin
      return Light.Point.Base.Ambient_Intensity;
   end Ambient_Intensity;

   --  -------------------------------------------------------------------------

   function Attenuation_Constant (Light : Point_Light) return Single is
   begin
      return Light.Attenuation.Atten_Constant;
   end Attenuation_Constant;

   --  -------------------------------------------------------------------------

   function Attenuation_Constant (Light : Spot_Light) return Single is
   begin
      return Light.Point.Attenuation.Atten_Constant;
   end Attenuation_Constant;

   --  -------------------------------------------------------------------------

   function Attenuation_Exponent (Light : Point_Light) return Single is
   begin
      return Light.Attenuation.Exp;
   end Attenuation_Exponent;

   --  -------------------------------------------------------------------------

   function Attenuation_Linear (Light : Point_Light) return Single is
   begin
      return Light.Attenuation.Linear;
   end Attenuation_Linear;

   --  -------------------------------------------------------------------------

   function Attenuation_Linear (Light : Spot_Light) return Single is
   begin
      return Light.Point.Attenuation.Linear;
   end Attenuation_Linear;

   --  -------------------------------------------------------------------------

   function Colour (Light : Directional_Light) return Colors.Basic_Color is
   begin
      return Light.Base.Colour;
   end Colour;

   --  -------------------------------------------------------------------------

   function Colour (Light : Point_Light) return Colors.Basic_Color is
   begin
      return Light.Base.Colour;
   end Colour;

   --  -------------------------------------------------------------------------

   function Colour (Light : Spot_Light) return Colors.Basic_Color is
   begin
      return Light.Point.Base.Colour;
   end Colour;

   --  -------------------------------------------------------------------------

   function Cut_Off (Light : Spot_Light) return Maths.Degree is
   begin
      return Light.Cut_Off;
   end Cut_Off;

   --  -------------------------------------------------------------------------

   function Diffuse_Intensity (Light : Directional_Light) return Single is
   begin
      return Light.Base.Diffuse_Intensity;
   end Diffuse_Intensity;

   --  -------------------------------------------------------------------------

   function Diffuse_Intensity (Light : Point_Light) return Single is
   begin
      return Light.Base.Diffuse_Intensity;
   end Diffuse_Intensity;

   --  -------------------------------------------------------------------------

   function Diffuse_Intensity (Light : Spot_Light) return Single is
   begin
      return Light.Point.Base.Diffuse_Intensity;
   end Diffuse_Intensity;

   --  -------------------------------------------------------------------------

   function Direction (Light : Directional_Light) return Singles.Vector3 is
   begin
      return Light.Direction;
   end Direction;

   --  -------------------------------------------------------------------------

   function Direction (Light : Spot_Light) return Singles.Vector3 is
   begin
      return Light.Direction;
   end Direction;

   --  -------------------------------------------------------------------------

   function Exponent (Light : Spot_Light) return Single is
   begin
      return Light.Point.Attenuation.Exp;
   end Exponent;

   --  -------------------------------------------------------------------------

procedure Init_Directional_Light (Light : in out Directional_Light;
                                  Amb_Intensity, Diff_Intensity : Single;
                                  theColour : Colors.Basic_Color;
                                  Dir : Singles.Vector3) is
   begin
      Light.Base.Ambient_Intensity := Amb_Intensity;
      Light.Base.Diffuse_Intensity := Diff_Intensity;
      Light.Base.Colour := theColour;
      Light.Direction := Dir;
   end Init_Directional_Light;

   --  ------------------------------------------------------------------------

   function Position (Light : Point_Light) return Singles.Vector3 is
   begin
      return Light.Position;
   end Position;

   --  ------------------------------------------------------------------------

   function Position (Light : Spot_Light) return Singles.Vector3 is
   begin
      return Light.Point.Position;
   end Position;

   --  ------------------------------------------------------------------------

   procedure Set_Ambient_Intensity (Light     : in out Point_Light;
                                    Intensity : Single := 1.0) is
   begin
      Light.Base.Ambient_Intensity := Intensity;
   end Set_Ambient_Intensity;

   --  ------------------------------------------------------------------------

   procedure Set_Ambient_Intensity (Light     : in out Spot_Light;
                                    Intensity : Single := 1.0) is
   begin
      Light.Point.Base.Ambient_Intensity := Intensity;
   end Set_Ambient_Intensity;

   --  ------------------------------------------------------------------------

   procedure Set_Attenuation_Constant (Light       : in out Point_Light;
                                       Attenuation : Single := 0.0) is
   begin
      Light.Attenuation.Atten_Constant := Attenuation;
   end Set_Attenuation_Constant;

   --  ------------------------------------------------------------------------

   procedure Set_Attenuation_Constant (Light       : in out Spot_Light;
                                       Attenuation : Single := 0.0) is
   begin
      Light.Point.Attenuation.Atten_Constant := Attenuation;
   end Set_Attenuation_Constant;

   --  ------------------------------------------------------------------------

   procedure Set_Direction (Light : in out Spot_Light; Dir : Singles.Vector3) is
   begin
      if Maths.Length (Light.Direction) = 0.0 then
         raise Light_Exception with
           "Ogldev_Lights_Common.Set_Direction zero length Spot_Light direction vector";
      end if;
      Light.Direction := Dir;

   exception
      when  others =>
         Put_Line ("An exception occurred in Ogldev_Lights_Common.Direction Spot_Light.");
         raise;
   end Set_Direction;

   --  ------------------------------------------------------------------------

   procedure Set_Exp_Attenuation (Light       : in out Point_Light;
                                  Attenuation : Single := 0.0) is
   begin
      Light.Attenuation.Exp := Attenuation;
   end Set_Exp_Attenuation;

   --  ------------------------------------------------------------------------

   procedure Set_Linear_Attenuation (Light       : in out Point_Light;
                                     Attenuation : Single := 1.0) is
   begin
      Light.Attenuation.Linear := Attenuation;
   end Set_Linear_Attenuation;

   --  ------------------------------------------------------------------------

   procedure Set_Linear_Attenuation (Light       : in out Spot_Light;
                                     Attenuation : Single := 1.0) is
   begin
      Light.Point.Attenuation.Linear := Attenuation;
   end Set_Linear_Attenuation;

   --  ------------------------------------------------------------------------

   procedure Set_Diffuse_Intensity (Light     : in out Point_Light;
                                    Intensity : Single := 0.1) is
   begin
      Light.Base.Diffuse_Intensity := Intensity;
   end Set_Diffuse_Intensity;

   --  ------------------------------------------------------------------------

   procedure Set_Diffuse_Intensity (Light     : in out Spot_Light;
                                    Intensity : Single := 0.1) is
   begin
      Light.Point.Base.Diffuse_Intensity := Intensity;
   end Set_Diffuse_Intensity;

   --  ------------------------------------------------------------------------

   procedure Set_Cut_Off (Light : in out Spot_Light; Cut_Off_Val : Maths.Degree) is
   begin
      Light.Cut_Off := Cut_Off_Val;
   end Set_Cut_Off;

   --  ------------------------------------------------------------------------

   procedure Set_Point_Light (Light : in out Point_Light; Pos : Singles.Vector3;
                              theColour : Colors.Basic_Color) is
   begin
      Light.Position := Pos;
      Light.Base.Colour := theColour;
   end Set_Point_Light;

   --  ------------------------------------------------------------------------

   procedure Set_Spot_Light (Light : in out Spot_Light; Pos : Singles.Vector3;
                             theColour : Colors.Basic_Color) is
   begin
      Light.Point.Position := Pos;
      Light.Point.Base.Colour := theColour;
   end Set_Spot_Light;

   --  ------------------------------------------------------------------------

end Ogldev_Lights_Common;
