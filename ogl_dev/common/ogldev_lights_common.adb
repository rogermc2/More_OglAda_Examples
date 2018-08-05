
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

   function Ambient_Intensity (Light : Directional_Light) return Single is
   begin
      return Light.Ambient_Intensity;
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
   function Colour (Light : Point_Light) return Singles.Vector3 is
   begin
      return Light.Base.Colour;
   end Colour;

   --  -------------------------------------------------------------------------

   function Colour (Light : Spot_Light) return Singles.Vector3 is
   begin
      return Light.Point.Base.Colour;
   end Colour;

   --  -------------------------------------------------------------------------

   function Cut_Off (Light : Spot_Light) return Single is
   begin
      return Light.Cut_Off;
   end Cut_Off;

   --  -------------------------------------------------------------------------

   function Diffuse_Intensity (Light : Directional_Light) return Single is
   begin
      return Light.Diffuse_Intensity;
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

   function Position (Light : Point_Light) return Singles.Vector3 is
   begin
      return Light.Position;
   end Position;

   --  ------------------------------------------------------------------------

   procedure Set_Ambient_Intensity (Light : in out Point_Light; Intensity : Single) is
   begin
      Light.Base.Ambient_Intensity := Intensity;
   end Set_Ambient_Intensity;

   --  ------------------------------------------------------------------------

   procedure Set_Ambient_Intensity (Light : in out Spot_Light; Intensity : Single) is
   begin
      Light.Point.Base.Ambient_Intensity := Intensity;
   end Set_Ambient_Intensity;

   --  ------------------------------------------------------------------------

   procedure Set_Attenuation_Constant (Light : in out Point_Light; Attenuation : Single) is
   begin
      Light.Attenuation.Atten_Constant := Attenuation;
   end Set_Attenuation_Constant;

   --  ------------------------------------------------------------------------
   procedure Set_Exp_Attenuation (Light : in out Point_Light; Attenuation : Single) is
   begin
      Light.Attenuation.Exp := Attenuation;
   end Set_Exp_Attenuation;

   --  ------------------------------------------------------------------------

   procedure Set_Linear_Attenuation (Light : in out Point_Light; Attenuation : Single) is
   begin
      Light.Attenuation.Linear := Attenuation;
   end Set_Linear_Attenuation;

   --  ------------------------------------------------------------------------

   procedure Set_Direction (Light : in out Spot_Light; Direction : Singles.Vector3) is
   begin
      Light.Direction := Direction;
   end Set_Direction;

   --  ------------------------------------------------------------------------

   procedure Set_Diffuse_Intensity (Light : in out Point_Light; Intensity : Single) is
   begin
      Light.Base.Diffuse_Intensity := Intensity;
   end Set_Diffuse_Intensity;

   --  ------------------------------------------------------------------------

   procedure Set_Diffuse_Intensity (Light : in out Spot_Light; Intensity : Single) is
   begin
      Light.Point.Base.Diffuse_Intensity := Intensity;
   end Set_Diffuse_Intensity;

   --  ------------------------------------------------------------------------

   procedure Set_Cut_Off (Light : in out Spot_Light; Cut_Off : Single) is
   begin
      Light.Cut_Off := Cut_Off;
   end Set_Cut_Off;

   --  ------------------------------------------------------------------------

   procedure Set_Point_Light (Light : in out Point_Light; Position : Singles.Vector3;
                              Colour : Singles.Vector3) is
   begin
      Light.Position := Position;
      Light.Base.Colour := Colour;
   end Set_Point_Light;

   --  ------------------------------------------------------------------------

   procedure Set_Spot_Light (Light : in out Spot_Light; Position : Singles.Vector3;
                              Colour : Singles.Vector3) is
   begin
      Light.Point.Position := Position;
      Light.Point.Base.Colour := Colour;
   end Set_Spot_Light;

   --  ------------------------------------------------------------------------

end Ogldev_Lights_Common;
