
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

end Ogldev_Lights_Common;
