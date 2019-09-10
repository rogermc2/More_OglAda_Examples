
with GL.Objects.Programs;
with GL.Types; use GL.Types;

package Lighting_Technique_17 is

    type Directional_Light is record
        Colour            : Singles.Vector3 := (1.0, 1.0, 1.0);
        Ambient_Intensity : Single := 0.5;
    end record;

   function Get_Directional_Ambient (Light : Directional_Light) return Single;
   procedure Set_Directional_Colour (Light : in out Directional_Light;
                                      Colour : Singles.Vector3);
   function Init (Shader_Program : in out GL.Objects.Programs.Program)
                  return Boolean;
    procedure Set_Directional_Ambient (Light : in out Directional_Light;
                                       Ambient : Single);
    procedure Set_WVP (WVP : Singles.Matrix4);
    procedure Set_Texture_Unit (Texture_Unit : Int);
    procedure Set_Directional_Light_Locations (Light : Directional_Light);

end Lighting_Technique_17;
