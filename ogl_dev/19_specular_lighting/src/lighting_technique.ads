
with GL.Objects.Programs;
with GL.Types; use GL.Types;

package Lighting_Technique is

    --  Declaration must conform with that of fragment shader
    type Directional_Light is record
        Colour            : Singles.Vector3 := (1.0, 1.0, 1.0);
        Ambient_Intensity : Single := 0.1;
        Diffuse_Intensity : Single := 0.9;
        Direction         : Singles.Vector3 := (0.5, 0.0, 0.5);
    end record;

    function Init (Shader_Program : in out GL.Objects.Programs.Program) return Boolean;
    procedure Set_WVP (WVP : Singles.Matrix4);
    procedure Set_World_Matrix (World_Inverse : Singles.Matrix4);
    procedure Set_Texture_Unit (Texture_Unit : Int);
    procedure Set_Directional_Light (Light : Directional_Light);
    procedure Set_Eye_World_Pos (Eye_World_Pos : Singles.Vector3);
    procedure Set_Ambient_Intensity (Intensity : Single);
    procedure Set_Mat_Specular_Intensity (Intensity : Single);
    procedure Set_Mat_Specular_Power (Power : Single);

end Lighting_Technique;
