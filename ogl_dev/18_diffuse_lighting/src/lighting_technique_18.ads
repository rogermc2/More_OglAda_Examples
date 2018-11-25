
with GL.Objects.Programs;
with GL.Types; use GL.Types;

package Lighting_Technique_18 is

    type Directional_Light is record
        Colour            : Singles.Vector3 := (1.0, 1.0, 1.0);
        Ambient_Intensity : Single := 0.1;
        Diffuse_Intensity : Single := 0.75;
        Direction         : Singles.Vector3 := (1.0, 0.0, 0.0);
    end record;

    function Init (Shader_Program : in out GL.Objects.Programs.Program) return Boolean;
    procedure Set_WVP_Location (WVP : Singles.Matrix4);
    procedure Set_World_Matrix_Location (World_Inverse : Singles.Matrix4);
    procedure Set_Texture_Unit (Texture_Unit : Int);
    procedure Set_Directional_Light_Location (Light : Directional_Light);
    procedure Set_Eye_World_Pos (Eye_World_Pos : Singles.Vector3);
    procedure Set_Ambient_Intensity_Location (Intensity : Single);

end Lighting_Technique_18;
