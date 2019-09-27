
with GL.Objects.Programs;
with GL.Types; use GL.Types;
with GL.Uniforms;

package Billboard_Technique_27 is

    type Technique is private;

    function Get_Uniform_Location (theTechnique : Technique; Uniform_Name : String)
                                   return GL.Uniforms.Uniform;
    function Init (theTechnique : out Technique) return Boolean;
    procedure Set_Camera_Position_Location (theTechnique : Technique; Pos : Singles.Vector3);
    procedure Set_Colour_Texture_Unit_Location (theTechnique : Technique; Texture_Unit : Int);
    procedure Set_VP_Location (theTechnique : Technique; VP : Singles.Matrix4);
    procedure Use_Program (theTechnique : Technique);

private
    type Technique is record
        Lighting_Program     : GL.Objects.Programs.Program;
        Camera_Pos_Location  : GL.Uniforms.Uniform := 0;
        Colour_Map_Location  : GL.Uniforms.Uniform := 0;
        VP_Location          : GL.Uniforms.Uniform := 0;
    end record;


end Billboard_Technique_27;
