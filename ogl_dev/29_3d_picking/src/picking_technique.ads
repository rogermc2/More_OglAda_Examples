
with Ada.Strings.Unbounded;

with GL.Objects.Programs;
with GL.Types;
with GL.Uniforms;

Package Picking_Technique is

    type Update_Technique is private;

    Update_Technique_Exception : Exception;

    function Active_Attributes (theTechnique : Update_Technique) return GL.Types.Size;
    function Get_Random_Texture_Location (theTechnique : Update_Technique)
                                         return GL.Uniforms.Uniform;
    function Get_WVP_Location (theTechnique : Update_Technique)
                               return GL.Uniforms.Uniform;
    function Get_Update_Program (theTechnique : Update_Technique)
                                return GL.Objects.Programs.Program;
    procedure Init (theTechnique : in out Update_Technique);
    procedure Set_WVP (theTechnique : Update_Technique;
                                  Delta_Time : GL.Types.UInt);
    procedure Set_Draw_Index (theTechnique : Update_Technique;
                        theTime : GL.Types.UInt);
    procedure Set_Object_Index (theTechnique : Update_Technique;
                                       Texture_Unit : GL.Types.Int);
     function Update_Program  (theTechnique : Update_Technique)
                             return GL.Objects.Programs.Program;
    procedure Use_Program (theTechnique : Update_Technique);

private
    use GL.Uniforms;
    type Update_Technique is record
    --  Shader_Object_List -- inherited from Technique.h
        Update_Program          : GL.Objects.Programs.Program;
        WVP_Location            : GL.Uniforms.Uniform := -1;
        Draw_Index_Location     : GL.Uniforms.Uniform := -1;
        Object_Index_Location    : GL.Uniforms.Uniform := -1;
    end record;

end Picking_Technique;
