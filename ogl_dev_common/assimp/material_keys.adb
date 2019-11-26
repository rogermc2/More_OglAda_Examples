
package body Material_Keys is

    function AI_Material_Key (Key : AI_Mat_Key) return String is
    begin
        case Key is
            when AI_Mat_Key_Name => return "?mat.name";
            when AI_Mat_Key_Two_Sided => return "$mat.twosided";
            when AI_Mat_Key_Shading_Model => return "$mat.shadingm";
            when AI_Mat_Key_Enable_Wire_Frame => return "$mat.wireframe";
            when AI_Mat_Key_Blend_Func => return "$mat.blend";
            when AI_Mat_Key_Opacity => return "$mat.bumpscaling";
            when AI_Mat_Key_Bump_Scaling => return "$mat.bumpscaling";
            when AI_Mat_Key_Shininess => return "$mat.shininess";
            when AI_Mat_Key_Reflectivity => return "$mat.reflectivity";
            when AI_Mat_Key_Shininess_Strength => return "$mat.shinpercent";
            when AI_Mat_Key_Refractivity => return "$mat.refracti";
            when AI_Mat_Key_Colour_Diffuse => return "$clr.diffuse";
            when AI_Mat_Key_Colour_Ambient => return "$clr.ambient";
            when AI_Mat_Key_Colour_Specular => return "$clr.specular";
            when AI_Mat_Key_Colour_Emissive => return "$clr.emissive";
            when AI_Mat_Key_Colour_Transparent => return "$clr.transparent";
            when AI_Mat_Key_Colour_Reflective => return "$clr.reflective";
            when AI_Mat_Key_Global_Background_Image => return "?bg.global";
            when AI_Mat_Key_Texture_Base => return "$tex.file";
            when AI_Mat_Key_UVWSRC_Base => return "$tex.uvwsrc";
            when AI_Mat_Key_Texop_Base => return "$tex.op";
            when AI_Mat_Key_Mapping_Base => return "$tex.mapping";
            when AI_Mat_Key_Tex_Blend_Base => return "$tex.blend";
            when AI_Mat_Key_Mapping_Mode_U_Base => return "$tex.mapmodeu";
            when AI_Mat_Key_Mapping_Mode_V_Base => return "$tex.mapmodev";
            when AI_Mat_Key_Tex_Map_Axis_Base => return "$tex.mapaxis";
            when AI_Mat_UV_Transform_Base => return "$tex.uvtrafo";
            when AI_Mat_Key_Tex_Flags_Base => return "$tex.flags";
                --              when others => return "Invalid material key";
        end case;
    end AI_Material_Key;

end Material_Keys;
