
package Material_Keys is

    type AI_Mat_Key is
      (AI_Mat_Key_Name, AI_Mat_Key_Two_Sided, AI_Mat_Key_Shading_Model,
       AI_Mat_Key_Enable_Wire_Frame, AI_Mat_Key_Blend_Func, AI_Mat_Key_Opacity,
       AI_Mat_Key_Bump_Scaling, AI_Mat_Key_Shininess, AI_Mat_Key_Reflectivity,
       AI_Mat_Key_Shininess_Strength, AI_Mat_Key_Refractivity,
       AI_Mat_Key_Colour_Diffuse, AI_Mat_Key_Colour_Ambient,
       AI_Mat_Key_Colour_Specular, AI_Mat_Key_Colour_Emissive,
       AI_Mat_Key_Colour_Transparent, AI_Mat_Key_Colour_Reflective,
       AI_Mat_Key_Global_Background_Image,
       AI_Mat_Key_Texture_Base, AI_Mat_Key_UVWSRC_Base,
       AI_Mat_Key_Texop_Base, AI_Mat_Key_Mapping_Base,
       AI_Mat_Key_Tex_Blend_Base, AI_Mat_Key_Mapping_Mode_U_Base,
       AI_Mat_Key_Mapping_Mode_V_Base, AI_Mat_Key_Tex_Map_Axis_Base,
       AI_Mat_UV_Transform_Base, AI_Mat_Key_Tex_Flags_Base);

    function AI_Material_Key (Key : AI_Mat_Key) return String;

end Material_Keys;
