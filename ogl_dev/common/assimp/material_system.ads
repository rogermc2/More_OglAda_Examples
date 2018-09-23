
with Ada.Strings.Unbounded;

with GL.Types;

with Material;

package Material_System is

    type Texture_Mapping is (Texture_Mapping_UV, Texture_Mapping_SPHERE, Texture_Mapping_CYLINDER,
                             Texture_Mapping_BOX, Texture_Mapping_PLANE,  Texture_Mapping_OTHER);

     Material_System_Exception : Exception;

--     function Get_Material_Float (aMaterial      : access Material.API_Material;
--                                  Key            : Interfaces.C.Strings.chars_ptr;
--                                  Property_Type  : Material.AI_Property_Type_Info;
--                                  Property_Index : Interfaces.C.unsigned;
--                                  theFloat       : access Interfaces.C.C_float;
--                                  pMax           : access Interfaces.C.unsigned := null)
--                                  return Assimp_Types.API_Return;
--     pragma Import (C, Get_Material_Float, "aiGetMaterialFloatArray");
--
--     function Get_Material_Integer (aMaterial      : access Material.API_Material;
--                                    Key            : Interfaces.C.Strings.chars_ptr;
--                                    Property_Type  : Material.AI_Property_Type_Info;
--                                    Property_Index : Interfaces.C.unsigned;
--                                    theInteger     : access Interfaces.C.int;
--                                    pMax           : access Interfaces.C.unsigned := null)
--                                    return Assimp_Types.API_Return;
--     pragma Import (C, Get_Material_Integer, "aiGetMaterialIntegerArray");
--
--     function Get_Material_Property (aMaterial      : access Material.API_Material;
--                                     Key            : Interfaces.C.Strings.chars_ptr;
--                                     Property_Type  : Material.AI_Property_Type_Info;
--                                     Property_Index : Interfaces.C.unsigned;
--                                     theProperty    : access Material.API_Material_Property_Ptr)
--                                  return Assimp_Types.API_Return;
--     pragma Import (C, Get_Material_Property, "aiGetMaterialProperty");
--
--     function Get_Material_String (aMaterial      : access Material.API_Material;
--                                   Key            : Interfaces.C.Strings.chars_ptr;
--                                   Property_Type  : Material.AI_Property_Type_Info;
--                                   Property_Index : Interfaces.C.unsigned;
--                                   Data_String    : access Assimp_Types.API_String)
--                                   return Assimp_Types.API_Return;
--     pragma Import (C, Get_Material_String, "aiGetMaterialString");

   function Get_Material_String (aMaterial      : Material.AI_Material;
                                 Key            : String;
                                  Property_Type  : Material.AI_Property_Type_Info;
                                  Property_Index : GL.Types.UInt;
                                  Data_String    : out Ada.Strings.Unbounded.Unbounded_String)
                                  return Boolean;
   private
    for Texture_Mapping use (Texture_Mapping_UV       => 0,
                             Texture_Mapping_SPHERE   => 1,
                             Texture_Mapping_CYLINDER => 2,
                             Texture_Mapping_BOX      => 3,
                             Texture_Mapping_PLANE    => 4,
                             Texture_Mapping_OTHER    => 5);
end Material_System;
