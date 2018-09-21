
with Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;

with Material;
with Assimp_Types;

package Material_System is

   function Get_Material_Float (aMaterial : access Material.API_Material;
                                Key       : access Assimp_Types.API_String;
                                Property_Type  : Material.AI_Property_Type_Info;
                                Property_Index : Interfaces.C.unsigned;
                                theFloat       : access Interfaces.C.C_float;
                                pMax           : access Interfaces.C.unsigned := null)
                                return Assimp_Types.API_Return;
   pragma Import (C, Get_Material_Float, "aiGetMaterialFloatArray");

   function Get_Material_Integer (aMaterial : access Material.API_Material;
                                  Key       : access Assimp_Types.API_String;
                                  Property_Type  : Material.AI_Property_Type_Info;
                                  Property_Index : Interfaces.C.unsigned;
                                  theInteger     : access Interfaces.C.int;
                                  pMax           : access Interfaces.C.unsigned := null)
                                 return Assimp_Types.API_Return;
   pragma Import (C, Get_Material_Integer, "aiGetMaterialIntegerArray");

function Get_Material_Property (aMaterial : access Material.API_Material;
                                Key       : access Assimp_Types.API_String;
                                Property_Type  : Material.AI_Property_Type_Info;
                                Property_Index : Interfaces.C.unsigned;
                                theProperty    : access Material.API_Material_Property_Ptr)
                                return Assimp_Types.API_Return;
   pragma Import (C, Get_Material_Property, "aiGetMaterialProperty");

   function Get_Material_String (aMaterial : access Material.API_Material;
                                 Key       : access Assimp_Types.API_String;
                                  Property_Type  : Material.AI_Property_Type_Info;
                                  Property_Index : Interfaces.C.unsigned;
                                 Data_String    : access Assimp_Types.API_String)
                                 return Assimp_Types.API_Return;
   pragma Import (C, Get_Material_String, "aiGetMaterialString");


   function Get_Material_String1 (aMaterial : Material.API_Material;
                                 Key       : Assimp_Types.API_String;
                                  Property_Type  : Material.AI_Property_Type_Info;
                                  Property_Index : Interfaces.C.unsigned;
                                 Data_String    : out Assimp_Types.API_String)
                                 return Assimp_Types.API_Return;
end Material_System;
