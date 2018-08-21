
with Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;

with Material;
with Assimp_Types;

package Material_System is

    function Get_Material_String (aMaterial : Material.API_Material; Key : Assimp_Types.API_String;
                                  Property_Type, Property_Index : Interfaces.C.unsigned;
                                  Data_String : out Assimp_Types.API_String) return Assimp_Types.API_Return ;

end Material_System;
