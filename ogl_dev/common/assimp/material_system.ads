
with Ada.Strings.Unbounded;
with GL.Types;

with Material;
with Assimp_Types;

package Material_System is

   type Texture_Mapping is (Texture_Mapping_UV, Texture_Mapping_SPHERE, Texture_Mapping_CYLINDER,
                            Texture_Mapping_BOX, Texture_Mapping_PLANE,  Texture_Mapping_OTHER);

   Material_System_Exception : Exception;

   function Get_Material_String (aMaterial      : Material.AI_Material;
                                 Key            : String;
--                                   Property_Type  : Material.AI_Property_Type_Info;
                                 Material_Type  : GL.Types.UInt;
                                 Property_Index : GL.Types.UInt;
                                 Data_String    : out Ada.Strings.Unbounded.Unbounded_String)
                                 return Assimp_Types.API_Return;

   function Get_Texture (aMaterial : Material.AI_Material;
                         Tex_Type  : Material.AI_Texture_Type;
                         Tex_Index : GL.Types.UInt := 0;
                         Path      : out Ada.Strings.Unbounded.Unbounded_String)
                         return Assimp_Types.API_Return;
private
   for Texture_Mapping use (Texture_Mapping_UV       => 0,
                            Texture_Mapping_SPHERE   => 1,
                            Texture_Mapping_CYLINDER => 2,
                            Texture_Mapping_BOX      => 3,
                            Texture_Mapping_PLANE    => 4,
                            Texture_Mapping_OTHER    => 5);

end Material_System;
