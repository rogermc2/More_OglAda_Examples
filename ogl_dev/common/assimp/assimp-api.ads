
with Interfaces.C;
with Interfaces.C.Strings;

with Assimp_Types;

with Assimp_Mesh;
with Material;
with Scene;

package Assimp.API is

   --  GetTexture (aiTextureType type, unsigned int index, aiString *path,
   --  aiTextureMapping *mapping=NULL, unsigned int *uvindex=NULL,
   --  float *blend=NULL, aiTextureOp *op=NULL, aiTextureMapMode *mapmode=NULL) const
--     function Get_Material_Texture1 (aMaterial : Material.API_Material;
--                                    Tex_Type : Material.AI_Texture_Type;
--                                    Index : Interfaces.C.unsigned;
--                                    Path : access Assimp_Types.API_String)
--                                    return Assimp_Types.API_Return;
--     pragma Import (C, Get_Material_Texture1, "aiGetMaterialTexture");

--     function Get_Material_Texture (aMaterial : access Material.API_Material;
--                                    Tex_Type : Material.AI_Texture_Type;
--                                    Index : Interfaces.C.unsigned;
--                                    Path : access Assimp_Types.API_String := null;
--                                    Mapping : access Material.AI_Texture_Mapping := null;
--                                    UV_Index : access Interfaces.C.unsigned := null;
--                                    Blend : access Interfaces.C.C_float := null;
--                                    Op : access Material.AI_Texture_Op := null;
--                                    Map_Mode : access Material.AI_Texture_Map_Mode := null)
--                                    return Assimp_Types.API_Return;
--     pragma Import (C, Get_Material_Texture, "aiGetMaterialTexture");

   function Get_Material_Texture_Count (aMaterial : Material.API_Material;
                                        Tex_Type : Material.AI_Texture_Type)
                                        return Interfaces.C.unsigned;
   pragma Import (C, Get_Material_Texture_Count, "aiGetMaterialTextureCount");

   function Import_File (File_Name : Interfaces.C.Strings.chars_ptr;
                         Flags : Interfaces.C.unsigned) return access Scene.API_Scene;
   pragma Import (C, Import_File, "aiImportFile");

   function Read_File (File_Name : Interfaces.C.Strings.chars_ptr;
                       Flags : Interfaces.C.unsigned) return access Scene.API_Scene;
   pragma Import (C, Read_File, "aiReadFile");

   procedure Init_Mesh (theMesh : in out Assimp_Mesh.Mesh_Array_Pointer;
                        Mesh_Index : Interfaces.C.unsigned);
   pragma Import (C, Init_Mesh, "aiInitMesh");

end Assimp.API;
