
--  with System;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

with GL.Types;

with Animation;
with Assimp_Mesh;
with Assimp_Texture;
with Camera;
with Material;
with Light;
with Mesh;
with Scene;

package C_Import is

   type API_Scene is record
            Flags          : Interfaces.C.unsigned := 0;
            Root_Node      : Scene.Node_Pointers.Pointer;
            Num_Meshes     : Interfaces.C.unsigned := 0;
            Meshes         : Assimp_Mesh.Mesh_Array_Pointer;
--              Meshes         : Mesh.API_Mesh_Array (1 .. Array_Sizes.Num_Meshes);
            Num_Materials  : Interfaces.C.unsigned := 0;
            Materials      : Material.Material_Pointers.Pointer;
--              Materials      : Material.API_Material_Array
--                (1 .. Array_Sizes.Num_Materials);
            Num_Animations : Interfaces.C.unsigned := 0;
            Animations     : Animation.Animation_Pointers.Pointer;
--              Animations     : Animation.API_Animation_Array
--                (1 .. Array_Sizes.Num_Animations);
            Num_Textures   : Interfaces.C.unsigned := 0;
            Textures       : Assimp_Texture.Texture_Pointers.Pointer;
--              Textures       : Assimp_Texture.API_Texture_Array
--                (1 .. Array_Sizes.Num_Textures);
            Num_Lights     : Interfaces.C.unsigned := 0;
            Lights         : Light.Light_Pointers.Pointer;
--              Lights         : Light.API_Light_Array
--                (1 .. Array_Sizes.Num_Lights);
            Num_Cameras    : Interfaces.C.unsigned := 0;
            Cameras         : Camera.Camera_Pointers.Pointer;
--              Cameras        : Camera.API_Camera_Array
--                (1 .. Array_Sizes.Num_Cameras);
            Private_Data   : Interfaces.C.Strings.chars_ptr;
         end record;
         pragma Convention (C_Pass_By_Copy, API_Scene);

   function Import_File (File_Name : String; Flags : GL.Types.UInt)
                         return Scene.AI_Scene;
private

--     type SA_Scene is record
--        Flags          : Interfaces.C.unsigned := 0;
--        Root_Node      : Scene.API_Node_Ptr;
--        Num_Meshes     : Interfaces.C.unsigned := 0;
--        Meshes         : Mesh.Mesh_Pointers.Pointer;
--  --        Meshes         : System.Address;
--        Num_Materials  : Interfaces.C.unsigned := 0;
--        Materials      : System.Address;
--        Num_Animations : Interfaces.C.unsigned := 0;
--        Animations     : System.Address;
--        Num_Textures   : Interfaces.C.unsigned := 0;
--        Textures       : System.Address;
--        Num_Lights     : Interfaces.C.unsigned := 0;
--        Lights         : System.Address;
--        Num_Cameras    : Interfaces.C.unsigned := 0;
--        Cameras        : System.Address;
--        Private_Data   : Interfaces.C.Strings.chars_ptr;
--     end record;
--     pragma Convention (C_Pass_By_Copy, SA_Scene);

end C_Import;
