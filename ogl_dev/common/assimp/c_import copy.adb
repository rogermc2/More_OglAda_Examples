
pragma Warnings (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

--  with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Assimp.API;
with Assimp_Texture;
with Assimp_Types;

with Animation;
with Camera;
with Light;
with Material;
with Mesh;

package body C_Import is
   -- Read a file  into a scene with the Assimp aiImportFile function
   function Import_File (File_Name : String; Flags : GL.Types.UInt)
                         return Scene.AI_Scene is
--        use Assimp_Types;
      use Scene;

--        C_Scene   : API_Scene;
      Sizes     : Assimp.API.SA_Scene;
   begin
     Put_Line ("C_Import.Import_File theScene.");
     Sizes := Assimp.API.Import_File
        (Interfaces.C.Strings.New_String (File_Name), unsigned (Flags)).all;

         Put ("C_Import.Import_File, Num_Meshes, Num_Materials, Num_Animations");
         Put_Line (", Num_Textures, Num_Lights, Num_Cameras:");
         Put_Line (unsigned'Image (Sizes.Num_Meshes) &
                   unsigned'Image (Sizes.Num_Materials) &
                   unsigned'Image (Sizes.Num_Animations) &
                   unsigned'Image (Sizes.Num_Textures) &
                   unsigned'Image (Sizes.Num_Lights) &
                   unsigned'Image (Sizes.Num_Cameras));
      declare
--           theScene : AI_Scene (2, 2, 2, 2, 2, 2);
         use Assimp_Types;
         type Meshes_Array is array (1 .. Sizes.Num_Meshes) of Mesh.API_Mesh;
         pragma Convention (C, Meshes_Array);
         type Meshes_Ptr  is access Meshes_Array;
         pragma Convention (C, Meshes_Ptr);

         type Materials_Array is array (1 .. Sizes.Num_Materials) of Material.API_Material;
         pragma Convention (C, Materials_Array);
         type Materials_Ptr  is access Materials_Array;
         pragma Convention (C, Materials_Ptr);
--           Materials_Array_Size : Interfaces.C.size_t := Materials_Array'Size;

         type Animations_Array is array (1 .. Sizes.Num_Animations) of Animation.AI_Animation;
         pragma Convention (C, Animations_Array);
         type Animations_Ptr  is access Animations_Array;
         pragma Convention (C, Animations_Ptr);
--           Animations_Array_Size : Interfaces.C.size_t := Animations_Array'Size;

         type Textures_Array is array (1 .. Sizes.Num_Textures) of Assimp_Texture.AI_Texture;
         pragma Convention (C, Textures_Array);
         type Textures_Ptr  is access Textures_Array;
         pragma Convention (C, Textures_Ptr);
--           Textures_Array_Size : Interfaces.C.size_t := Textures_Array'Size;

         type Lights_Array is array (1 .. Sizes.Num_Lights) of Light.AI_Light;
         pragma Convention (C, Lights_Array);
         type Lights_Ptr  is access Lights_Array;
         pragma Convention (C, Lights_Ptr);
--           Lights_Array_Size : Interfaces.C.size_t := Lights_Array'Size;

         type Cameras_Array is array (1 .. Sizes.Num_Cameras) of Camera.AI_Camera;
         pragma Convention (C, Cameras_Array);
         type Cameras_Ptr  is access Cameras_Array;
         pragma Convention (C, Cameras_Ptr);
--           Cameras_Array_Size : Interfaces.C.size_t := Cameras_Array'Size;

         type Sized_Scene is record
            Flags          : Interfaces.C.unsigned := 0;
            Root_Node      : Node_Ptr := Null;
            Num_Meshes     : Interfaces.C.unsigned := 0;
            Meshes         : Meshes_Ptr := Null;
            Num_Materials  : Interfaces.C.unsigned := 0;
            Materials      : Materials_Ptr := Null;
            Num_Animations : Interfaces.C.unsigned := 0;
            Animations     : Animations_Ptr := Null;
            Num_Textures   : Interfaces.C.unsigned := 0;
            Textures       : Textures_Ptr := Null;
            Num_Lights     : Interfaces.C.unsigned := 0;
            Lights         : Lights_Ptr := Null;
            Num_Cameras    : Interfaces.C.unsigned := 0;
            Cameras        : Cameras_Ptr := Null;
            Private_Data   : Interfaces.C.Strings.chars_ptr
              := Interfaces.C.Strings.Null_Ptr;
         end record;
         pragma Convention (C_Pass_By_Copy, Sized_Scene);

         function Import_File (File_Name : Interfaces.C.Strings.chars_ptr;
                               Flags : Interfaces.C.unsigned)
                               return access Sized_Scene;
         pragma Import (C, Import_File, "aiImportFile");

--           procedure To_AI_Scene (C_Scene : Sized_Scene;
--                                  theScene : in out AI_Scene) is
--              theScene : AI_Scene (Index_Int (C_Scene.Num_Meshes),
--                                   Index_Int (C_Scene.Num_Materials),
--                                   Index_Int (C_Scene.Num_Animations),
--                                   Index_Int (C_Scene.Num_Textures),
--                                   Index_Int (C_Scene.Num_Lights),
--                                   Index_Int (C_Scene.Num_Cameras));
--            begin
--              Put_Line ("C_Import.To_AI_Scene entered.");
--              theScene.Flags := C_Scene.Flags;
--        theScene.Root_Node := C_Scene.Root_Node.all;
--        theScene.Meshes := C_Scene.Meshes.all;
--        theScene.Materials := C_Scene.Materials.all;
--        theScene.Animations := C_Scene.Animations.all;
--        theScene.Textures := C_Scene.Textures.all;
--        theScene.Lights := C_Scene.Lights.all;
--        theScene.Cameras := C_Scene.Cameras.all;
--              theScene.Private_Data := Ada.Strings.Unbounded.To_Unbounded_String
--                (Interfaces.C.Strings.Value (C_Scene.Private_Data));

--           exception
--              when  others =>
--                 Put_Line ("An exception occurred in C_Import.To_AI_Scene.");
--                 raise;
--           end To_AI_Scene;

         S_Scene  : Sized_Scene;

      begin
         S_Scene := Import_File
           (Interfaces.C.Strings.New_String (File_Name), unsigned (Flags)).all;

         Put ("C_Import.Import_File, Num_Meshes, Num_Materials, Num_Animations");
         Put_Line (", Num_Textures, Num_Lights, Num_Cameras:");
         Put_Line (unsigned'Image (S_Scene.Num_Meshes) &
                   unsigned'Image (S_Scene.Num_Materials) &
                   unsigned'Image (S_Scene.Num_Animations) &
                   unsigned'Image (S_Scene.Num_Textures) &
                   unsigned'Image (S_Scene.Num_Lights) &
                   unsigned'Image (S_Scene.Num_Cameras));
         declare
            theScene : AI_Scene;
--              theScene : AI_Scene (Index_Int (Sizes.Num_Meshes),
--                                   Index_Int (Sizes.Num_Materials),
--                                   Index_Int (Sizes.Num_Animations),
--                                   Index_Int (Sizes.Num_Textures),
--                                   Index_Int (Sizes.Num_Lights),
--                                   Index_Int (Sizes.Num_Cameras));
         begin
            Put_Line ("C_Import.Import_File calling To_AI_Scene.");
--              To_AI_Scene (S_Scene, theScene);
--              theScene.Flags := S_Scene.Flags;
            return theScene;
         end;
      end;

   exception
      when  others =>
         Put_Line ("An exception occurred in C_Import.Import_File.");
         raise;
   end Import_File;

end C_Import;
