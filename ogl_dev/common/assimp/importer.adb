
with Ada.Text_IO; use Ada.Text_IO;

with Assimp.API;
with Assimp_Mesh;

package body Importer is

    -------------------------------------------------------------------------

    --  Read a file  into a scene with the Assimp aiImportFile function
    function Import_File (File_Name : String; Flags : GL.Types.UInt)
                          return Scene.AI_Scene is
        use Scene;
        C_Scene  : API_Scene;
        theScene : AI_Scene;
    begin
        C_Scene := Assimp.API.Import_File
          (Interfaces.C.Strings.New_String (File_Name), unsigned (Flags)).all;

        Put ("Importer.Import_File, Num_Meshes, Num_Materials, Num_Animations");
        Put_Line (", Num_Textures, Num_Lights, Num_Cameras:");
        Put_Line (unsigned'Image (C_Scene.Num_Meshes) &
                    unsigned'Image (C_Scene.Num_Materials) &
                    unsigned'Image (C_Scene.Num_Animations) &
                    unsigned'Image (C_Scene.Num_Textures) &
                    unsigned'Image (C_Scene.Num_Lights) &
                    unsigned'Image (C_Scene.Num_Cameras));

        Put_Line ("Importer.Import_File calling To_AI_Scene.");
        To_AI_Scene (C_Scene, theScene);
        Put_Line ("Importer.Import_File returned from To_AI_Scene.");
        return theScene;

    exception
        when  others =>
            Put_Line ("An exception occurred in Importer.Import_File.");
            raise;
    end Import_File;

    ------------------------------------------------------------------------

    function Read_File (File_Name : String; Flags : GL.Types.UInt) return Scene.AI_Scene is
        use Scene;
        C_Scene   : API_Scene;
        theScene  : AI_Scene;
        C_Mesh_Ptr : access Assimp_Mesh.Mesh_Array_Pointer;
        C_Mesh    : Assimp_Mesh.API_Mesh;
        Prim      : Interfaces.C.unsigned;
        Num_Meshes : Interfaces.C.unsigned;
    begin
        C_Scene := Assimp.API.Read_File
          (Interfaces.C.Strings.New_String (File_Name), unsigned (Flags)).all;
        Num_Meshes := C_Scene.Num_Meshes;

        Put ("Importer.Read_File, Num_Meshes, Num_Materials, Num_Animations");
        Put_Line (", Num_Textures, Num_Lights, Num_Cameras:");
        Put_Line (unsigned'Image (C_Scene.Num_Meshes) &
                    unsigned'Image (C_Scene.Num_Materials) &
                    unsigned'Image (C_Scene.Num_Animations) &
                    unsigned'Image (C_Scene.Num_Textures) &
                    unsigned'Image (C_Scene.Num_Lights) &
                    unsigned'Image (C_Scene.Num_Cameras));
        C_Mesh_Ptr := C_Scene.Meshes;
        C_Mesh := Assimp_Mesh.Mesh_Array_Pointers.Value
             (C_Scene.Meshes.all, ptrdiff_t (Num_Meshes)) (0);
        Prim := C_Scene.Meshes.all.Primitive_Types;
        Put_Line ("Importer.Read_File, C_Scene Primitive_Types, Num Vertices, Faces, UV_Components, Bones, Anim_Meshes");
        Put_Line (unsigned'Image (Prim) &
                  unsigned'Image (C_Mesh.Num_Vertices) &
                  unsigned'Image (C_Mesh.Num_Faces) &
                  unsigned'Image (C_Mesh.Num_UV_Components) &
                  unsigned'Image (C_Mesh.Num_Bones) &
                  unsigned'Image (C_Mesh.Num_Anim_Meshes));
        New_Line;
        To_AI_Scene (C_Scene, theScene);
        return theScene;
    exception
        when  others =>
            Put_Line ("An exception occurred in Importer.Read_File.");
            raise;
    end Read_File;

    ------------------------------------------------------------------------

end Importer;
