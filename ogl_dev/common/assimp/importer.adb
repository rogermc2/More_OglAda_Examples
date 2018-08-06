
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Assimp.API;

package body Importer is

    procedure To_AI_Scene (C_Scene : API_Scene;
                           theScene : in out Scene.AI_Scene);

    --  -------------------------------------------------------------------------

    -- Read a file  into a scene with the Assimp aiImportFile function
    function Import_File (File_Name : String; Flags : GL.Types.UInt)
                          return Scene.AI_Scene is
        use Scene;

        C_Scene   : API_Scene;
        theScene  : AI_Scene;
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
        return theScene;

    exception
        when  others =>
            Put_Line ("An exception occurred in Importer.Import_File.");
            raise;
    end Import_File;

    --  ------------------------------------------------------------------------

    function Read_File (File_Name : String; Flags : GL.Types.UInt) return Scene.AI_Scene is
        use Scene;
        C_Scene   : API_Scene;
        theScene  : AI_Scene;
    begin
        C_Scene := Assimp.API.Read_File
          (Interfaces.C.Strings.New_String (File_Name), unsigned (Flags)).all;
        To_AI_Scene (C_Scene, theScene);
        return theScene;
    exception
        when  others =>
            Put_Line ("An exception occurred in Importer.Read_File.");
            raise;
    end Read_File;

    --  ------------------------------------------------------------------------

    procedure To_AI_Scene (C_Scene : API_Scene;
                           theScene : in out Scene.AI_Scene) is
--          C_Mesh_Array : constant Assimp_Mesh.API_Mesh_Array
--            := Assimp_Mesh.Mesh_Pointers.Value
--              (C_Scene.Meshes, ptrdiff_t (C_Scene.Num_Meshes));
--          C_Materials_Array : constant Material.API_Material_Array
--            := Material.Material_Pointers.Value
--              (C_Scene.Materials, ptrdiff_t (C_Scene.Num_Materials));
--          C_Animation_Array : constant Animation.API_Animation_Array
--            := Animation.Animation_Pointers.Value
--              (C_Scene.Animations, ptrdiff_t (C_Scene.Num_Animations));
--          C_Texture_Array : constant Assimp_Texture.API_Texture_Array
--            := Assimp_Texture.Texture_Pointers.Value
--              (C_Scene.Textures, ptrdiff_t (C_Scene.Num_Textures));
--          C_Light_Array : constant Light.API_Light_Array
--            := Light.Light_Pointers.Value
--              (C_Scene.Lights, ptrdiff_t (C_Scene.Num_Lights));
--          C_Camera_Array : constant Camera.API_Camera_Array
--            := Camera.Camera_Pointers.Value
--              (C_Scene.Cameras, ptrdiff_t (C_Scene.Num_Cameras));
--          C_Root_Node : constant Scene.API_Node
--            := Scene.Node_Pointers.Value (C_Scene.Root_Node, 1) (0);
    begin
        null;
--          Put_Line ("C_Import.To_AI_Scene, setting Flags");
--          theScene.Flags := C_Scene.Flags;
--          Put_Line ("C_Import.To_AI_Scene, calling To_Node_List");
--          Scene.To_Node_List (C_Root_Node,  theScene.Nodes);
--          Put_Line ("C_Import.To_AI_Scene, calling To_AI_Mesh_Map");
--          theScene.Meshes := Mesh.To_AI_Mesh_Map (C_Scene.Num_Meshes, C_Mesh_Array);
--          Put_Line ("C_Import.To_AI_Scene, calling To_AI_Materials_Map");
--          theScene.Materials :=
--            Material.To_AI_Materials_Map (C_Scene.Num_Materials, C_Materials_Array);
--          Put_Line ("C_Import.To_AI_Scene, calling To_AI_Animation_Map");
--          theScene.Animations :=
--            Animation.To_AI_Animation_Map (C_Scene.Num_Animations, C_Animation_Array);
--          theScene.Textures :=
--            Assimp_Texture.To_AI_Texture_Map (C_Scene.Num_Textures, C_Texture_Array);
--          theScene.Lights :=
--            Light.To_AI_Light_Map (C_Scene.Num_Lights, C_Light_Array);
--          theScene.Cameras :=
--            Camera.To_AI_Camera_Map (C_Scene.Num_Cameras, C_Camera_Array);
--          theScene.Private_Data := Ada.Strings.Unbounded.To_Unbounded_String
--            (Interfaces.C.Strings.Value (C_Scene.Private_Data));

    exception
        when  others =>
            Put_Line ("An exception occurred in Importer.To_AI_Scene.");
            raise;
    end To_AI_Scene;

end Importer;
