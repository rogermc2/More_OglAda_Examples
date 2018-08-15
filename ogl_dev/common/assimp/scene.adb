
with Interfaces.C; use Interfaces.C;

with Ada.Text_IO; use Ada.Text_IO;

with Ogldev_Math;

package body Scene is


    procedure To_Node_List (Root_Node : API_Node;
                            Nodes : in out AI_Nodes_List) is
--          C_Node  : API_Node;
        aNode   : AI_Node;
    begin
        aNode.Name :=  Ada.Strings.Unbounded.To_Unbounded_String
          (Interfaces.C.To_Ada (Root_Node.Name.Data));
        aNode.Transformation :=
          Ogldev_Math.To_GL_Matrix4 (Root_Node.Transformation);
--          aNode.Meshes :=
--            Assimp_Mesh.To_AI_Mesh_Map (Root_Node.Num_Meshes, Root_Node.Meshes.all);
        Nodes.Append (aNode);
        for index in 1 .. Root_Node.Num_Children loop
            null;
        end loop;
    end To_Node_List;

    --  -------------------------------------------------------------------------

   procedure To_AI_Scene (C_Scene : API_Scene;
                           theScene : in out Scene.AI_Scene) is
        C_Mesh_Array : constant Assimp_Mesh.API_Mesh_Array
          := Assimp_Mesh.Mesh_Array_Pointers.Value
            (C_Scene.Meshes, ptrdiff_t (C_Scene.Num_Meshes));
        C_Materials_Array : constant Material.API_Material_Array
          := Material.Material_Pointers.Value
            (C_Scene.Materials, ptrdiff_t (C_Scene.Num_Materials));
        C_Animation_Array : constant Animation.API_Animation_Array
          := Animation.Animation_Pointers.Value
            (C_Scene.Animations, ptrdiff_t (C_Scene.Num_Animations));
        C_Texture_Array : constant Assimp_Texture.API_Texture_Array
          := Assimp_Texture.Texture_Pointers.Value
            (C_Scene.Textures, ptrdiff_t (C_Scene.Num_Textures));
        C_Light_Array : constant Light.API_Light_Array
          := Light.Light_Pointers.Value
            (C_Scene.Lights, ptrdiff_t (C_Scene.Num_Lights));
        C_Camera_Array : constant Camera.API_Camera_Array
          := Camera.Camera_Pointers.Value
            (C_Scene.Cameras, ptrdiff_t (C_Scene.Num_Cameras));
        C_Root_Node : constant Scene.API_Node
          := Scene.Node_Pointers.Value (C_Scene.Root_Node, 1) (0);
    begin
        Put_Line ("C_Import.To_AI_Scene, setting Flags");
        theScene.Flags := C_Scene.Flags;
        Put_Line ("C_Import.To_AI_Scene, calling To_Node_List");
        Scene.To_Node_List (C_Root_Node,  theScene.Nodes);
        Put_Line ("C_Import.To_AI_Scene, calling To_AI_Mesh_Map");
        theScene.Meshes := Assimp_Mesh.To_AI_Mesh_Map (C_Scene.Num_Meshes, C_Mesh_Array);
        Put_Line ("C_Import.To_AI_Scene, calling To_AI_Materials_Map");
        theScene.Materials :=
          Material.To_AI_Materials_Map (C_Scene.Num_Materials, C_Materials_Array);
        Put_Line ("C_Import.To_AI_Scene, calling To_AI_Animation_Map");
        theScene.Animations :=
          Animation.To_AI_Animation_Map (C_Scene.Num_Animations, C_Animation_Array);
        theScene.Textures :=
          Assimp_Texture.To_AI_Texture_Map (C_Scene.Num_Textures, C_Texture_Array);
        theScene.Lights :=
          Light.To_AI_Light_Map (C_Scene.Num_Lights, C_Light_Array);
        theScene.Cameras :=
          Camera.To_AI_Camera_Map (C_Scene.Num_Cameras, C_Camera_Array);

    exception
        when  others =>
            Put_Line ("An exception occurred in Importer.To_AI_Scene.");
            raise;
    end To_AI_Scene;

end Scene;
