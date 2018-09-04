
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with Ogldev_Math;

package body Scene is

    procedure To_Node_List (Root_Node : in out API_Node;
                            Nodes : in out AI_Nodes_List) is
        Name_Length : constant size_t := Root_Node.Name.Length;
        aNode   : AI_Node;
    begin
        Put_Line ("Scene.To_Node_List, Name length: " & size_t'Image (Name_Length));
        if Name_Length > 0 then
            aNode.Name :=  Ada.Strings.Unbounded.To_Unbounded_String
              (Interfaces.C.To_Ada (Root_Node.Name.Data));
        end if;
        Put_Line ("Scene.To_Node_List, Name: " & Ada.Strings.Unbounded.To_String (aNode.Name));
        aNode.Transformation :=
          Ogldev_Math.To_GL_Matrix4 (Root_Node.Transformation);
--          aNode.Meshes :=
--            Assimp_Mesh.To_AI_Mesh_Map (Root_Node.Num_Meshes, Root_Node.Meshes.all);
        Nodes.Append (aNode);

        for index in 1 .. Root_Node.Num_Children loop
            null;
        end loop;

    exception
        when  others =>
            Put_Line ("An exception occurred in Scene.To_Node_List.");
            raise;
    end To_Node_List;

    --  -------------------------------------------------------------------------

   procedure To_AI_Scene (C_Scene : in out API_Scene;
                          theScene : in out Scene.AI_Scene) is
        use Material.Material_Pointers_Package;
        Num_Meshes     : constant unsigned := (C_Scene.Num_Meshes);
        Num_Materials  : constant unsigned := (C_Scene.Num_Materials);
        Num_Animations : constant unsigned := (C_Scene.Num_Animations);
        Num_Textures   : constant unsigned := (C_Scene.Num_Textures);
        Num_Lights     : constant unsigned := (C_Scene.Num_Lights);
        Num_Cameras    : constant unsigned := (C_Scene.Num_Cameras);

        C_Mesh_Array  : Assimp_Mesh.API_Mesh_Array (1 .. Num_Meshes)
          := Assimp_Mesh.Mesh_Array_Pointers.Value
          (C_Scene.Meshes.all, ptrdiff_t (Num_Meshes));

        C_Materials_Array : Material.API_Material_Array  (1 .. Num_Materials)
          := Material.Material_Pointers_Package.Value (C_Scene.Materials.all, ptrdiff_t (Num_Materials));

        C_Root_Node : Scene.API_Node
          := Scene.Node_Pointers.Value (C_Scene.Root_Node, 1) (0);
   begin
        Put ("Scene.To_AI_Scene, Num_Meshes, Num_Materials, Num_Animations ");
        Put_Line ("Num_Textures, Num_Lights, Num_Cameras:");
        Put_Line (unsigned'Image (Num_Meshes) & unsigned'Image (Num_Materials) &
                  unsigned'Image (Num_Animations) & unsigned'Image (Num_Textures) &
                  unsigned'Image (Num_Lights) & unsigned'Image (Num_Cameras));

      Put_Line ("Scene.To_AI_Scene, C_Mesh_Array (1):");
      Put ("Primitive_Types, Num_Vertices, Num_Faces, Num_UV_Components: ");
        Put_Line (unsigned'Image (C_Mesh_Array (1).Primitive_Types) &
                   unsigned'Image (C_Mesh_Array (1).Num_Vertices) &
                   unsigned'Image (C_Mesh_Array (1).Num_Faces) &
                   unsigned'Image (C_Mesh_Array (1).Num_UV_Components));
         Put ("Num_Animations, Num_Bones, Material_Index: ");
         Put_Line (unsigned'Image (C_Mesh_Array (1).Num_Bones) &
                   unsigned'Image (C_Mesh_Array (1).Num_Anim_Meshes) &
                   unsigned'Image (C_Mesh_Array (1).Material_Index));
      New_Line;

        theScene.Flags := C_Scene.Flags;
        Scene.To_Node_List (C_Root_Node, theScene.Nodes);
        Put_Line ("Scene.To_AI_Scene, calling To_AI_Mesh_Map, C_Mesh_Array size: "  &
                    GL.Types.uint'Image (C_Mesh_Array'Length));
        theScene.Meshes := Assimp_Mesh.To_AI_Mesh_Map (Num_Meshes, C_Mesh_Array);
        Put_Line ("Scene.To_AI_Scene, Meshes loaded");
        theScene.Materials :=
          Material.To_AI_Materials_Map (Num_Materials, C_Materials_Array);
        Put_Line ("Scene.To_AI_Scene, Materials_Map loaded");

        if Num_Textures > 0 then
            declare
                C_Texture_Array : constant Assimp_Texture.API_Texture_Array :=
                Assimp_Texture.Texture_Pointers.Value (C_Scene.Textures.all, ptrdiff_t (Num_Textures));
            begin
                theScene.Textures :=
                  Assimp_Texture.To_AI_Texture_Map (Num_Textures, C_Texture_Array);
            end;
        end if;

        if Num_Animations > 0 then
            Put_Line ("Scene.To_AI_Scene, calling To_AI_Animation_Map");
            declare
                C_Animation_Array : constant Animation.API_Animation_Array :=
                Animation.Animation_Pointers.Value (C_Scene.Animations.all, ptrdiff_t (Num_Animations));
            begin
            theScene.Animations :=
              Animation.To_AI_Animation_Map (Num_Animations, C_Animation_Array);
            end;
        end if;

        if Num_Lights > 0 then
            declare
                C_Light_Array : constant Light.API_Light_Array
                  := Light.Light_Pointers.Value (C_Scene.Lights.all, ptrdiff_t (Num_Lights));
            begin
                theScene.Lights :=
                  Light.To_AI_Light_Map (Num_Lights, C_Light_Array);
            end;
        end if;

        if Num_Cameras > 0 then
            declare
                C_Camera_Array : constant Camera.API_Camera_Array
                  := Camera.Camera_Pointers.Value
                    (C_Scene.Cameras.all, ptrdiff_t (Num_Cameras));
            begin
                theScene.Cameras :=
                  Camera.To_AI_Camera_Map (Num_Cameras, C_Camera_Array);
            end;
        end if;

    exception
        when  others =>
            Put_Line ("An exception occurred in Scene.To_AI_Scene.");
            raise;
    end To_AI_Scene;

end Scene;
