
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with AI_Conversion;
with Assimp_Util;

with Ogldev_Math;

package body Scene is

   procedure To_Node_List (Root_Node : API_Node;
                           Nodes     : in out AI_Nodes_List) is
      Name_Length : constant size_t := Root_Node.Name.Length;
      aNode       : AI_Node;
   begin
      if Name_Length > 0 then
         aNode.Name :=  Ada.Strings.Unbounded.To_Unbounded_String
           (Interfaces.C.To_Ada (Root_Node.Name.Data));
      end if;
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

   procedure To_AI_Scene (C_Scene  : API_Scene;
                          theScene : in out Scene.AI_Scene) is
      use Material.Material_Pointers_Package;
      use Node_Pointers;
      Num_Meshes     : constant unsigned := (C_Scene.Num_Meshes);
      Num_Materials  : constant unsigned := (C_Scene.Num_Materials);
      Num_Animations : constant unsigned := (C_Scene.Num_Animations);
      Num_Textures   : constant unsigned := (C_Scene.Num_Textures);
      Num_Lights     : constant unsigned := (C_Scene.Num_Lights);
      Num_Cameras    : constant unsigned := (C_Scene.Num_Cameras);

      C_Material_Ptrs_Array : Material.API_Material_Ptr_Array (1 .. Num_Materials);
      C_Root_Node       : Scene.API_Node;
   begin
      Put ("Scene.To_AI_Scene, Num_Meshes, Num_Materials, Num_Animations, ");
      Put_Line ("Num_Textures, Num_Lights, Num_Cameras:");
      Put_Line (unsigned'Image (Num_Meshes) & unsigned'Image (Num_Materials) &
                  unsigned'Image (Num_Animations) & unsigned'Image (Num_Textures) &
                  unsigned'Image (Num_Lights) & unsigned'Image (Num_Cameras));

--        Put ("Primitive_Types, Num_Vertices, Num_Faces: ");
--        Put_Line (unsigned'Image (C_Mesh_Array (1).Primitive_Types) &
--                    unsigned'Image (C_Mesh_Array (1).Num_Vertices) &
--                    unsigned'Image (C_Mesh_Array (1).Num_Faces));
--        Put ("Num_Animations, Num_Bones, Material_Index: ");
--        Put_Line (unsigned'Image (C_Mesh_Array (1).Num_Bones) &
--                    unsigned'Image (C_Mesh_Array (1).Num_Anim_Meshes) &
--                    unsigned'Image (C_Mesh_Array (1).Material_Index));
--        New_Line;
--        for index in 1 .. C_Mesh_Array'Length loop
--           Put_Line ("Name length, string: Mesh" & Integer'Image (index) & ": " &
--                       size_t'Image (C_Mesh_Array (unsigned (index)).Name.Length) & " '" &
--                       Assimp_Util.To_String (C_Mesh_Array (1).Name) & " '");
--        end loop;
--        New_Line;

      theScene.Flags := C_Scene.Flags;
      if C_Scene.Root_Node /= null then
         C_Root_Node := Scene.Node_Pointers.Value (C_Scene.Root_Node, 1) (0);
         Scene.To_Node_List (C_Root_Node, theScene.Nodes);
      end if;

      theScene.Meshes := Assimp_Mesh.To_AI_Mesh_Map (Num_Meshes, C_Scene.Meshes);
      if Num_Materials > 0 then
         C_Material_Ptrs_Array := Material.Material_Pointers_Package.Value
          (C_Scene.Materials, ptrdiff_t (Num_Materials));
         theScene.Materials :=
           AI_Conversion.To_AI_Materials_Map (Num_Materials, C_Material_Ptrs_Array);
--           Put_Line ("Scene.To_AI_Scene theScene.Materials set, Num_Materials, Num_Textures: " &
--                    unsigned'Image (Num_Materials) & unsigned'Image (Num_Textures));
      end if;

      if Num_Textures > 0 then
              theScene.Textures :=
              Assimp_Texture.To_AI_Texture_Map (Num_Textures, C_Scene.Textures);
      end if;

      if Num_Animations > 0 then
           theScene.Animations :=
              Animation.To_AI_Animation_Map (Num_Animations, C_Scene.Animations);
      end if;

      if Num_Lights > 0 then
           theScene.Lights := Light.To_AI_Light_Map (Num_Lights, C_Scene.Lights);
      end if;

      if Num_Cameras > 0 then
            theScene.Cameras :=
              Camera.To_AI_Camera_Map (Num_Cameras, C_Scene.Cameras);
      end if;

   exception
      when  others =>
         Put_Line ("An exception occurred in Scene.To_AI_Scene.");
         raise;
   end To_AI_Scene;

end Scene;
