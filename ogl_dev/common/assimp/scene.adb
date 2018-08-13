
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

end Scene;
