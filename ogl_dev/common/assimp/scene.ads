
with Interfaces.C;
with Interfaces.C.Pointers;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

with GL.Types;

with Animation;
with Assimp_Texture;
with Assimp_Types; use Assimp_Types;
with Camera;
with Light;
with Material;
with API_Vectors_Matrices;
with Assimp_Mesh;
with Metadata;

package Scene is

    --     package Mesh_Address_Conversion is new
    --       System.Address_To_Access_Conversions (Mesh.API_Mesh_Array);

    type Cameras_Ptr is access Camera.API_Camera_Array;
    type Lights_Ptr is access Light.API_Light_Array;
    type Materials_Ptr is access Material.API_Material_Array;
    type Textures_Ptr is access Assimp_Texture.API_Texture_Array;

    type API_Node_Ptr is private;

    type AI_Node;
    type Node_Ptr is access AI_Node;
    pragma Convention (C, Node_Ptr);

    type AI_Node is record
        Name           : Ada.Strings.Unbounded.Unbounded_String;
        Transformation : GL.Types.Singles.Matrix4;
        --        Parent         : Node_Ptr := Null;
        --        Children       : AI_Node_Array_Ptr := Null;
        Meshes         : Assimp_Mesh.AI_Mesh_Map;
        Meta_Data      : Metadata.AI_Metadata;
    end record;

    package AI_Nodes_Package is new
      Ada.Containers.Doubly_Linked_Lists (AI_Node);
    type AI_Nodes_List is new AI_Nodes_Package.List with null Record;

    type AI_Scene is record
        Flags          : Interfaces.C.unsigned := 0;
        Nodes          : AI_Nodes_List;
        Meshes         : Assimp_Mesh.AI_Mesh_Map;
        Materials      : Material.AI_Material_Map;
        Animations     : Animation.AI_Animation_Map;
        Textures       : Assimp_Texture.AI_Texture_Map;
        Lights         : Light.AI_Light_Map;
        Cameras        : Camera.AI_Camera_Map;
        Private_Data   : Ada.Strings.Unbounded.Unbounded_String :=
                           Ada.Strings.Unbounded.To_Unbounded_String ("");
    end record;

    type API_Node_Array;
    type API_Node is record
        Name           : Assimp_Types.AI_String;
        Transformation : API_Vectors_Matrices.API_Matrix_4D;
        Parent         : access API_Node := Null;
        Num_Children   : Interfaces.C.unsigned := 0;
        Children       : access API_Node_Array := Null;
        Num_Meshes     : Interfaces.C.unsigned := 0;
        Meshes         : access Assimp_Mesh.API_Mesh_Array := Null;
        Meta_Data      : access Metadata.API_Metadata := Null;
    end record;
    pragma Convention (C_Pass_By_Copy, API_Node);

    type API_Node_Array is array (Interfaces.C.unsigned range <>) of aliased API_Node;
    pragma Convention (C, API_Node_Array);

   package Node_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Node, API_Node_Array, API_Node'(others => <>));

    procedure To_Node_List (Root_Node : API_Node;
                            Nodes : in out Scene.AI_Nodes_List);

private

    type API_Node_Ptr is access API_Node;

end Scene;
