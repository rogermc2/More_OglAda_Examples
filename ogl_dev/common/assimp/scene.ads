
with Interfaces.C;
with Interfaces.C.Pointers;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

with GL.Types;

with Animation;
with Assimp_Texture;
with Assimp_Types;
with Camera;
with Light;
with Material;
with API_Vectors_Matrices;
with Assimp_Mesh;
with Metadata;

package Scene is

    --     package Mesh_Address_Conversion is new
    --       System.Address_To_Access_Conversions (Mesh.API_Mesh_Array);
    type Scene_Flags is (AI_Scene_Flags_Incomplete, AI_Scene_Flags_Validated,
                         AI_Scene_Flags_Validation_Warning, AI_Scene_Flags_Non_Verbose_Format,
                         AI_Scene_Flags_Terrain);
   pragma Convention (C, Scene_Flags);

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
         end record;
         pragma Convention (C_Pass_By_Copy, API_Scene);
    procedure To_AI_Scene (C_Scene : in out API_Scene;
                           theScene : in out Scene.AI_Scene);
    procedure To_Node_List (Root_Node : in out API_Node;
                            Nodes : in out Scene.AI_Nodes_List);

private

    type API_Node_Ptr is access API_Node;

    for Scene_Flags use (AI_Scene_Flags_Incomplete         => 1,
                         AI_Scene_Flags_Validated          => 2,
                         AI_Scene_Flags_Validation_Warning => 4,
                         AI_Scene_Flags_Non_Verbose_Format => 8,
                         AI_Scene_Flags_Terrain            => 16);
end Scene;
