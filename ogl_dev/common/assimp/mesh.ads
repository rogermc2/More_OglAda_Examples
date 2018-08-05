
with Interfaces.C.Pointers;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;

with Interfaces.C;

with GL.Types; use GL.Types;

with Assimp_Types; use Assimp_Types;
with Matrix_4x4;
with API_Vectors;
with Ogldev_Math;

package Mesh is

   type AI_Anim_Mesh is record
      Vertices        : Singles.Vector3;
      Normals         : Singles.Vector3;
      Tangents        : Singles.Vector3;
      Bitangents      : Singles.Vector3;
      Colours         : Singles.Vector4_Array (1 .. 8);
      Texture_Coords  : Singles.Vector2_Array (1 .. 8);
      Num_Indices     : UInt := 0;
   end record;

   package Anim_Mesh_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, AI_Anim_Mesh);
   type AI_Anim_Mesh_Map is new Anim_Mesh_Package.Map with null Record;

   type API_Anim_Mesh is private;
   type API_Anim_Mesh_Array is array (Interfaces.C.unsigned range <>) of
     access API_Anim_Mesh;
   pragma Convention (C, API_Anim_Mesh_Array);

   type AI_Vertex_Weight is record
      Vertex_ID   : UInt := 0;
      Weight      : Single := 0.0;
   end record;

   package Face_Index_Package is new
    Ada.Containers.Doubly_Linked_Lists (UInt);
   type AI_Face_Indices is new Face_Index_Package.List with null record;

   package Faces_Package is new
    Ada.Containers.Indefinite_Ordered_Maps (UInt, AI_Face_Indices);
   type AI_Faces is new Faces_Package.Map with null record;

   package Weights_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, AI_Vertex_Weight);
   type Weights_Map is new Weights_Package.Map with null Record;

   type Bone is record
      Name          : Ada.Strings.Unbounded.Unbounded_String;
      Weights       : Weights_Map;
      Offset_Matrix : Matrix_4x4.AI_Matrix_4x4;
   end record;

   package Bone_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, Bone);
   type Bone_Map is new Bone_Package.Map with null Record;

   type AI_Mesh is record
      Primitive_Types   : UInt;
      Vertices          : Ogldev_Math.AI_3D_Map;
      Normals           : Ogldev_Math.AI_3D_Map;
      Tangents          : Ogldev_Math.AI_3D_Map;
      Bitangents        : Ogldev_Math.AI_3D_Map;
      Colours           : Singles.Vector4;
      Texture_Coords    : Ogldev_Math.AI_2D_Map;
      Num_UV_Components : UInt;
      Faces             : AI_Faces;
      Bones             : Bone_Map;
      Material_Index    : UInt;
      Name              : Assimp_Types.AI_String;
      Anim_Meshes       : AI_Anim_Mesh_Map;
   end record;

   package Mesh_Package is new Ada.Containers.Indefinite_Ordered_Maps (UInt, AI_Mesh);
   type AI_Mesh_Map is new Mesh_Package.Map with null Record;

   type API_Face is private;
   type API_Face_Array is array (Interfaces.C.unsigned range <>) of access API_Face;
   pragma Convention (C, API_Face_Array);

   type API_Bone is private;
   type API_Bone_Array is array (Interfaces.C.unsigned range <>) of access API_Bone;
   pragma Convention (C, API_Bone_Array);

 type API_Mesh is record
      Primitive_Types   : Interfaces.C.unsigned := 0;
      Num_Vertices      : Interfaces.C.unsigned := 0;
      Num_Faces         : Interfaces.C.unsigned := 0;
      Vertices          : access Vector3_Array;
      Normals           : access Vector3_Array;
      Tangents          : access Vector3_Array;
      Bitangents        : access Vector3_Array;
      Colours           : access API_Vectors.API_Colour_4D;
      Texture_Coords    : access Texture_Coords_Array;
      Num_UV_Components : Interfaces.C.unsigned := 0;
      Faces             : access API_Face_Array;
      Num_UBones        : Interfaces.C.unsigned := 0;
      Bones             : access API_Bone_Array;
      Material_Index    : Interfaces.C.unsigned := 0;
      Name              : Assimp_Types.AI_String;
      Num_Anim_Meshes   : Interfaces.C.unsigned := 0;
      Anim_Meshes       : access API_Anim_Mesh_Array;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Mesh);

   type API_Mesh_Array is array (Interfaces.C.unsigned range <>) of aliased API_Mesh;
   pragma Convention (C, API_Mesh_Array);

   package Mesh_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Mesh, API_Mesh_Array, API_Mesh'(others => <>));

   function To_AI_Mesh (C_Mesh : API_Mesh) return AI_Mesh;
   function To_AI_Mesh_Map (Num_Meshes : Interfaces.C.unsigned := 0;
                            C_Array : API_Mesh_Array) return AI_Mesh_Map;

private

   type Num_UV_Components_Array is array (1 .. 8) of Interfaces.C.unsigned;

   type API_Anim_Mesh is record
      Vertices        : API_Vectors.API_Vector_3D;
      Normals         : API_Vectors.API_Vector_3D;
      Tangents        : API_Vectors.API_Vector_3D;
      Bitangents      : API_Vectors.API_Vector_3D;
      Colours         : Colors_Array;
      Texture_Coords  : Texture_Coords_Array;
      Num_Vertices    : Interfaces.C.unsigned := 0;
      Weight          : Interfaces.C.C_float := 0.0;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Anim_Mesh);

   type API_Bone is record
      Name          : Assimp_Types.AI_String;
      Num_Weights   : Interfaces.C.unsigned := 0;
      Weights       : access AI_Vertex_Weight;
      Offset_Matrix : Matrix_4x4.AI_Matrix_4x4;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Bone);

   type API_Face is record
      Num_Indices : Interfaces.C.unsigned := 0;
      Indices     : access Unsigned_Array;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Face);

   type API_Vertex_Weight is record
      Vertex_ID   : Interfaces.C.unsigned := 0;
      Weight      : Interfaces.C.C_float := 0.0;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Vertex_Weight);

   type API_Weight_Array is array (Interfaces.C.unsigned range <>) of
     access API_Vertex_Weight;
   pragma Convention (C, API_Weight_Array);

end Mesh;
