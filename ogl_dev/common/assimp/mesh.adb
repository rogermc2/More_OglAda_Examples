
with Assimp_Util; use Assimp_Util;

package body Mesh is

   function To_Texture_Coords_Map (Num_Coords : Interfaces.C.unsigned := 0;
                                   C_Array : Texture_Coords_Array)
                                   return Ogldev_Math.AI_2D_Map;

   --  ------------------------------------------------------------------------

--     function Default_Mesh return API_Mesh is
--        use Interfaces.C;
--        Empty_String : char_array (0 .. 0);
--     begin
--        Empty_String (0) := nul;
--        return (0, 0, 0, null, null, null, null, null, null, 0, null, 0,
--                null, 0, (0, Empty_String), 0, null);
--        Vertices          : access Vector3_Array;
--        Normals           : access Vector3_Array;
--        Tangents          : access Vector3_Array;
--        Bitangents        : access Vector3_Array;
--        Colours           : access AI_Vectors.AI_Colour_4D;
--        Texture_Coords    : access Texture_Coords_Array;
--        Num_UV_Components : Interfaces.C.unsigned := 0;
--        Faces             : access API_Face_Array;
--        Num_UBones        : Interfaces.C.unsigned := 0;
--        Bones             : access API_Bone_Array;
--        Material_Index    : Interfaces.C.unsigned := 0;
--        Name              : Assimp_Types.AI_String;
--        Num_Anim_Meshes   : Interfaces.C.unsigned := 0;
--        Anim_Meshes       : access API_Anim_Mesh_Array;);
--     end Default_Mesh;

   --  ------------------------------------------------------------------------

  function To_AI_Anim_Mesh_Map (Num_Meshes : Interfaces.C.unsigned := 0;
                                 C_Array : API_Anim_Mesh_Array)
                                 return AI_Anim_Mesh_Map is
      Meshes    : AI_Anim_Mesh_Map;
      Anim_Mesh : AI_Anim_Mesh;
      C_Mesh    : API_Anim_Mesh;
   begin
      for index in 1 .. Num_Meshes loop
         C_Mesh := C_Array (Interfaces.C.unsigned (index)).all;
         Anim_Mesh.Vertices := To_OGL_Vector3 (C_Mesh.Vertices);
         Anim_Mesh.Normals := To_OGL_Vector3 (C_Mesh.Normals);
         Anim_Mesh.Tangents := To_OGL_Vector3 (C_Mesh.Tangents);
         Anim_Mesh.Bitangents := To_OGL_Vector3 (C_Mesh.Bitangents);
         for index2 in C_Mesh.Colours'Range loop
            Anim_Mesh.Colours (Int (index2)) :=
              To_Colour4D (C_Mesh.Colours (index2).all);
         end loop;
         for index2 in C_Mesh.Texture_Coords'Range loop
            Anim_Mesh.Texture_Coords (Int (index2)) :=
              To_OGL_Vector2 (C_Mesh.Texture_Coords (index2).all);
         end loop;
         Meshes.Insert (UInt (index), Anim_Mesh);
      end loop;
      return Meshes;
   end To_AI_Anim_Mesh_Map;

   --  ------------------------------------------------------------------------

   function To_AI_Face_Indices (Num_Faces : Interfaces.C.unsigned := 0;
                                C_Array : API_Face_Array) return AI_Faces is
      --  An API_Face_Array is an array of of API_Face.
      --  An API_Face contains an array of vertex indice.
      Faces       : AI_Faces;
      C_Face      : API_Face;
      Num_Indices : UInt;
   begin
      for index in 1 .. Num_Faces loop
         C_Face := C_Array (Interfaces.C.unsigned (index)).all;
         Num_Indices := UInt (C_Face.Num_Indices);
         declare
            use Face_Index_Package;
            Indices_Array : constant Unsigned_Array (1 .. Num_Indices) :=
                            C_Face.Indices.all;
            aFace         : AI_Face_Indices;
         begin
            for index2 in Indices_Array'Range loop
               aFace.Append (UInt (Indices_Array (index2).all));
            end loop;
            Faces.Insert (UInt (index), aFace);
         end;
      end loop;
      return Faces;
   end To_AI_Face_Indices;

   --  ------------------------------------------------------------------------

   function To_AI_Bone_Map (Num_Bones : Interfaces.C.unsigned := 0;
                            C_Array : API_Bone_Array) return Bone_Map is
      Bones       : Bone_Map;
      C_Bone      : API_Bone;
   begin
      for index in 1 .. Num_Bones loop
         C_Bone := C_Array (Interfaces.C.unsigned (index)).all;
         declare
            aBone    : Bone;
            Weights  : Weights_Map;
            aWeight  : AI_Vertex_Weight;
         begin
            aBone.Name := Ada.Strings.Unbounded.To_Unbounded_String
              (Interfaces.C.To_Ada (C_Bone.Name.Data));
            aBone.Offset_Matrix := C_Bone.Offset_Matrix;
            for index2 in 1 .. C_Bone.Num_Weights loop
               aWeight.Vertex_ID := Weights.Element (UInt (index2)).Vertex_ID;
               aWeight.Weight := Weights.Element (UInt (index2)).Weight;
               aBone.Weights.Insert (UInt (index2), aWeight);
            end loop;
            Bones.Insert (UInt (index), aBone);
         end;
      end loop;
      return Bones;
   end To_AI_Bone_Map;

   --  ------------------------------------------------------------------------

   function To_AI_Mesh (C_Mesh : API_Mesh) return AI_Mesh is
      use Ogldev_Math;
      theMesh : AI_Mesh;
   begin
      theMesh.Primitive_Types := UInt (C_Mesh.Primitive_Types);
      theMesh.Vertices := To_AI_Map3D (UInt (C_Mesh.Num_Vertices),
                                        C_Mesh.Vertices.all);
      theMesh.Normals := To_AI_Map3D (UInt (C_Mesh.Num_Vertices),
                                      C_Mesh.Normals.all);
      theMesh.Tangents := To_AI_Map3D (UInt (C_Mesh.Num_Vertices),
                                       C_Mesh.Tangents.all);
      theMesh.Bitangents := To_AI_Map3D (UInt (C_Mesh.Num_Vertices),
                                         C_Mesh.Bitangents.all);
      theMesh.Colours := To_Colour4D (C_Mesh.Colours.all);
      theMesh.Texture_Coords :=
        To_Texture_Coords_Map (C_Mesh.Num_Vertices,
                               C_Mesh.Texture_Coords.all);
      theMesh.Num_UV_Components := UInt (C_Mesh.Num_UV_Components);
      theMesh.Faces := To_AI_Face_Indices (C_Mesh.Num_Faces, C_Mesh.Faces.all);
      theMesh.Bones := To_AI_Bone_Map (C_Mesh.Num_UBones, C_Mesh.Bones.all);
      theMesh.Material_Index := UInt (C_Mesh.Material_Index);
      theMesh.Name := C_Mesh.Name;
      theMesh.Anim_Meshes := To_AI_Anim_Mesh_Map (C_Mesh.Num_Anim_Meshes,
                                                  C_Mesh.Anim_Meshes.all);
      return theMesh;
   end To_AI_Mesh;

   --  ------------------------------------------------------------------------

   function To_AI_Mesh_Map (Num_Meshes : Interfaces.C.unsigned := 0;
                            C_Array : API_Mesh_Array) return AI_Mesh_Map is
      Meshes : AI_Mesh_Map;
      aMesh  : AI_Mesh;
   begin
      for index in 1 .. Num_Meshes loop
         aMesh := To_AI_Mesh (C_Array (index));
         Meshes.Insert (UInt (index), aMesh);
      end loop;
      return Meshes;
   end To_AI_Mesh_Map;

   --  ------------------------------------------------------------------------

   function To_Texture_Coords_Map (Num_Coords : Interfaces.C.unsigned := 0;
                                   C_Array : Texture_Coords_Array)
                                      return  Ogldev_Math.AI_2D_Map is
      Texture_Coords : Ogldev_Math.AI_2D_Map;
      C_Vec          : API_Vectors.API_Vector_2D;
   begin
      for index in 1 .. Num_Coords loop
         C_Vec := C_Array (Integer (index)).all;
         Texture_Coords.Insert (UInt (index),
                                (Single (C_Vec.X), Single (C_Vec.Y)));
      end loop;
      return Texture_Coords;
   end To_Texture_Coords_Map;

   --  ------------------------------------------------------------------------

end Mesh;
