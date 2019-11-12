
with Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with Ogldev_Math;

with Assimp_Util;

package body Mesh_Conversion is

   function To_AI_Colours_Map (C_Array      : API_Colour_4D_Ptr_Array;
                               Num_Vertices : Interfaces.C.unsigned)
                               return Assimp_Mesh.Colour_Coords_Map;
   function To_AI_Texture_Coords_Map (C_Array      : API_Texture_Coords_3D_Ptr_Array;
                                      Num_Vertices : Interfaces.C.unsigned)
                                      return Assimp_Mesh.Texture_Coords_Map;
   function To_AI_Vertices_Map (C_Array_Ptr  : Vector_3D_Array_Pointers.Pointer;
                                Num_Vertices : Interfaces.C.unsigned)
                                return Assimp_Mesh.Vertices_Map;
   function To_AI_Vertex_Weight_Map (Weights_Ptr : Vertex_Weight_Array_Pointer;
                                     Num_Weights : Interfaces.C.unsigned)
                                     return Assimp_Mesh.Vertex_Weight_Map;

   ------------------------------------------------------------------------

   function To_AI_Bones_Map (B_Array_Access : access Bones_Array_Pointer;
                             Num_Bones      : Interfaces.C.unsigned)
                             return Assimp_Mesh.Bones_Map is

      Bones_Array_Ptr : constant Bones_Array_Pointer := B_Array_Access.all;
      B_Array         : constant API_Bones_Array := Bones_Array_Pointers.Value
        (Bones_Array_Ptr, Interfaces.C.ptrdiff_t (Num_Bones));
      anAPI_Bone      : API_Bone;
      anAI_Bone       : Assimp_Mesh.AI_Bone;
      theMap          : Assimp_Mesh.Bones_Map;
   begin
      for index in 1 .. Num_Bones loop
         anAPI_Bone := B_Array (index);
         anAI_Bone.Name := Assimp_Util.To_Unbounded_String (anAPI_Bone.Name);
         anAI_Bone.Weights := To_AI_Vertex_Weight_Map (anAPI_Bone.Weights, anAPI_Bone.Num_Weights);
         anAI_Bone.Offset_Matrix := Ogldev_Math.To_GL_Matrix4 (anAPI_Bone.Offset_Matrix);
         theMap.Insert (UInt (index), anAI_Bone);
      end loop;
      return theMap;
   end To_AI_Bones_Map;

   --  ------------------------------------------------------------------------

   function To_AI_Colours_Map (C_Array      : API_Colour_4D_Ptr_Array;
                               Num_Vertices : Interfaces.C.unsigned)
                               return Assimp_Mesh.Colour_Coords_Map is
      use Colours_4D_Array_Pointers;
      API_Colours_Ptr   : Colours_4D_Array_Pointer;
      API_Colours_Array : API_Colours_4D_Array (1 .. Num_Vertices);
      API_Colours       : API_Vectors_Matrices.API_Colour_4D;
      Colours           : Singles.Vector4;
      aColours_Map      : Assimp_Mesh.Colours_Map;
      TheMap            : Assimp_Mesh.Colour_Coords_Map;
   begin
      for index in C_Array'First .. C_Array'Last loop
         if C_Array (index) /= null then
            API_Colours_Ptr := C_Array (index);
            API_Colours_Array :=
              Colours_4D_Array_Pointers.Value
                (API_Colours_Ptr, Interfaces.C.ptrdiff_t (Num_Vertices));
            for T_index in API_Colours_Array'First .. API_Colours_Array'Last loop
               API_Colours := API_Colours_Array (T_index);
               Colours (GL.X) := Single (API_Colours.R);
               Colours (GL.Y) := Single (API_Colours.G);
               Colours (GL.Z) := Single (API_Colours.B);
               Colours (GL.Z) := Single (API_Colours.A);
               aColours_Map.Insert (UInt (T_index), Colours);
            end loop;
            theMap.Insert (UInt (index), aColours_Map);
         end if;
      end loop;
      return TheMap;
   end To_AI_Colours_Map;

   --  ------------------------------------------------------------------------

   function To_AI_Face_Indices_Map (Indices_Ptr : Unsigned_Array_Pointer;
                                    Num_Indices : Interfaces.C.unsigned)
                                    return Assimp_Mesh.Indices_Map is
      Index_Array  : API_Unsigned_Array (1 .. Num_Indices);
      theMap       : Assimp_Mesh.Indices_Map;
   begin
      Index_Array := Unsigned_Array_Pointers.Value
        (Indices_Ptr, Interfaces.C.ptrdiff_t (Num_Indices));

      for index in 1 .. Num_Indices loop
         theMap.Insert (UInt (index), UInt (Index_Array (index)));
      end loop;
      return theMap;
   end To_AI_Face_Indices_Map;

   --  ------------------------------------------------------------------------

   function To_AI_Faces_Map (F_Array_Ptr : Faces_Array_Pointer;
                             Num_Faces   : Interfaces.C.unsigned)
                             return Assimp_Mesh.Faces_Map is
      use API_Faces_Array_Pointers;
      Face_Array   : API_Faces_Array (1 .. Num_Faces);
      anAPI_Face   : API_Face;
      anAI_Face    : Assimp_Mesh.AI_Face;
      theMap       : Assimp_Mesh.Faces_Map;
   begin
      if F_Array_Ptr = null then
         Put_Line ("Assimp_Mesh.To_AI_Faces_Map F_Array_Ptr is null.");
      else
         Face_Array := API_Faces_Array_Pointers.Value
           (F_Array_Ptr, Interfaces.C.ptrdiff_t (Num_Faces));

         for index in 1 .. Num_Faces loop
            anAPI_Face := Face_Array (index);
            anAI_Face.Indices := To_AI_Face_Indices_Map (anAPI_Face.Indices, anAPI_Face.Num_Indices);
            theMap.Insert (UInt (index), anAI_Face);
         end loop;
      end if;
      return theMap;

   exception
      when others =>
         Put_Line ("An exception occurred in Assimp_Mesh.To_AI_Faces_Map.");
         raise;
   end To_AI_Faces_Map;

   --  ------------------------------------------------------------------------

   function To_AI_Mesh (C_Mesh : API_Mesh) return Assimp_Mesh.AI_Mesh is
      use Interfaces.C;
      use Vector_3D_Array_Pointers;
      theAI_Mesh   : Assimp_Mesh.AI_Mesh;
      Num_Faces    : constant unsigned :=C_Mesh.Num_Faces;
      Num_Bones    : constant unsigned := C_Mesh.Num_Bones;
   begin
      theAI_Mesh.Name :=  Assimp_Util.To_Unbounded_String (C_Mesh.Name);

      if C_Mesh.Vertices = null then
         raise Strings.Dereference_Error with
           "To_AI_Mesh exception: C_Mesh.Vertices is null.";
      end if;
      theAI_Mesh.Vertices := To_AI_Vertices_Map (C_Mesh.Vertices, C_Mesh.Num_Vertices);

      if C_Mesh.Normals /= null then
         theAI_Mesh.Normals := To_AI_Vertices_Map (C_Mesh.Normals, C_Mesh.Num_Vertices);
      end if;

      if C_Mesh.Tangents /= null then
         theAI_Mesh.Tangents := To_AI_Vertices_Map (C_Mesh.Tangents, C_Mesh.Num_Vertices);
      end if;

      if C_Mesh.Bit_Tangents /= null then
         theAI_Mesh.Bit_Tangents := To_AI_Vertices_Map (C_Mesh.Bit_Tangents, C_Mesh.Num_Vertices);
      end if;

      theAI_Mesh.Colours := To_AI_Colours_Map (C_Mesh.Colours, C_Mesh.Num_Vertices);

      theAI_Mesh.Texture_Coords :=
        To_AI_Texture_Coords_Map (C_Mesh.Texture_Coords, C_Mesh.Num_Vertices);

      theAI_Mesh.Material_Index := UInt (C_Mesh.Material_Index);
      if Num_Faces > 0 then
         theAI_Mesh.Faces := To_AI_Faces_Map (C_Mesh.Faces, C_Mesh.Num_Faces);
      end if;

      if Num_Bones > 0 then
         theAI_Mesh.Bones := To_AI_Bones_Map (C_Mesh.Bones, C_Mesh.Num_Bones);
      end if;
      return theAI_Mesh;

   exception
      when others =>
         Put_Line ("An exception occurred in Assimp_Mesh.To_AI_Mesh.");
         raise;
   end To_AI_Mesh;

   --  ------------------------------------------------------------------------

   function To_AI_Mesh_Map (Num_Meshes   : Interfaces.C.unsigned := 0;
                            C_Mesh_Ptr_Array : Mesh_Ptr_Array_Pointer)
                            return Assimp_Mesh.AI_Mesh_Map is
      use Interfaces.C;
      Meshes   : Assimp_Mesh.AI_Mesh_Map;
      C_Meshes : constant API_Mesh_Ptr_Array := Mesh_Array_Pointers.Value
        (C_Mesh_Ptr_Array, ptrdiff_t (Num_Meshes));
      C_Mesh   : API_Mesh;
      aMesh    : Assimp_Mesh.AI_Mesh;
   begin
       for index in 1 .. Num_Meshes loop
         C_Mesh := C_Meshes (index - 1).all;
         aMesh := To_AI_Mesh (C_Mesh);
         Meshes.Insert (UInt (index), aMesh);
      end loop;
      return Meshes;

   exception
      when others =>
         Put_Line ("An exception occurred in Assimp_Mesh.To_AI_Mesh_Map.");
         raise;

   end To_AI_Mesh_Map;

   --  ------------------------------------------------------------------------

   function To_AI_Texture_Coords_Map (C_Array      : API_Texture_Coords_3D_Ptr_Array;
                                      Num_Vertices : Interfaces.C.unsigned)
                                      return Assimp_Mesh.Texture_Coords_Map is
      use Texture_Coords_Array_Pointers;
      API_Coords_Ptr   : Texture_Coords_Array_Pointer;
      API_Coords_Array : API_Texture_Coords_Array (1 .. Num_Vertices);
      API_Coords       : API_Vectors_Matrices.API_Texture_Coords_3D;
      Texture_Coords   : Singles.Vector3;
      Coords_Map       : Assimp_Mesh.Vertices_Map;
      theMap           : Assimp_Mesh.Texture_Coords_Map;
   begin
      for index in C_Array'First .. C_Array'Last loop
         if C_Array (index) /= null then
            API_Coords_Ptr := C_Array (index);
            API_Coords_Array :=
              Texture_Coords_Array_Pointers.Value
                (API_Coords_Ptr, Interfaces.C.ptrdiff_t (Num_Vertices));
            for T_index in API_Coords_Array'First .. API_Coords_Array'Last loop
               API_Coords := API_Coords_Array (T_index);
               Texture_Coords (GL.X) := Single (API_Coords.U);
               Texture_Coords (GL.Y) := Single (API_Coords.V);
               Texture_Coords (GL.Z) := Single (API_Coords.W);
               Coords_Map.Insert (UInt (T_index), Texture_Coords);
            end loop;
            theMap.Insert (UInt (index), Coords_Map);
         end if;
      end loop;
      return theMap;
   end To_AI_Texture_Coords_Map;

   --  ------------------------------------------------------------------------

   function To_AI_Vertices_Map (C_Array_Ptr  : Vector_3D_Array_Pointers.Pointer;
                                Num_Vertices : Interfaces.C.unsigned)
                                return Assimp_Mesh.Vertices_Map is
      V_Array      : API_Vectors_Matrices.API_Vector_3D_Array (1 .. Num_Vertices);
      anAPI_Vector : API_Vector_3D;
      anAI_Vector  : Singles.Vector3;
      theMap       : Assimp_Mesh.Vertices_Map;
   begin
      V_Array := API_Vectors_Matrices.Vector_3D_Array_Pointers.Value
        (C_Array_Ptr, Interfaces.C.ptrdiff_t (Num_Vertices));

      for index in 1 .. Num_Vertices loop
         anAPI_Vector := V_Array (index);
         anAI_Vector (GL.X) := Single (anAPI_Vector.X);
         anAI_Vector (GL.Y) := Single (anAPI_Vector.Y);
         anAI_Vector (GL.Z) := Single (anAPI_Vector.Z);
         theMap.Insert (UInt (index), anAI_Vector);
      end loop;
      return theMap;
   end To_AI_Vertices_Map;

   --  ------------------------------------------------------------------------

   function To_AI_Vertex_Weight_Map (Weights_Ptr : Vertex_Weight_Array_Pointer;
                                     Num_Weights : Interfaces.C.unsigned)
                                     return Assimp_Mesh.Vertex_Weight_Map is
      Weight_Array : API_Vertex_Weight_Array (1 .. Num_Weights);
      anAI_Weight  : Assimp_Mesh.AI_Vertex_Weight;
      theMap        :Assimp_Mesh. Vertex_Weight_Map;
   begin
      Weight_Array := Vertex_Weight_Array_Pointers.Value
        (Weights_Ptr, Interfaces.C.ptrdiff_t (Num_Weights));

      for index in 1 .. Num_Weights loop
         anAI_Weight.Vertex_ID := UInt (Weight_Array (index).Vertex_ID);
         anAI_Weight.Weight := Single (Weight_Array (index).Weight);
         theMap.Insert (UInt (index), anAI_Weight);
      end loop;
      return theMap;
   end To_AI_Vertex_Weight_Map;

   --  ------------------------------------------------------------------------

end Mesh_Conversion;
