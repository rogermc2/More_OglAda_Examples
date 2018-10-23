
with System;

with Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

with Ogldev_Math;
with Ogldev_Util;

with Assimp_Util;
with Importer;
with Scene;

package body Assimp_Mesh is

   procedure Init_From_Scene (theMesh   : in out Mesh; theScene : Scene.AI_Scene;
                              File_Name : String);
   procedure Init_Materials (theMesh   : in out Mesh; theScene : Scene.AI_Scene;
                             File_Name : String);
   procedure Init_Mesh (theMesh : in out Mesh; Mesh_Index : UInt; anAI_Mesh : AI_Mesh);
   function To_AI_Vertex_Weight_Map (Weights_Ptr : Vertex_Weight_Array_Pointer;
                                     Num_Weights : Interfaces.C.unsigned) return Vertex_Weight_Map;
   function To_AI_Colours_Map (C_Array_Ptr  : Colours_4D_Array_Pointer;
                               Num_Vertices : Interfaces.C.unsigned) return Colours_Map;
   function To_AI_Vertices_Map (C_Array_Ptr  : Vector_3D_Array_Pointers.Pointer;
                                Num_Vertices : Interfaces.C.unsigned) return Vertices_Map;

   ------------------------------------------------------------------------

   function Has_Texture_Coords (aMesh : Mesh; Index : UInt) return Boolean is
   begin
      return Assimp_Texture.Texture_Map_Size (aMesh.Textures) > 0;
   end Has_Texture_Coords;

   ------------------------------------------------------------------------

   procedure Init_From_Scene (theMesh   : in out Mesh; theScene  : Scene.AI_Scene;
                              File_Name : String) is
      use AI_Mesh_Package;
      anAI_Mesh : AI_Mesh;
      Index     : UInt := 0;
   begin
      for iterator  in theScene.Meshes.Iterate loop
         anAI_Mesh := Element (iterator);
         Index := Index + 1;
         Init_Mesh (theMesh, Index, anAI_Mesh);
      end loop;

      Init_Materials (theMesh, theScene, File_Name);
   end Init_From_Scene;

   ------------------------------------------------------------------------

   procedure Init_Materials (theMesh   : in out Mesh; theScene : Scene.AI_Scene;
                             File_Name : String) is
   begin
      null;
   end Init_Materials;

   ------------------------------------------------------------------------

   procedure Init_Mesh (theMesh   : in out Mesh; Mesh_Index : UInt;
                        anAI_Mesh : AI_Mesh) is
      --          Num_Vertices : constant Int := Int (theMesh.Vertices.Length);
      --          Vertices     : Vertex_Array (1 .. Num_Vertices);
      --          Indices      : GL.Types.UInt_Array (1 .. 3 * Num_Vertices);
      --          Position     : GL.Types.Singles.Vector3;
      --          Normal       : GL.Types.Singles.Vector3;
      --          Tex_Coord    : GL.Types.Singles.Vector3;
      --          Face         : Assimp_Mesh.AI_Face;
      --          Index_Index  : Int := 0;
   begin
      null;
      --          for Index in 1 .. Num_Vertices loop
      --              Position := theMesh.Vertices.Element (UInt (Index));
      --              Normal := theMesh.Normals.Element (UInt (Index));
      --              Tex_Coord := theMesh.Texture_Coords (Index);
      --              Vertices (Index) := (Position, (Tex_Coord (GL.X), Tex_Coord (GL.Y)), Normal);
      --          end loop;

      --          for Index in 1 .. theMesh.Faces.Length loop
      --              Face := theMesh.Faces.Element (UInt (Index));
      --              Index_Index := Index_Index + 1;
      --              Indices (Int (Index)) := Face.Indices (1);
      --              Index_Index := Index_Index + 1;
      --              Indices (Int (Index)) := Face.Indices (2);
      --              Index_Index := Index_Index + 1;
      --              Indices (Int (Index)) := Face.Indices (3);
      --          end loop;
      --        Init_Buffers (anEntry, Vertices, Indices);

   exception
      when others =>
         Put_Line ("An exception occurred in Assimp_Mesh.Init_Mesh.");
         raise;
   end Init_Mesh;

   --  ------------------------------------------------------------------------

   procedure Load_Mesh (File_Name : String; theMesh : in out Mesh) is
      theScene : Scene.AI_Scene;
   begin
      theScene := Importer.Read_File (File_Name, UInt (Ogldev_Util.Assimp_Load_Flags));
      Init_From_Scene (theMesh, theScene, File_Name);

   exception
      when others =>
         Put_Line ("An exception occurred in Assimp_Mesh.Load_Mesh.");
         raise;
   end Load_Mesh;

   --  ------------------------------------------------------------------------

   procedure Render_Mesh (theMesh : Mesh) is
   begin
      null;

   exception
      when others =>
         Put_Line ("An exception occurred in Assimp_Mesh.Render_Mesh.");
         raise;
   end Render_Mesh;

   --  ------------------------------------------------------------------------

   function To_AI_Bones_Map (B_Array_Access : access Bones_Array_Pointer;
                             Num_Bones      : Interfaces.C.unsigned) return Bones_Map is

      Bones_Array_Ptr : constant Bones_Array_Pointer := B_Array_Access.all;
      B_Array         : constant API_Bones_Array := Bones_Array_Pointers.Value
        (Bones_Array_Ptr, Interfaces.C.ptrdiff_t (Num_Bones));
      anAPI_Bone      : API_Bone;
      anAI_Bone       : AI_Bone;
      theMap          : Bones_Map;
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

   function To_AI_Colours_Map (C_Array_Ptr  : Colours_4D_Array_Pointer;
                               Num_Vertices : Interfaces.C.unsigned) return Colours_Map is
      use Colours_Package;
      V_Array        : API_Vectors_Matrices.API_Colours_4D_Array (1 .. Num_Vertices);
      C_Array        : API_Vectors_Matrices.API_Colour_Set_4D;
      Colours        : Colour_Array_4D;
      API_Colours    : API_Vectors_Matrices.API_Colour_4D;
      theMap          : Colours_Map;
   begin
      V_Array := API_Vectors_Matrices.Colours_4D_Array_Pointers.Value
        (C_Array_Ptr, Interfaces.C.ptrdiff_t (Num_Vertices));

      for V_index in V_Array'First .. V_Array'Last loop
         C_Array := V_Array (V_index);
         for index in C_Array'First .. C_Array'Last loop
            API_Colours := C_Array (index);
            Colours (UInt (index)).R := Single (API_Colours.R);
            Colours (UInt (index)).G := Single (API_Colours.G);
            Colours (UInt (index)).B := Single (API_Colours.B);
            Colours (UInt (index)).A := Single (API_Colours.A);
         end loop;
         theMap.Insert (UInt (V_index), Colours);
      end loop;
      return theMap;
   end To_AI_Colours_Map;

   --  ------------------------------------------------------------------------

   function To_AI_Face_Indices_Map (Indices_Ptr : Unsigned_Array_Pointer;
                                    Num_Indices : Interfaces.C.unsigned) return Indices_Map is
      Index_Array  : API_Unsigned_Array (1 .. Num_Indices);
      theMap       : Indices_Map;
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
                             Num_Faces   : Interfaces.C.unsigned) return Faces_Map is

      Face_Array   : API_Faces_Array (1 .. Num_Faces);
      anAPI_Face   : API_Face;
      anAI_Face    : AI_Face;
      theMap       : Faces_Map;
   begin
      Face_Array := Faces_Array_Pointers.Value
        (F_Array_Ptr, Interfaces.C.ptrdiff_t (Num_Faces));

      for index in 1 .. Num_Faces loop
         anAPI_Face := Face_Array (index);
         anAI_Face.Indices := To_AI_Face_Indices_Map (anAPI_Face.Indices, anAPI_Face.Num_Indices);
         theMap.Insert (UInt (index), anAI_Face);
      end loop;
      return theMap;
   end To_AI_Faces_Map;

   --  ------------------------------------------------------------------------

   function To_AI_Mesh (C_Mesh : API_Mesh) return AI_Mesh is
      use Interfaces.C;
      use Vector_3D_Array_Pointers;
      use API_Vectors_Matrices;
      use Colours_4D_Array_Pointers;
      theAI_Mesh   : AI_Mesh;
      Num_Vertices : constant unsigned := C_Mesh.Num_Vertices;
      Num_Faces    : constant unsigned :=C_Mesh.Num_Faces;
      Num_Bones    : constant unsigned := C_Mesh.Num_Bones;
      Colours      : API_Colour_4D;
      Textures     : API_Vector_3D;
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

      if C_Mesh.Colours /= null then
         theAI_Mesh.Colours := To_AI_Colours_Map (C_Mesh.Colours, C_Mesh.Num_Vertices);
      end if;

      Put_Line ("Assimp_Mesh.To_AI_Mesh C_Mesh.Texture_Coords size: " &
                  unsigned'Image (C_Mesh.Texture_Coords'Length));
      for index in C_Mesh.Texture_Coords'First .. C_Mesh.Texture_Coords'Last loop
         if C_Mesh.Texture_Coords (unsigned (index)) /= null then
            Textures := C_Mesh.Texture_Coords (unsigned (index)).all;
            theAI_Mesh.Texture_Coords (GL.Types.Int (index)) (GL.X) := Single (Textures.X);
            theAI_Mesh.Texture_Coords (GL.Types.Int (index)) (GL.Y) := Single (Textures.Y);
            theAI_Mesh.Texture_Coords (GL.Types.Int (index)) (GL.Z) := Single (Textures.Z);
         end if;
         Utilities.Print_Vector ("Assimp_Mesh.To_AI_Mesh theAI_Mesh.Texture_Coords",
                                 theAI_Mesh.Texture_Coords (GL.Types.Int (index)));
      end loop;

      for index in GL.Types.Int range 1 .. API_Max_Texture_Coords loop
         theAI_Mesh.Num_UV_Components (index) := UInt (C_Mesh.Num_UV_Components (unsigned (index)));
      end loop;
      Utilities.Print_GL_UInt_Array ("Assimp_Mesh.To_AI_Mesh theAI_Mesh.Num_UV_Components",
                                     theAI_Mesh.Num_UV_Components);

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
                            C_Mesh_Array : API_Mesh_Array)
                             return AI_Mesh_Map is
      use Interfaces.C;
      Meshs  : AI_Mesh_Map;
      aMesh  : AI_Mesh;
   begin
      --          Put_Line ("Assimp_Mesh.To_AI_Mesh_Map Num_Meshes: " & Interfaces.C.unsigned'image (Num_Meshes));
      for index in 1 .. Num_Meshes loop
         Put_Line ("Assimp_Mesh.To_AI_Mesh_Map, index: " &  unsigned'Image (index));
         Put_Line ("Assimp_Mesh.To_AI_Mesh_Map, Primitive_Types, Num Vertices, Faces, Bones, Anim_Meshes, Material_Index");
         Put_Line (unsigned'Image (C_Mesh_Array (index).Primitive_Types) &
                     unsigned'Image (C_Mesh_Array (index).Num_Vertices) &
                     unsigned'Image (C_Mesh_Array (index).Num_Faces) &
                     unsigned'Image (C_Mesh_Array (index).Num_Bones) &
                     unsigned'Image (C_Mesh_Array (index).Num_Anim_Meshes) &
                     unsigned'Image (C_Mesh_Array (index).Material_Index));
         New_Line;
         aMesh := To_AI_Mesh (C_Mesh_Array (index));
         Meshs.Insert (UInt (index), aMesh);
      end loop;

      return Meshs;

   exception
      when others =>
         Put_Line ("An exception occurred in Assimp_Mesh.To_AI_Mesh_Map.");
         raise;

   end To_AI_Mesh_Map;

   --  ------------------------------------------------------------------------

   function To_AI_Vertices_Map (C_Array_Ptr  : Vector_3D_Array_Pointers.Pointer;
                                Num_Vertices : Interfaces.C.unsigned) return Vertices_Map is

      V_Array      : API_Vectors_Matrices.API_Vector_3D_Array (1 .. Num_Vertices);
      anAPI_Vector : API_Vector_3D;
      anAI_Vector  : Singles.Vector3;
      theMap       : Vertices_Map;
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

   --      procedure To_API_Mesh (anAI_Mesh : AI_Mesh; C_Mesh : in out API_Mesh) is
   --          use Interfaces;
   --          use Faces_Package;
   --          V_Length  : constant C.unsigned := C.unsigned (Length (anAI_Mesh.Vertices));
   --          N_Length  : constant C.unsigned := C.unsigned (Length (anAI_Mesh.Normals));
   --          T_Length  : constant C.unsigned := C.unsigned (Length (anAI_Mesh.Tangents));
   --          BT_Length : constant C.unsigned := C.unsigned (Length (anAI_Mesh.Bit_Tangents));
   --          B_Length  : constant C.unsigned := C.unsigned (Length (anAI_Mesh.Bones));
   --          F_Length  : constant C.unsigned := C.unsigned (Length (anAI_Mesh.Faces));
   --          --          Colour_Ptrs   : API_Vectors_Matrices.API_Colour_4D_Ptr_Array;
   --          V_Array   : aliased  API_Vector_3D_Array (1 .. V_Length);
   --          N_Array   : aliased  API_Vector_3D_Array (1 .. N_Length);
   --          T_Array   : aliased  API_Vector_3D_Array (1 .. T_Length);
   --          BT_Array  : aliased  API_Vector_3D_Array (1 .. BT_Length);
   --          --        B_Array   : aliased  API_Vector_3D_Array (1 .. B_Length);
   --          F_Array   : aliased  API_Faces_Array (1 .. F_Length);
   --          F_Curs    : Faces_Package.Cursor := anAI_Mesh.Faces.First;
   --      begin
   --          C_Mesh.Num_Vertices := V_Length;
   --          C_Mesh.Num_Faces := F_Length;
   --          C_Mesh.Num_Bones := B_Length;
   --          for index in GL.Types.Int range 1 .. API_Max_Texture_Coords loop
   --              C_Mesh.Num_UV_Components (C.unsigned (index)) := C.unsigned (anAI_Mesh.Num_UV_Components (index));
   --          end loop;
   --          C_Mesh.Material_Index := C.unsigned (anAI_Mesh.Material_Index);
   --          C_Mesh.Name := Assimp_Util.To_Assimp_API_String (anAI_Mesh.Name);
   --          for index in UInt range 1 .. API_Max_Colour_Sets loop
   --              null;
   --              --              Colour_Ptrs (Integer (index)).R := C.C_float (anAI_Mesh.Colours (index).R);
   --              --              Colour_Ptrs (Integer (index)).G := C.C_float (anAI_Mesh.Colours (index).G);
   --              --              Colour_Ptrs (Integer (index)).B := C.C_float (anAI_Mesh.Colours (index).B);
   --              --              Colour_Ptrs (Integer (index)).A := C.C_float (anAI_Mesh.Colours (index).A);
   --          end loop;
   --          for index in 1 .. API_Max_Texture_Coords loop
   --              null;
   --              --              C_Mesh.Texture_Coords (C.unsigned (index)).X :=
   --              --                C.C_float (anAI_Mesh.Texture_Coords (GL.Types.Int (index)) (GL.X));
   --              --              C_Mesh.Texture_Coords (C.unsigned (index)).Y :=
   --              --                C.C_float (anAI_Mesh.Texture_Coords (GL.Types.Int (index)) (GL.Y));
   --              --              C_Mesh.Texture_Coords (C.unsigned (index)).Z :=
   --              --                C.C_float (anAI_Mesh.Texture_Coords (GL.Types.Int (index)) (GL.Z));
   --          end loop;
   --          Vertices_To_API (anAI_Mesh.Vertices, V_Array);
   --          Vertices_To_API (anAI_Mesh.Normals, N_Array);
   --          Vertices_To_API (anAI_Mesh.Tangents, T_Array);
   --          Vertices_To_API (anAI_Mesh.Bit_Tangents, BT_Array);
   --
   --          while Has_Element (F_Curs) loop
   --              declare
   --                  use Ada.Containers;
   --                  use Indices_Package;
   --                  aFace     : constant AI_Face := Element (F_Curs);
   --                  Index_Map : constant Indices_Map := aFace.Indices;
   --                  F_Indices : array (1 .. Index_Map.Length) of aliased Interfaces.C.unsigned;
   --                  Index     : Count_Type;
   --              begin
   --                  for I_Cursor in Index_Map.Iterate loop
   --                      Index := Count_Type (Key (I_Cursor));
   --                      F_Indices (Index) := Interfaces.C.unsigned (Element (I_Cursor));
   --                  end loop;
   --                  F_Array (C.unsigned (Key (F_Curs))).Indices := F_Indices (1)'Unchecked_Access;
   --              end;
   --              Next (F_Curs);
   --          end loop;
   --
   --          --        C_Mesh.Colours := Colours (0)'access;
   --          C_Mesh.Vertices := V_Array (V_Array'First)'Unchecked_Access;
   --          C_Mesh.Normals := N_Array (N_Array'First)'Unchecked_Access;
   --          C_Mesh.Tangents := T_Array (T_Array'First)'Unchecked_Access;
   --          C_Mesh.Bit_Tangents := BT_Array (BT_Array'First)'Unchecked_Access;
   --          --        C_Mesh.Bones := B_Array (B_Array'First)'Unchecked_Access;
   --          C_Mesh.Faces := F_Array (F_Array'First)'Unchecked_Access;
   --
   --      exception
   --          when others =>
   --              Put_Line ("An exception occurred in Assimp_Mesh.To_API_Mesh.");
   --              raise;
   --      end To_API_Mesh;

   --  ------------------------------------------------------------------------

   function To_AI_Vertex_Weight_Map (Weights_Ptr : Vertex_Weight_Array_Pointer;
                                     Num_Weights : Interfaces.C.unsigned) return Vertex_Weight_Map is
      Weight_Array : API_Vertex_Weight_Array (1 .. Num_Weights);
      anAI_Weight  : AI_Vertex_Weight;
      theMap       : Vertex_Weight_Map;
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

   --      procedure Vertices_To_API (Vertices : Vertices_Map; V_Array  : in out API_Vector_3D_Array) is
   --          use Interfaces;
   --          use Vertices_Package;
   --
   --          V_Curs   : Vertices_Package.Cursor := Vertices.First;
   --      begin
   --          while Has_Element (V_Curs) loop
   --              V_Array (C.unsigned (Key (V_Curs))).X := C.C_float (Element (V_Curs) (GL.X));
   --              V_Array (C.unsigned (Key (V_Curs))).Y := C.C_float (Element (V_Curs) (GL.Y));
   --              V_Array (C.unsigned (Key (V_Curs))).Z := C.C_float (Element (V_Curs) (GL.Z));
   --              Next (V_Curs);
   --          end loop;
   --
   --      exception
   --          when others =>
   --              Put_Line ("An exception occurred in Assimp_Mesh.Vertices_To_API.");
   --              raise;
   --      end Vertices_To_API;

   --  ------------------------------------------------------------------------

end Assimp_Mesh;
