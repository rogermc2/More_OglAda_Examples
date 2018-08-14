
with Ada.Text_IO; use Ada.Text_IO;

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

   procedure Init_Mesh (theMesh : in out Mesh; Mesh_Index : UInt; anAI_Mesh : AI_Mesh) is
--        pai_Mesh : API_Mesh := To_API_Mesh (anAI_Mesh);
   begin
   null;
--         Assimp.API.Init_Mesh (theMesh, Mesh_Index);
--                  (Material, Tex_Type, unsigned (Tex_Index), C_Path'Access);
   end Init_Mesh;

   --  ------------------------------------------------------------------------

   procedure Load_Mesh (File_Name : String; theMesh : in out Mesh) is
      theScene : Scene.AI_Scene;
   begin
        null;
      theScene := Importer.Read_File (File_Name, UInt (Ogldev_Util.Assimp_Load_Flags));
      Init_From_Scene (theMesh, theScene, File_Name);
   end Load_Mesh;

   --  ------------------------------------------------------------------------

   procedure Render_Mesh (theMesh : Mesh) is
   begin
      null;
   end Render_Mesh;

   --  ------------------------------------------------------------------------

   function To_AI_Mesh (C_Mesh : API_Mesh) return AI_Mesh is
      use Interfaces;
      theAI_Mesh   : AI_Mesh;
      Num_Vertices : constant C.unsigned := C_Mesh.Num_Vertices;
--        Num_Faces    : constant C.unsigned :=C_Mesh.Num_Faces;
--        Num_UV       : constant C.unsigned := C_Mesh.Num_UV_Components;
--        Num_Bones    : constant C.unsigned := C_Mesh.Num_Bones;
      V_Array      : API_Vectors_Matrices.API_Vector_3D_Array (1 .. Num_Vertices);
      anAPI_Vertex : API_Vector_3D;
      anAI_Vertex  : Singles.Vector3;
   begin
      theAI_Mesh.Material_Index := UInt (C_Mesh.Material_Index);
      theAI_Mesh.Name :=  Assimp_Util.To_Unbounded_String (C_Mesh.Name);
      for index in 1 .. AI_Max_Colour_Sets loop
         theAI_Mesh.Colours (index).R :=
           Single (C_Mesh.Colours (C.unsigned (index)).R);
         theAI_Mesh.Colours (index).G :=
           Single (C_Mesh.Colours (C.unsigned (index)).G);
         theAI_Mesh.Colours (index).B :=
           Single (C_Mesh.Colours (C.unsigned (index)).B);
         theAI_Mesh.Colours (index).A :=
           Single (C_Mesh.Colours (C.unsigned (index)).A);
      end loop;
        --   Vertices : Vector_3D_Array_Pointer
        V_Array := Vector_3D_Array_Pointers.Value (C_Mesh.Vertices);
      for index in 1 .. Num_Vertices loop
          anAPI_Vertex := V_Array (index);
          anAI_Vertex (GL.X) := Single (anAPI_Vertex.X);
          anAI_Vertex (GL.Y) := Single (anAPI_Vertex.Y);
          anAI_Vertex (GL.Z) := Single (anAPI_Vertex.Z);
          theAI_Mesh.Vertices.Insert (UInt (index), anAI_Vertex);
      end loop;

      for index in 1 .. AI_Max_Texture_Coords loop
         theAI_Mesh.Texture_Coords (GL.Types.Int (index)) (GL.X):=
           Single (C_Mesh.Texture_Coords (C.unsigned (index)).X);
         theAI_Mesh.Texture_Coords (GL.Types.Int (index)) (GL.Y):=
           Single (C_Mesh.Texture_Coords (C.unsigned (index)).Y);
         theAI_Mesh.Texture_Coords (GL.Types.Int (index)) (GL.Z):=
           Single (C_Mesh.Texture_Coords (C.unsigned (index)).Z);
      end loop;

      return theAI_Mesh;
   end To_AI_Mesh;

   --  ------------------------------------------------------------------------

   function To_AI_Mesh_Map (Num_Meshes : Interfaces.C.unsigned := 0;
                            C_Mesh_Array : API_Mesh_Array)
                            return AI_Mesh_Map is
      Meshs  : AI_Mesh_Map;
      aMesh  : AI_Mesh;
   begin
      for index in 1 .. Num_Meshes loop
         aMesh := To_AI_Mesh (C_Mesh_Array (index));
         Meshs.Insert (UInt (index), aMesh);
      end loop;
      return Meshs;
   end To_AI_Mesh_Map;

   --  ------------------------------------------------------------------------

   procedure Vertices_To_API (Vertices : Vertices_Map; V_Array  : in out API_Vector_3D_Array) is
      use Interfaces;
      use Vertices_Package;

      V_Curs   : Vertices_Package.Cursor := Vertices.First;
    begin
      while Has_Element (V_Curs) loop
         V_Array (C.unsigned (Key (V_Curs))).X := C.C_float (Element (V_Curs) (GL.X));
         V_Array (C.unsigned (Key (V_Curs))).Y := C.C_float (Element (V_Curs) (GL.Y));
         V_Array (C.unsigned (Key (V_Curs))).Z := C.C_float (Element (V_Curs) (GL.Z));
         Next (V_Curs);
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Assimp_Mesh.To_API_Mesh.");
         raise;
   end Vertices_To_API;

   --  ------------------------------------------------------------------------

   procedure To_API_Mesh (anAI_Mesh : AI_Mesh; C_Mesh : in out API_Mesh) is
      use Interfaces;
      use Faces_Package;
      V_Length  : constant C.unsigned := C.unsigned (Length (anAI_Mesh.Vertices));
      N_Length  : constant C.unsigned := C.unsigned (Length (anAI_Mesh.Normals));
      T_Length  : constant C.unsigned := C.unsigned (Length (anAI_Mesh.Tangents));
      BT_Length : constant C.unsigned := C.unsigned (Length (anAI_Mesh.Bit_Tangents));
      B_Length  : constant C.unsigned := C.unsigned (Length (anAI_Mesh.Bones));
      F_Length  : constant C.unsigned := C.unsigned (Length (anAI_Mesh.Faces));
      V_Array   : aliased  API_Vector_3D_Array (1 .. V_Length);
      N_Array   : aliased  API_Vector_3D_Array (1 .. N_Length);
      T_Array   : aliased  API_Vector_3D_Array (1 .. T_Length);
      BT_Array  : aliased  API_Vector_3D_Array (1 .. BT_Length);
      B_Array   : aliased  API_Vector_3D_Array (1 .. B_Length);
      F_Array   : aliased  API_Faces_Array (1 .. F_Length);
      F_Curs    : Faces_Package.Cursor := anAI_Mesh.Faces.First;
   begin
      C_Mesh.Num_Vertices := V_Length;
      C_Mesh.Num_Faces := F_Length;
      C_Mesh.Num_Bones := B_Length;
      C_Mesh.Num_UV_Components := C.unsigned (anAI_Mesh.Num_UV_Components);
      C_Mesh.Material_Index := C.unsigned (anAI_Mesh.Material_Index);
      C_Mesh.Name := Assimp_Util.To_Assimp_AI_String (anAI_Mesh.Name);
      for index in 1 .. AI_Max_Colour_Sets loop
         C_Mesh.Colours (C.unsigned (index)).R :=
           C.C_float (anAI_Mesh.Colours (index).R);
         C_Mesh.Colours (C.unsigned (index)).G :=
           C.C_float (anAI_Mesh.Colours (index).G);
         C_Mesh.Colours (C.unsigned (index)).B :=
           C.C_float (anAI_Mesh.Colours (index).B);
         C_Mesh.Colours (C.unsigned (index)).A :=
           C.C_float (anAI_Mesh.Colours (index).A);
      end loop;
      for index in 1 .. AI_Max_Texture_Coords loop
         C_Mesh.Texture_Coords (C.unsigned (index)).X :=
           C.C_float (anAI_Mesh.Texture_Coords (GL.Types.Int (index)) (GL.X));
         C_Mesh.Texture_Coords (C.unsigned (index)).Y :=
           C.C_float (anAI_Mesh.Texture_Coords (GL.Types.Int (index)) (GL.Y));
         C_Mesh.Texture_Coords (C.unsigned (index)).Z :=
           C.C_float (anAI_Mesh.Texture_Coords (GL.Types.Int (index)) (GL.Z));
      end loop;
      Vertices_To_API (anAI_Mesh.Vertices, V_Array);
      Vertices_To_API (anAI_Mesh.Normals, N_Array);
      Vertices_To_API (anAI_Mesh.Tangents, T_Array);
      Vertices_To_API (anAI_Mesh.Bit_Tangents, BT_Array);

      while Has_Element (F_Curs) loop
         declare
            aFace     : constant AI_Face := Element (F_Curs);
            Index_Map : constant Indices_Map := aFace.Indices;
            F_Indices : array (1 .. Index_Map.Length) of aliased Interfaces.C.unsigned;
         begin
            for index in Index_Map.Length loop
               F_Indices (index) :=
                 Interfaces.C.unsigned (Index_Map.Element (index));
            end loop;
            F_Array (C.unsigned (Key (F_Curs))).Indices :=
              F_Indices(1)'Unchecked_Access;
         end;
         Next (F_Curs);
      end loop;

      C_Mesh.Vertices := V_Array (V_Array'First)'Unchecked_Access;
      C_Mesh.Normals := N_Array (N_Array'First)'Unchecked_Access;
      C_Mesh.Tangents := T_Array (T_Array'First)'Unchecked_Access;
      C_Mesh.Bit_Tangents := BT_Array (BT_Array'First)'Unchecked_Access;
      C_Mesh.Bones := B_Array (B_Array'First)'Unchecked_Access;
      C_Mesh.Faces := F_Array (F_Array'First)'Unchecked_Access;

   exception
      when others =>
         Put_Line ("An exception occurred in Assimp_Mesh.To_API_Mesh.");
         raise;
   end To_API_Mesh;

   --  ------------------------------------------------------------------------

end Assimp_Mesh;
