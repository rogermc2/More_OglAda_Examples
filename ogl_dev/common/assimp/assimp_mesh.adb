
with Ogldev_Util;

--  with Assimp.API;
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
      use Vertices_Package;
      theAI_Mesh : AI_Mesh;
      Num_Vertices : constant C.unsigned := C_Mesh.Num_Vertices;
      Num_Faces    : constant C.unsigned :=C_Mesh.Num_Faces;
      Num_UV       : constant C.unsigned := C_Mesh.Num_UV_Components;
      Num_Bones    : constant C.unsigned := C_Mesh.Num_Bones;
      V_Array      : API_Vector_3D_Array (1 .. Num_Vertices);
      V_Curs    : Cursor := theAI_Mesh.Vertices.First;
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
      for index in 1 .. Num_Vertices loop
         theAI_Mesh.Vertices.Update_Element (index, C_Mesh.Vertices (C.unsigned (index));
           Single (C_Mesh.Vertices (C.unsigned (index)) (1));
         theAI_Mesh.Texture_Coords (GL.Types.Int (index)) (GL.Y):=
           Single (C_Mesh.Texture_Coords (C.unsigned (index)).Y);
         theAI_Mesh.Texture_Coords (GL.Types.Int (index)) (GL.Z):=
           Single (C_Mesh.Texture_Coords (C.unsigned (index)).Z);
      end loop;for index in 1 .. AI_Max_Texture_Coords loop
         theAI_Mesh.Texture_Coords (GL.Types.Int (index)) (GL.X):=
           Single (C_Mesh.Texture_Coords (C.unsigned (index)).X);
         theAI_Mesh.Texture_Coords (GL.Types.Int (index)) (GL.Y):=
           Single (C_Mesh.Texture_Coords (C.unsigned (index)).Y);
         theAI_Mesh.Texture_Coords (GL.Types.Int (index)) (GL.Z):=
           Single (C_Mesh.Texture_Coords (C.unsigned (index)).Z);
      end loop;
--        while Has_Element (V_Curs) loop
--           V_Array (C.unsigned (Key (V_Curs))).X := C.C_float (Element (V_Curs) (GL.X));
--           V_Array (C.unsigned (Key (V_Curs))).Y := C.C_float (Element (V_Curs) (GL.Y));
--           V_Array (C.unsigned (Key (V_Curs))).Z := C.C_float (Element (V_Curs) (GL.Z));
--          Next  (V_Curs);
--        end loop;

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
         Materials.Insert (UInt (index), aMesh);
      end loop;
      return Meshs;
   end To_AI_Mesh_Map;

   --  ------------------------------------------------------------------------

   function To_API_Mesh (anAI_Mesh : AI_Mesh) return API_Mesh is
      use Interfaces;
      use Vertices_Package;
      C_Mesh   : API_Mesh;
      V_Length : constant C.unsigned := C.unsigned (Length (anAI_Mesh.Vertices));
      V_Array  : API_Vector_3D_Array (1 .. V_Length);
      V_Curs   : Cursor := anAI_Mesh.Vertices.First;
   begin
      C_Mesh.Num_Vertices := V_Length;
      C_Mesh.Num_Faces := C.unsigned (Length (anAI_Mesh.Faces));
      C_Mesh.Num_UV_Components := C.unsigned (anAI_Mesh.Num_UV_Components);
      C_Mesh.Num_Bones := C.unsigned (Length (anAI_Mesh.Bones));
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
      while Has_Element (V_Curs) loop
         V_Array (C.unsigned (Key (V_Curs))).X := C.C_float (Element (V_Curs) (GL.X));
         V_Array (C.unsigned (Key (V_Curs))).Y := C.C_float (Element (V_Curs) (GL.Y));
         V_Array (C.unsigned (Key (V_Curs))).Z := C.C_float (Element (V_Curs) (GL.Z));
        Next  (V_Curs);
      end loop;

      return C_Mesh;
   end To_API_Mesh;

   --  ------------------------------------------------------------------------

end Assimp_Mesh;
