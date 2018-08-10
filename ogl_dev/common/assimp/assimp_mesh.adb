
with Interfaces.C.Strings;

with Ogldev_Util;

with Assimp.API;
with Assimp_Util;
with Importer;
with Scene;

package body Assimp_Mesh is

   function Init_From_Scene (theMesh   : in out Mesh; theScene : Scene.AI_Scene;
                             File_Name : String) return Boolean;
   function Init_Materials (theMesh   : in out Mesh; theScene : Scene.AI_Scene;
                            File_Name : String) return Boolean;
   procedure Init_Mesh (theMesh : in out Mesh; Mesh_Index : UInt; anAI_Mesh : AI_Mesh);
   function To_API_Mesh (anAI_Mesh : AI_Mesh) return API_Mesh;

   --  ------------------------------------------------------------------------

   function Init_From_Scene (theMesh   : in out Mesh; theScene  : Scene.AI_Scene;
                             File_Name : String) return Boolean is
      use AI_Mesh_Package;
      anAI_Mesh : AI_Mesh;
      Index     : UInt := 0;
   begin
      for iterator  in theScene.Meshes.Iterate loop
         anAI_Mesh := Element (iterator);
         Index := Index + 1;
         Init_Mesh (theMesh, Index, anAI_Mesh);
      end loop;

      return Init_Materials (theMesh, theScene, File_Name);
   end Init_From_Scene;

   --  ------------------------------------------------------------------------

   function Init_Materials (theMesh   : in out Mesh; theScene : Scene.AI_Scene;
                            File_Name : String) return Boolean is
   begin
      return False;
   end Init_Materials;

   --  ------------------------------------------------------------------------

   procedure Init_Mesh (theMesh : in out Mesh; Mesh_Index : UInt; anAI_Mesh : AI_Mesh) is
      pai_Mesh : API_Mesh := To_API_Mesh (anAI_Mesh);
   begin

      null;
      --          Assimp.API.Init_Mesh (theMesh : in out API_Mesh; Mesh_Index : UInt)
      --            (Material, Tex_Type, unsigned (Tex_Index), C_Path'Access);
   end Init_Mesh;

   --  ------------------------------------------------------------------------

   procedure Load_Mesh (File_Name : String; theMesh : in out Mesh) is
      theScene : Scene.AI_Scene;
      Ok       : Boolean := False;
   begin
      theScene := Importer.Read_File (File_Name, UInt (Ogldev_Util.Assimp_Load_Flags));
      Ok := Init_From_Scene (theMesh, theScene, File_Name);
   end Load_Mesh;

   --  ------------------------------------------------------------------------

   procedure Render_Mesh (theMesh : Mesh) is
   begin
      null;
   end Render_Mesh;

   --  ------------------------------------------------------------------------

   function To_API_Mesh (anAI_Mesh : AI_Mesh) return API_Mesh is
      use Interfaces;
      C_Mesh   : API_Mesh;
      V_Length : constant C.unsigned := C.unsigned (Length (anAI_Mesh.Vertices));
      V_Array  : API_Vector_3D_Array (1 .. V_Length);
   begin
      C_Mesh.Num_Vertices := C.unsigned (V_Length);
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
      return C_Mesh;
   end To_API_Mesh;

   --  ------------------------------------------------------------------------

end Assimp_Mesh;
