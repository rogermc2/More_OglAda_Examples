
with Ada.Text_IO; use Ada.Text_IO;

with Ogldev_Util;

with Importer;
with Scene;

package body Assimp_Mesh is

   procedure Init_From_Scene (theMesh   : in out Mesh; theScene : Scene.AI_Scene;
                              File_Name : String);
   procedure Init_Materials (theMesh   : in out Mesh; theScene : Scene.AI_Scene;
                             File_Name : String);
   procedure Init_Mesh (theMesh : in out Mesh; Mesh_Index : UInt; anAI_Mesh : AI_Mesh);

   ------------------------------------------------------------------------

   function Has_Texture_Coords (aMesh : Mesh) return Boolean is
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
      theScene := Importer.Read_File (File_Name, UInt (Ogldev_Util.Assimp_Basic_Load_Flags));
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

end Assimp_Mesh;
