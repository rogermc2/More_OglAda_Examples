
with Interfaces.C;

with GNAT.Directory_Operations;

with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Low_Level.Enums;

with Maths;
with Utilities;

with Assimp_Mesh;
with Assimp_Types;

with Importer;
with Material;

with Ogldev_Engine_Common;
with Ogldev_Util;

with Scene;

package body Mesh_22 is
   type Vertex is record
      Pos    : GL.Types.Singles.Vector3;
      Tex    : GL.Types.Singles.Vector2;
      Normal : GL.Types.Singles.Vector3;
   end record;
   type Vertex_Array is array (Int range <>) of Vertex;

   procedure Init_Materials (Initial_Mesh : in out Mesh_22;
                             File_Name    : String;
                             theScene     : Scene.AI_Scene);
   procedure Init_Mesh (Index : UInt; aMesh : in out Assimp_Mesh.AI_Mesh;
                        aMesh_22 : in out Mesh_22);

   --  -------------------------------------------------------------------------

   procedure Init_Buffers (theEntry : in out Mesh_Entry;
                           Vertices : Vertex_Array;
                           Indices  : GL.Types.UInt_Array) is
      use GL;
      use GL.Objects.Buffers;
      Vertices_Length : constant Int := Vertices'Length;
      Indices_Length  : constant Int := Indices'Length;
      Vertices_Array  : Maths.Vector8_Array (1 .. Vertices_Length);
   begin
      theEntry.Num_Indices := UInt (Indices_Length);
      theEntry.VBO.Initialize_Id;
      Array_Buffer.Bind (theEntry.VBO);
      Array_Buffer.Bind (theEntry.VBO);

      for index in 1 ..  Vertices_Length loop
         Vertices_Array (index) :=
           (Vertices (index).Pos (X), Vertices (index).Pos (Y), Vertices (index).Pos (Z),
            Vertices (index).Tex (X), Vertices (index).Tex (Y),
            Vertices (index).Normal (X), Vertices (index).Normal (Y), Vertices (index).Normal (Z));
      end loop;
      Utilities.Load_Vector8_Buffer (Array_Buffer, Vertices_Array, Static_Draw);
      Utilities.Load_Element_Buffer (Array_Buffer, Indices, Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in Mesh_22.Init_Buffers.");
         raise;

   end Init_Buffers;

   --  -------------------------------------------------------------------------

   procedure Init_From_Scene (Initial_Mesh : in out Mesh_22;
                              File_Name    : String;
                              theScene     : Scene.AI_Scene) is
      use Mesh_Entry_Package;
      Curs         : Cursor := Initial_Mesh.Entries.First;
      Index        : UInt := 0;
      aMesh        : Assimp_Mesh.AI_Mesh;
      aMesh_22     : Mesh_22;
      anEntry      : Mesh_Entry;
   begin
      while Has_Element (Curs) loop
         Index := Index + 1;
         aMesh := theScene.Meshes (Index);
         Put_Line ("Mesh_22.Init_From_Scene, aMesh.Material_Index: " &
                     UInt'Image (aMesh.Material_Index));
         Init_Mesh (Index, aMesh, aMesh_22);
         Next (Curs);
      end loop;
      Init_Materials (Initial_Mesh, File_Name, theScene);

   exception
      when others =>
         Put_Line ("An exception occurred in Mesh_22.Init_From_Scene.");
         raise;
   end Init_From_Scene;

   -------------------------------------------------------------------------

   procedure Init_Materials (Initial_Mesh : in out Mesh_22;
                             File_Name    : String;
                             theScene     : Scene.AI_Scene) is
      use Material.AI_Material_Package;
      use Assimp_Types;
      use Material;
      --        Current_Dir : constant GNAT.Directory_Operations.Dir_Name_Str
      --          := GNAT.Directory_Operations.Get_Current_Dir;
      Dir           : constant GNAT.Directory_Operations.Dir_Name_Str
                       := GNAT.Directory_Operations.Dir_Name (File_Name);
      Path          : Ada.Strings.Unbounded.Unbounded_String;
      --        Full_Path   : constant String := Current_Dir & Dir;
      Result        : Assimp_Types.API_Return;
      Materials_Map : constant AI_Material_Map := theScene.Materials;

      procedure Load_Textures (Curs : AI_Material_Package.Cursor) is
         use Ogldev_Texture.Mesh_Texture_Package;
         aMaterial  : constant AI_Material := Element (Curs);
         aTexture   : Ogldev_Texture.Ogl_Texture;
      begin
         for index in 1 .. Initial_Mesh.Textures.Length loop
            Initial_Mesh.Textures.Delete_First;
         end loop;
         Put_Line ("Mesh_22.Init_Materials.Load_Textures Textures.Length: " &
                     Ada.Containers.Count_Type'Image (Initial_Mesh.Textures.Length));
         for index in 1 .. Initial_Mesh.Textures.Length loop
            if Get_Texture_Count (aMaterial, AI_Texture_Diffuse) > 0 then
               Get_Texture (aMaterial, AI_Texture_Diffuse, UInt (index), Path, Result);
               if Result = Assimp_Types.API_Return_Success then
                  Ogldev_Texture.Init_Texture
                    (aTexture, GL.Low_Level.Enums.Texture_2D, File_Name);
                  Initial_Mesh.Textures.Insert (UInt (index), aTexture);
                  Put_Line ("Mesh_22.Init_Materials.Load_Textures loaded texture from " & File_Name);
               else
                  Ogldev_Texture.Init_Texture
                    (aTexture, GL.Low_Level.Enums.Texture_2D, "../../Content/white.png");
                  Initial_Mesh.Textures.Insert (UInt (index), aTexture);
                  Put_Line ("Mesh_22.Init_Materials.Load_Textures loaded default texture from Content/white.png");
               end if;
                  Ogldev_Texture.Load (aTexture);
            end if;
         end loop;
      end Load_Textures;

   begin
      Put_Line ("Mesh_22.Init_Materials Dir: " & Dir);
      Materials_Map.Iterate (Load_Textures'Access);

   exception
      when others =>
         Put_Line ("An exception occurred in Mesh_22.Init_Materials.");
         raise;
   end Init_Materials;

   -------------------------------------------------------------------------

   procedure Init_Mesh (Index : UInt; aMesh : in out Assimp_Mesh.AI_Mesh; aMesh_22 : in out Mesh_22) is
   use Mesh_Entry_Package;
      Num_Vertices : constant Int := Int (aMesh.Vertices.Length);
      Vertices     : Vertex_Array (1 .. Num_Vertices);
      Indices      : GL.Types.UInt_Array (1 .. 3 * Num_Vertices);
      Mat_index    : GL.Types.UInt := aMesh.Material_Index;
      anEntry      : Mesh_Entry;
      Position     : GL.Types.Singles.Vector3;
      Normal       : GL.Types.Singles.Vector3;
      Tex_Coord    : GL.Types.Singles.Vector3;
      Face         : Assimp_Mesh.AI_Face;
      Index_Index  : Int := 0;
   begin
      anEntry := aMesh_22.Entries.Element (Index);
      anEntry.Material_Index := Material_Type'Val (aMesh.Material_Index);

      for Index in 1 .. Num_Vertices loop
         Position := aMesh.Vertices.Element (UInt (Index));
         Normal := aMesh.Normals.Element (UInt (Index));
         Tex_Coord := aMesh.Texture_Coords (Index);
         Vertices (Index) := (Position, (Tex_Coord (GL.X), Tex_Coord (GL.Y)), Normal);
      end loop;

      for Index in 1 .. aMesh.Faces.Length loop
         Face := aMesh.Faces.Element (UInt (Index));
         Index_Index := Index_Index + 1;
         Indices (Int (Index)) := Face.Indices (1);
         Index_Index := Index_Index + 1;
         Indices (Int (Index)) := Face.Indices (2);
         Index_Index := Index_Index + 1;
         Indices (Int (Index)) := Face.Indices (3);
      end loop;

      --  m_Entries[Index].Init(Vertices, Indices);
      Init_Buffers (anEntry, Vertices, Indices);
      aMesh_22.Entries.Replace (Index, anEntry);

   exception
      when others =>
         Put_Line ("An exception occurred in Mesh_22.Init_Mesh.");
         raise;
   end Init_Mesh;

   --  -------------------------------------------------------------------------

   procedure Load_Mesh (theMesh : in out Mesh_22; File_Name : String) is
      theScene : Scene.AI_Scene;
   begin
      theScene :=
        Importer.Read_File (File_Name, UInt (Ogldev_Util.Assimp_Load_Flags));
      Init_From_Scene (theMesh, File_Name, theScene);

   exception
      when others =>
         Put_Line ("An exception occurred in Mesh_22.Load_Mesh.");
         raise;
   end Load_Mesh;

   -------------------------------------------------------------------------

   procedure Render_Mesh (theMesh : Mesh_22) is
      use Ada.Containers;
      use Mesh_Entry_Package;
      procedure Draw (Entry_Cursor :  Mesh_Entry_Package.Cursor) is
         use Ogldev_Texture.Mesh_Texture_Package;
         --              Material_Kind  : constant Material_Type
         --                := Element (Entry_Cursor).Material_Index;
         Material_Index :  Material_Type;
         Tex_Curs       : Ogldev_Texture.Mesh_Texture_Package.Cursor;
         Num_Indices    : constant Int := Int (Element (Entry_Cursor).Num_Indices);
      begin
         GL.Objects.Buffers.Array_Buffer.Bind (Element (Entry_Cursor).VBO);
         GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);
         GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, 0, 12);
         GL.Attributes.Set_Vertex_Attrib_Pointer (2, 3, Single_Type, 0, 20);

         GL.Objects.Buffers.Element_Array_Buffer.Bind (Element (Entry_Cursor).IBO);
         Material_Index := Element (Entry_Cursor).Material_Index;
         Put_Line ("Mesh_22.Render_Mesh, Material_Index: " & Material_Type'Image (Material_Index));
         if Material_Index'Enum_Rep < theMesh.Textures.Length then
            if not theMesh.Textures.Is_Empty then
               Ogldev_Texture.Bind (Element (Tex_Curs),
                                    Ogldev_Engine_Common.Colour_Texture_Unit_Index);
            end if;
         else
            Put_Line ("Mesh_22.Render_Mesh, Invalid Material_Index.");
         end if;
         GL.Objects.Buffers.Draw_Elements
           (Triangles, Num_Indices, UInt_Type, 0);
      end Draw;
   begin
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Enable_Vertex_Attrib_Array (1);
      GL.Attributes.Enable_Vertex_Attrib_Array (2);

      Iterate (theMesh.Entries, Draw'Access);

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
      GL.Attributes.Disable_Vertex_Attrib_Array (1);
      GL.Attributes.Disable_Vertex_Attrib_Array (2);

   exception
      when others =>
         Put_Line ("An exception occurred in Mesh_22.Render_Mesh .");
         raise;
   end Render_Mesh;

   --  -------------------------------------------------------------------------

end Mesh_22;
