
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Low_Level.Enums;
with GL.Objects.Textures;

with Maths;
with Utilities;

with Assimp_Types; use Assimp_Types;
with Importer;

with Ogldev_Util;

with Assimp_Mesh;
with Material;
with Material_System;
with Scene;

package body Meshes_24 is
   use GL.Types;
   type Vertex is record
      Pos     : GL.Types.Singles.Vector3;
      Tex     : GL.Types.Singles.Vector2;
      Normal  : GL.Types.Singles.Vector3;
   end record;
   type Vertex_Array is array (Int range <>) of Vertex;

   procedure Init_Materials (theMeshes : in out Mesh_24; File_Name : String;
                             theScene  : Scene.AI_Scene);
   procedure Init_Mesh (aMesh       : in out Mesh_Entry;
                        Source_Mesh : Assimp_Mesh.AI_Mesh);
   procedure Init_Mesh_Entry (theEntry : in out Mesh_Entry;
                              Vertices : Vertex_Array;
                              Indices  : GL.Types.UInt_Array);

   --  -------------------------------------------------------------------------

   procedure Init_From_Scene (Initialized_Meshes : in out Mesh_24;
                              File_Name          : String;
                              theScene           : Scene.AI_Scene) is
      use Assimp_Mesh.AI_Mesh_Package;
      Curs           : Cursor := theScene.Meshes.First;
      Mesh_Index     : UInt := 0;
      an_AI_Mesh     : Assimp_Mesh.AI_Mesh;
      aMesh          : Mesh_Entry;
   begin
      -- A scene contains an AI_Mesh_Map and an AI_Mesh_Map
      Put_Line ("Meshes_24.Init_From_Scene, initializing " &
                  File_Name);
      --  Initialize the meshes in the scene one by one
      while Has_Element (Curs) loop
         Mesh_Index := Mesh_Index + 1;
         an_AI_Mesh := theScene.Meshes (Mesh_Index);
         Init_Mesh (aMesh, an_AI_Mesh);
         Initialized_Meshes.Entries.Append (aMesh);
         Next (Curs);
      end loop;
      Init_Materials (Initialized_Meshes, File_Name, theScene);

   exception
      when others =>
         Put_Line ("An exception occurred in Meshes_24.Init_From_Scene.");
         raise;

   end Init_From_Scene;

   --  -------------------------------------------------------------------------

   procedure Init_Materials (theMeshes : in out Mesh_24; File_Name : String;
                             theScene  : Scene.AI_Scene) is
      use Ada.Strings.Unbounded;
      use Material.AI_Material_Package;
      use Assimp_Types;
      use Material;

      Path          : Ada.Strings.Unbounded.Unbounded_String;
      Dir           : constant String :=
                        Ada.Directories.Containing_Directory (File_Name) & "/";
      Materials_Map : constant AI_Material_Map := theScene.Materials;
      Result        : Assimp_Types.API_Return := Assimp_Types.API_Return_Success;

      procedure Load_Textures (Material_Curs : AI_Material_Package.Cursor) is
         use Ada.Strings.Unbounded;
         use Ogldev_Texture.Mesh_Texture_Package;
         aMaterial  : constant AI_Material := Element (Material_Curs);
         aTexture   : Ogldev_Texture.Ogl_Texture;
         Index      : constant GL.Types.UInt := Key (Material_Curs);
      begin
         if Result = Assimp_Types.API_Return_Success and then
           Get_Texture_Count (aMaterial, AI_Texture_Diffuse) > 0 then
            --  Get_Texture returns the path to the texture

            Result := Material_System.Get_Texture
              (aMaterial, AI_Texture_Diffuse, 0, Path);

            if Result = Assimp_Types.API_Return_Success then
               if Ogldev_Texture.Init_Texture
                 (aTexture, GL.Low_Level.Enums.Texture_2D,
                  Dir & To_String (Path)) then
                  Ogldev_Texture.Load (aTexture);
                  theMeshes.Textures.Insert (index, aTexture);
                  Put_Line ("Meshes_24.Init_Materials.Load_Textures loaded texture " &
                             GL.Types.UInt'Image (index) & " from " &
                             Dir & To_String (Path));
               else
                  Put_Line ("Meshes_24.Init_Material.Load_Textures texture " &
                             Dir & To_String (Path) & " not found.");
               end if;
            else
               Put_Line ("Meshes_23.Init_Material.Load_Textures Get_Texture result: " &
                          Assimp_Types.API_Return'Image (Result));
            end if;
         end if;
      end Load_Textures;

   begin
      New_Line;
      Materials_Map.Iterate (Load_Textures'Access);

   exception
      when others =>
         Put_Line ("An exception occurred in Meshes_24.Init_Material.API_Return_Success.");
         raise;
   end Init_Materials;

   --  -------------------------------------------------------------------------

   procedure Init_Mesh (aMesh       : in out Mesh_Entry;
                        Source_Mesh : Assimp_Mesh.AI_Mesh) is
      use Ada.Containers;
      Num_Vertices  : constant UInt := UInt (Source_Mesh.Vertices.Length);
      Vertices      : Vertex_Array (1 .. Int (Num_Vertices));
      Indices       : GL.Types.UInt_Array (1 .. Int (3 * Source_Mesh.Faces.Length));
      Position      : GL.Types.Singles.Vector3;
      Normal        : GL.Types.Singles.Vector3;
      Tex_Coord_Map : constant Assimp_Mesh.Texture_Coords_Map := Source_Mesh.Texture_Coords;
      Tex_Vertices  : Assimp_Mesh.Vertices_Map;
      Tex_Coord     : GL.Types.Singles.Vector3;
      Face          : Assimp_Mesh.AI_Face;
      Indices_Index : Int := 0;
   begin
      aMesh.Material_Index := Source_Mesh.Material_Index;
      Tex_Vertices := Tex_Coord_Map.Element (1);

      for V_Index in 1 .. Num_Vertices loop
         Position := Source_Mesh.Vertices.Element (V_Index);
         Normal := Source_Mesh.Normals.Element (V_Index);
            --  Tex_Coord_Map, contains Num_UV_Components sets of Texture_Coords.
            --  Sets of Texture_Coords are also known as UV channels.
            --  A mesh may contain 0 to AI_MAX_NUMBER_OF_TEXTURECOORDS per * vertex.

         Tex_Coord := Tex_Vertices (V_Index);
         Vertices (Int (V_Index)) :=
           (Position, (Tex_Coord (GL.X), Tex_Coord (GL.Y)), Normal);
      end loop;

      if Source_Mesh.Faces.Is_Empty then
         Put_Line ("Meshes_24.Init_Mesh, Source_Mesh.Faces is empty.");
      else
         for Face_Index in 1 .. Source_Mesh.Faces.Length loop
            Face := Source_Mesh.Faces.Element (UInt (Face_Index));
            Indices_Index := Indices_Index + 1;
            Indices (Indices_Index) := Face.Indices (1);
            Indices_Index := Indices_Index + 1;
            Indices (Indices_Index) := Face.Indices (2);
            Indices_Index := Indices_Index + 1;
            Indices (Indices_Index) := Face.Indices (3);
         end loop;
      end if;

      --  m_Entries[Index].Init(Vertices, Indices);
      Init_Mesh_Entry (aMesh, Vertices, Indices);

   exception
      when others =>
         Put_Line ("An exception occurred in Meshes_24.Init_Mesh.");
         raise;
   end Init_Mesh;

   --  -------------------------------------------------------------------------

   procedure Init_Mesh_Entry (theEntry : in out Mesh_Entry;
                              Vertices : Vertex_Array;
                              Indices  : GL.Types.UInt_Array) is
      use GL;
      use GL.Objects.Buffers;
      Vertices_Length : constant Int := Vertices'Length;
      Indices_Length  : constant Int := Indices'Length;
      Vertices_Array  : Maths.Vector8_Array (1 .. Vertices_Length);
   begin
      theEntry.Num_Indices := UInt (Indices_Length);

      for index in 1 ..  Vertices_Length loop
         Vertices_Array (index) :=
           (Vertices (index).Pos (X), Vertices (index).Pos (Y), Vertices (index).Pos (Z),
            Vertices (index).Tex (X), Vertices (index).Tex (Y),
            Vertices (index).Normal (X), Vertices (index).Normal (Y), Vertices (index).Normal (Z));
      end loop;
      Put_Line ("Meshes_24.Init_Mesh_Entry, Vertice Indices Lengths" &
                  Int'Image (Vertices_Length) & "  " &
                  Int'Image (Indices_Length));

      theEntry.Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (theEntry.Vertex_Buffer);
      Utilities.Load_Vector8_Buffer (Array_Buffer, Vertices_Array, Static_Draw);

      theEntry.Index_Buffer.Initialize_Id;
      Element_Array_Buffer.Bind (theEntry.Index_Buffer);
      Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices, Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in Meshes_24.Init_Mesh_Entry.");
         raise;
   end Init_Mesh_Entry;

   --  -------------------------------------------------------------------------

   procedure Load_Mesh (theMesh : in out Mesh_24; File_Name : String) is
      theScene : Scene.AI_Scene;
   begin
      theScene :=
        Importer.Read_File (File_Name, UInt (Ogldev_Util.Assimp_Basic_Load_Flags));
      Init_From_Scene (theMesh, File_Name, theScene);

   exception
      when others =>
         Put_Line ("An exception occurred in Meshes_24.Load_Mesh.");
         raise;
   end Load_Mesh;

   --  --------------------------------------------------------------------------

   procedure  Render (theMesh : Mesh_24) is
      use Mesh_Entry_Package;
      Entry_Cursor    : Cursor;
      anEntry         : Mesh_Entry;
      aMaterial_Index : UInt;
      aTexture        : Ogldev_Texture.Ogl_Texture;
   begin
      if theMesh.Entries.Is_Empty then
         raise Mesh_24_Error with "Meshes_24.Render theMesh.Entries is empty.";
      else
         Entry_Cursor := theMesh.Entries.First;
         GL.Attributes.Enable_Vertex_Attrib_Array (0);
         GL.Attributes.Enable_Vertex_Attrib_Array (1);
         GL.Attributes.Enable_Vertex_Attrib_Array (2);

         while Has_Element (Entry_Cursor) loop
            anEntry := Element (Entry_Cursor);
            GL.Objects.Buffers.Array_Buffer.Bind (anEntry.Vertex_Buffer);

            GL.Attributes.Set_Vertex_Attrib_Pointer
              (Index  => 0, Count => 3, Kind => Single_Type,
               Stride => 8, Offset => 0);
            GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, 8, 3);  --  texture
            GL.Attributes.Set_Vertex_Attrib_Pointer (2, 3, Single_Type, 8, 5);

            GL.Objects.Buffers.Element_Array_Buffer.Bind (anEntry.Index_Buffer);
            aMaterial_Index := anEntry.Material_Index;  --  normal

            if not theMesh.Textures.Is_Empty then
               if theMesh.Textures.Contains (aMaterial_Index) then
                  aTexture := theMesh.Textures.Element (aMaterial_Index);
                  if aTexture.Texture_Object.Initialized then
                     Ogldev_Texture.Bind (aTexture, 0);
                     if not GL.Objects.Textures.Is_Texture
                       (GL.Objects.Raw_Id (GL.Objects.GL_Object (aTexture.Texture_Object))) then
                        Put_Line ("Meshes_24.Render_Mesh, aTexture is invalid.");
                     end if;
                  else
                     Put_Line ("Meshes_24.Render_Mesh, Texture_Object is not initialized.");
                  end if;
               else
                  Put_Line ("Meshes_24.Render_Mesh, theMesh.Textures does not contain Material: " &
                              UInt'Image (aMaterial_Index));
               end if;
            end if;

            GL.Objects.Buffers.Draw_Elements
              (GL.Types.Triangles, GL.Types.Int (anEntry.Num_Indices), UInt_Type);
            Next (Entry_Cursor);
         end loop;

         GL.Attributes.Disable_Vertex_Attrib_Array (0);
         GL.Attributes.Disable_Vertex_Attrib_Array (1);
         GL.Attributes.Disable_Vertex_Attrib_Array (2);
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Meshes_24.Render.");
         raise;
   end Render;

   --  --------------------------------------------------------------------------

end Meshes_24;
