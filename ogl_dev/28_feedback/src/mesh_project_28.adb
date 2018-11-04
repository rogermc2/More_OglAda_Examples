with System;

with Interfaces.C;
with Interfaces.C.Pointers;

with GNAT.Directory_Operations;

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Low_Level.Enums;

with Maths;
with Utilities;

with Assimp_Types; use Assimp_Types;
with Importer;

with Ogldev_Util;

with Assimp_Mesh;
with Material;
with Material_System;
with Scene;

package body Mesh_Project_28 is
    use GL.Types;
   type Vertex is record
      Pos    : GL.Types.Singles.Vector3;
      Tex    : GL.Types.Singles.Vector2;
      Normal : GL.Types.Singles.Vector3;
   end record;
   type Vertex_Array is array (Int range <>) of Vertex;

   Mesh_28_Exception : Exception;

   procedure Init_Materials (theMesh  : in out Mesh_28; File_Name : String;
                             theScene : Scene.AI_Scene);
   procedure Init_Mesh (Mesh_Index : UInt; Source_Mesh : Assimp_Mesh.AI_Mesh;
                        aMesh_28   : in out Mesh_28);

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
      theEntry.Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (theEntry.Vertex_Buffer);
      theEntry.Index_Buffer.Initialize_Id;
      Element_Array_Buffer.Bind (theEntry.Index_Buffer);

      for index in 1 ..  Vertices_Length loop
         Vertices_Array (index) :=
           (Vertices (index).Pos (X), Vertices (index).Pos (Y), Vertices (index).Pos (Z),
            Vertices (index).Tex (X), Vertices (index).Tex (Y),
            Vertices (index).Normal (X), Vertices (index).Normal (Y), Vertices (index).Normal (Z));
      end loop;

      Array_Buffer.Bind (theEntry.Vertex_Buffer);
      Utilities.Load_Vector8_Buffer (Array_Buffer, Vertices_Array, Static_Draw);
      Element_Array_Buffer.Bind (theEntry.Index_Buffer);
      Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices, Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in Mesh_Project_28.Init_Buffers.");
         raise;

   end Init_Buffers;

   --  -------------------------------------------------------------------------

   procedure Init_From_Scene (Initialized_Mesh : in out Mesh_28;
                              File_Name : String;
                              theScene : Scene.AI_Scene) is
      use Assimp_Mesh.AI_Mesh_Package;
      Curs         : Cursor := theScene.Meshes.First;
      Mesh_Index   : UInt := 0;
      aMesh        : Assimp_Mesh.AI_Mesh;
   begin
      --  Initialized_Mesh works because there is only one mesh
      --  Initialized_Mesh contains vertices and textures maps
      while Has_Element (Curs) loop
         Mesh_Index := Mesh_Index + 1;
         aMesh := theScene.Meshes (Mesh_Index);
         Init_Mesh (Mesh_Index, aMesh, Initialized_Mesh);
         Init_Materials (Initialized_Mesh, File_Name, theScene);
         Next (Curs);
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Project_22_Mesh.Init_From_Scene.");
         raise;

   end Init_From_Scene;

   --  -------------------------------------------------------------------------

   procedure Init_Materials (theMesh  : in out Mesh_28; File_Name : String;
                             theScene : Scene.AI_Scene) is
      use Material.AI_Material_Package;
      use Assimp_Types;
      use Material;

      --  Extract the directory part from the file name
      Dir           : constant GNAT.Directory_Operations.Dir_Name_Str
        := GNAT.Directory_Operations.Dir_Name (File_Name);
      Path          : Ada.Strings.Unbounded.Unbounded_String;
      Result        : Assimp_Types.API_Return;
      Materials_Map : constant AI_Material_Map := theScene.Materials;

      procedure Load_Textures (Material_Curs : AI_Material_Package.Cursor) is
         use Ada.Strings.Unbounded;
         use Ogldev_Texture.Mesh_Texture_Package;
         aMaterial  : constant AI_Material := Element (Material_Curs);
         aTexture   : Ogldev_Texture.Ogl_Texture;
         Index      : constant GL.Types.UInt := Key (Material_Curs);
      begin
         if Get_Texture_Count (aMaterial, AI_Texture_Diffuse) > 0 then
            Result := Material_System.Get_Texture
              (aMaterial, AI_Texture_Diffuse, 0, Path);

            if Result = Assimp_Types.API_Return_Success then
               if Ogldev_Texture.Init_Texture
                 (aTexture, GL.Low_Level.Enums.Texture_2D,
                  Dir & To_String (Path)) then
                  Ogldev_Texture.Load (aTexture);
                  theMesh.Textures.Insert (index, aTexture);
                  Put_Line ("Mesh_Project_28.Init_Materials.Load_Textures loaded texture from "
                            & Dir & To_String (Path));
               elsif Ogldev_Texture.Init_Texture
                 (aTexture, GL.Low_Level.Enums.Texture_2D, Dir & "white.png") then
                  Ogldev_Texture.Load (aTexture);
                  theMesh.Textures.Insert (index, aTexture);
                  New_Line;
                  Put_Line ("Mesh_Project_28.Init_Materials.Load_Textures loaded default texture from "
                            & Dir & "white.png");
               else
                  Put_Line ("Mesh_Project_28.Init_Materials.Load_Textures default texture "
                            & Dir & "white.png not found.");
               end if;
            else
               Put_Line ("Mesh_Project_28.Init_Materials.Load_Textures Get_Texture failed");
            end if;
         end if;
      end Load_Textures;

   begin
      New_Line;
      Materials_Map.Iterate (Load_Textures'Access);

   exception
      when others =>
         Put_Line ("An exception occurred in Mesh_Project_28.Init_Materials.");
         raise;

   end Init_Materials;

   --  -------------------------------------------------------------------------

  procedure Init_Mesh (Mesh_Index : UInt; Source_Mesh : Assimp_Mesh.AI_Mesh;
                        aMesh_28   : in out Mesh_28) is
      use Ada.Containers;
      use Mesh_Entry_Package;
      Num_Vertices  : constant UInt := UInt (Source_Mesh.Vertices.Length);
      Vertices      : Vertex_Array (1 .. Int (Num_Vertices));
      Indices       : GL.Types.UInt_Array (1 .. Int (3 * Source_Mesh.Faces.Length));
      anEntry       : Mesh_Entry;
      Position      : GL.Types.Singles.Vector3;
      Normal        : GL.Types.Singles.Vector3;
      Contains_1    : constant Boolean := Source_Mesh.Texture_Coords.Contains (1);
      Tex_Coord_Map : Assimp_Mesh.Vertices_Map;
      Tex_Coord     : GL.Types.Singles.Vector3;
      Face          : Assimp_Mesh.AI_Face;
      Index_Index   : Int := 0;
   begin
      anEntry.Material_Index := Source_Mesh.Material_Index;
      if Contains_1 then
         Tex_Coord_Map := Source_Mesh.Texture_Coords.Element (1);
      end if;

      for V_Index in 1 .. Num_Vertices loop
         Position := Source_Mesh.Vertices.Element (V_Index);
         Normal := Source_Mesh.Normals.Element (V_Index);
         if Contains_1 then
            if Tex_Coord_Map.Contains (V_Index) then
               Tex_Coord := Tex_Coord_Map.Element (V_Index);
            else
               Tex_Coord := (0.0, 0.0, 0.0);
            end if;
         else
            Tex_Coord := (0.0, 0.0, 0.0);
         end if;
         Vertices (Int (V_Index)) :=
           (Position, (Tex_Coord (GL.X), Tex_Coord (GL.Y)), Normal);
      end loop;

      if Source_Mesh.Faces.Is_Empty then
         Put_Line ("Mesh_Project_28.Init_Mesh, Source_Mesh.Faces is empty.");
      else
         for Face_Index in 1 .. Source_Mesh.Faces.Length loop
            Face := Source_Mesh.Faces.Element (UInt (Face_Index));
            Index_Index := Index_Index + 1;
            Indices (Index_Index) := Face.Indices (1);
            Index_Index := Index_Index + 1;
            Indices (Index_Index) := Face.Indices (2);
            Index_Index := Index_Index + 1;
            Indices (Index_Index) := Face.Indices (3);
         end loop;
      end if;

      --  m_Entries[Index].Init(Vertices, Indices);
      Init_Buffers (anEntry, Vertices, Indices);
      aMesh_28.Entries.Insert (Integer (Mesh_Index), anEntry);

   exception
      when others =>
         Put_Line ("An exception occurred in Mesh_Project_28.Init_Mesh.");
         raise;
   end Init_Mesh;

   --  -------------------------------------------------------------------------

   procedure Load_Mesh (theMesh : in out Mesh_28; File_Name : String) is
      theScene : Scene.AI_Scene;
   begin
      theScene :=
        Importer.Read_File (File_Name, UInt (Ogldev_Util.Assimp_Load_Flags));
      Init_From_Scene (theMesh, File_Name, theScene);

   exception
      when others =>
         Put_Line ("An exception occurred in Mesh_Project_28.Load_Mesh.");
         raise;
   end Load_Mesh;

   --  --------------------------------------------------------------------------

procedure  Render (theMesh : Mesh_28) is

   begin
        null;
   end Render;

   --  --------------------------------------------------------------------------

end Mesh_Project_28;
