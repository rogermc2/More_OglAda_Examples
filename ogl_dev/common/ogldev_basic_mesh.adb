
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Directory_Operations;

with GL.Attributes;
with GL.Low_Level.Enums;

with Utilities;

with Assimp_Mesh;
with Assimp_Types;
with Importer;
with Material;
with Material_System;

with Ogldev_Engine_Common;
with Ogldev_Util;
with Scene;

package body Ogldev_Basic_Mesh is
   type Vertex is record
      Pos    : GL.Types.Singles.Vector3;
      Tex    : GL.Types.Singles.Vector2;
      Normal : GL.Types.Singles.Vector3;
   end record;
   type Vertex_Array is array (Int range <>) of Vertex;

   Position_Location  : constant GL.Attributes.Attribute := 0;
   Tex_Coord_Location : constant GL.Attributes.Attribute := 1;
   Normal_Location    : constant GL.Attributes.Attribute := 2;

   procedure Init_Mesh (Source_Mesh : Assimp_Mesh.AI_Mesh;
                        Positions, Normals : out GL.Types.Singles.Vector3_Array;
                        Tex_Coords : out GL.Types.Singles.Vector2_Array;
                        Indices : out GL.Types.UInt_Array);

   procedure Init_Materials (Initial_Mesh : in out Basic_Mesh;
                             File_Name : String;
                             theScene : Scene.AI_Scene);

   --  -------------------------------------------------------------------------

   function Get_Base_Vertex (theEntry : Basic_Mesh_Entry) return UInt is
   begin
      return theEntry.Base_Vertex;
   end Get_Base_Vertex;

   --  -------------------------------------------------------------------------

   function Get_Base_Index (theEntry : Basic_Mesh_Entry) return UInt is
   begin
      return theEntry.Base_Index;
   end Get_Base_Index;

   --  -------------------------------------------------------------------------

   function Get_Material_Index (theEntry : Basic_Mesh_Entry) return Material_Type is
   begin
      return theEntry.Material_Index;
   end Get_Material_Index;

   --  -------------------------------------------------------------------------

   function Get_Num_Indices (theEntry : Basic_Mesh_Entry) return UInt is
   begin
      return theEntry.Num_Indices;
   end Get_Num_Indices;

   --  -------------------------------------------------------------------------

   function Get_Orientation  (theMesh : Basic_Mesh) return Orientation is
   begin
      return theMesh.Direction;
   end Get_Orientation;

   --  -------------------------------------------------------------------------

   procedure Init_Buffers (Buffers : Mesh_Buffer_Array;
                           Positions : GL.Types.Singles.Vector3_Array;
                           Normals : GL.Types.Singles.Vector3_Array;
                           Tex_Coords : GL.Types.Singles.Vector2_Array;
                           Indices : GL.Types.UInt_Array) is
      use GL.Attributes;
      use GL.Objects.Buffers;
   begin
      Array_Buffer.Bind (Buffers (Pos_VB'Enum_Rep));
      Utilities.Load_Vertex_Buffer (Array_Buffer, Positions, Static_Draw);
      Enable_Vertex_Attrib_Array (Position_Location);
      Set_Vertex_Attrib_Pointer (Position_Location, 3, Single_Type, 0, 0);

      Array_Buffer.Bind (Buffers (Tex_Coord_VB'Enum_Rep));
      Utilities.Load_Vertex_Buffer (Array_Buffer, Tex_Coords, Static_Draw);
      Enable_Vertex_Attrib_Array (Tex_Coord_Location);
      Set_Vertex_Attrib_Pointer (Tex_Coord_Location, 2, Single_Type, 0, 0);

      Array_Buffer.Bind (Buffers (Normal_VB'Enum_Rep));
      Utilities.Load_Vertex_Buffer (Array_Buffer, Normals, Static_Draw);
      Enable_Vertex_Attrib_Array (Normal_Location);
      Set_Vertex_Attrib_Pointer (Normal_Location, 3, Single_Type, 0, 0);

      Element_Array_Buffer.Bind (Buffers (Index_Buffer'Enum_Rep + 1));
      Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices, Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in Ogldev_Basic_Mesh.Init_Buffers.");
         raise;
   end Init_Buffers;

   -------------------------------------------------------------------------

   procedure Init_From_Scene (Initial_Mesh : in out Basic_Mesh;
                              File_Name : String;
                              theScene : Scene.AI_Scene) is
      use Mesh_Entry_Package;
      Num_Vertices : UInt := 0;
      Num_Indices  : UInt := 0;
      Curs         : Cursor := Initial_Mesh.Entries.First;
      Index        : UInt := 0;
      aMesh        : Assimp_Mesh.AI_Mesh;
      anEntry      : Basic_Mesh_Entry;
   begin
      while Has_Element (Curs) loop
         Index := Index + 1;
         aMesh := theScene.Meshes (Index);
         Set_Entry (anEntry, Num_Indices, Num_Vertices, 3 * UInt (aMesh.Faces.Length),
                    Material_Type'Enum_Val (aMesh.Material_Index));
         Mesh_Entry_Package.Replace_Element (Initial_Mesh.Entries, Curs, anEntry);
         Num_Vertices := Num_Vertices + UInt (aMesh.Vertices.Length);
         Num_Indices := Num_Indices + anEntry.Num_Indices;
         Next (Curs);
      end loop;

      --  Initialize the meshes in the scene one by one
      declare
         Positions    : GL.Types.Singles.Vector3_Array (1 .. Int (Num_Vertices));
         Normals      : GL.Types.Singles.Vector3_Array (1 .. Int (Num_Vertices));
         Tex_Coords   : GL.Types.Singles.Vector2_Array (1 .. Int (Num_Vertices));
         Indices      : GL.Types.UInt_Array (1 .. Int (Num_Indices));
      begin
         Curs := Initial_Mesh.Entries.First;
         Index := 0;
         while Has_Element (Curs) loop
            Index := Index + 1;
            aMesh := theScene.Meshes (Index);
            Init_Mesh (aMesh, Positions, Normals, Tex_Coords, Indices);
            Next (Curs);
         end loop;

         Init_Materials (Initial_Mesh, File_Name, theScene);
         Init_Buffers (Initial_Mesh.Buffers, Positions, Normals,
                       Tex_Coords, Indices);
      end; --  declare block;

   exception
      when others =>
         Put_Line ("An exception occurred in Ogldev_Basic_Mesh.Init_From_Scene.");
         raise;
   end Init_From_Scene;

-----------------------------------------------------------------------

   procedure Init_Materials (Initial_Mesh : in out Basic_Mesh;
                             File_Name : String;
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

      procedure Load_Textures (Curs : AI_Material_Package.Cursor) is
      use Ogldev_Texture.Mesh_Texture_Package;
      aMaterial  : constant AI_Material := Element (Curs);
      aTexture   : Ogldev_Texture.Ogl_Texture;
      begin
         for index in 1 .. Initial_Mesh.Textures.Length loop
            Initial_Mesh.Textures.Delete_First;
         end loop;
         for index in 1 .. Initial_Mesh.Textures.Length loop
            if Get_Texture_Count (aMaterial, AI_Texture_Diffuse) > 0 then
               Result := Material_System.Get_Texture (aMaterial, AI_Texture_Diffuse, UInt (index), Path);
               if Result = Assimp_Types.API_Return_Success and then
                  Ogldev_Texture.Init_Texture
                    (aTexture, GL.Low_Level.Enums.Texture_2D, File_Name) then
                  Initial_Mesh.Textures.Insert (UInt (index), aTexture);
                  Ogldev_Texture.Load (aTexture);
                  Put_Line ("Init_Materials loaded texture from " & File_Name);
               end if;
            end if;
            end loop;
      end Load_Textures;

   begin
      Materials_Map.Iterate (Load_Textures'Access);

   exception
      when others =>
         Put_Line ("An exception occurred in Ogldev_Basic_Mesh.Init_Materials.");
         raise;
   end Init_Materials;

-----------------------------------------------------------------------

   procedure Init_Mesh (Source_Mesh : Assimp_Mesh.AI_Mesh;
                        Positions, Normals : out GL.Types.Singles.Vector3_Array;
                        Tex_Coords : out GL.Types.Singles.Vector2_Array;
                        Indices : out GL.Types.UInt_Array) is
      use Ada.Containers;
      use Assimp_Mesh.Faces_Package;
      Num_Vertices  : constant UInt := UInt (Source_Mesh.Vertices.Length);
      Tex_Coord_Map : Assimp_Mesh.Vertices_Map;
      Tex_Coord     : GL.Types.Singles.Vector3;
      Vertices      : Vertex_Array (1 .. Int (Num_Vertices));
      Face          : Assimp_Mesh.AI_Face;
      Index_Index   : Int := 0;
   begin
      --  Populate the vertex attribute vectors
      for V_Index in UInt range 1 .. UInt (Source_Mesh.Vertices.Length) loop
         Positions (Int (V_Index)) := Source_Mesh.Vertices.Element (V_Index);
         Normals (Int (V_Index)) := Source_Mesh.Normals.Element (V_Index);
         if V_Index <= UInt (Source_Mesh.Texture_Coords.Length) then
            if Tex_Coord_Map.Contains (V_Index) then
               Tex_Coord := Tex_Coord_Map.Element (V_Index);
            else
               Tex_Coord := (0.0, 0.0, 0.0);
            end if;
         else
            Tex_Coord := (0.0, 0.0, 0.0);
         end if;
         Tex_Coords (Int (V_Index)) := (Tex_Coord (GL.X), Tex_Coord (GL.Y));
         Vertices (Int (V_Index)) :=
           (Positions (Int (V_Index)), (Tex_Coord (GL.X), Tex_Coord (GL.Y)), Normals (Int (V_Index)));
       end loop;
      --  Populate the index buffer
      if Source_Mesh.Faces.Is_Empty then
         Put_Line ("Project_22_Mesh.Init_Mesh, Source_Mesh.Faces is empty.");
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

   exception
      when others =>
         Put_Line ("An exception occurred in Ogldev_Basic_Mesh.Init_Mesh.");
         raise;
   end Init_Mesh;

   -------------------------------------------------------------------------

   procedure Load_Mesh (theMesh : in out Basic_Mesh; File_Name : String) is
      theScene : Scene.AI_Scene;
   begin
      Put_Line (" Ogldev_Basic_Mesh.Load_Mesh, import scene.");
      theScene :=
        Importer.Import_File (File_Name, UInt (Ogldev_Util.Assimp_Load_Flags));
      theMesh.VAO.Initialize_Id;
      theMesh.VAO.Bind;

      Put_Line (" Ogldev_Basic_Mesh.Load_Mesh, scene imported.");
      --   Create the buffers for the vertices attributes
      for index in 1 .. Num_Buffers loop
         theMesh.Buffers (index).Initialize_Id;
      end loop;

      Init_From_Scene (theMesh, File_Name, theScene);

   exception
      when others =>
         Put_Line ("An exception occurred in Ogldev_Basic_Mesh.Load_Mesh.");
         raise;
   end Load_Mesh;

   -------------------------------------------------------------------------

   procedure Render (theMesh : Basic_Mesh) is
      use Ada.Containers;
      use Mesh_Entry_Package;
      procedure Draw (Entry_Cursor :  Mesh_Entry_Package.Cursor) is
         use Ogldev_Texture.Mesh_Texture_Package;
         Material_Kind  : constant Material_Type
           := Element (Entry_Cursor).Material_Index;
         Material_Index : constant Natural := Natural (Material_Kind'Enum_Rep);
         Tex_Curs       : Ogldev_Texture.Mesh_Texture_Package.Cursor;
         Num_Indices    : constant UInt := Element (Entry_Cursor).Num_Indices;
      begin
         if Material_Index < Natural (theMesh.Textures.Length) then
            if not theMesh.Textures.Is_Empty then
               null;
--                 Ogldev_Texture.Bind (Element (Tex_Curs),
--                                      Ogldev_Engine_Common.Colour_Texture_Unit_Index);
            end if;
            null;
--              GL.Objects.Buffers.Draw_Elements_Base_Vertex
--                (Triangles, Num_Indices, UInt_Type,
--                 Element (Entry_Cursor).Base_Index,
--                 Element (Entry_Cursor).Base_Vertex);
         else
            Put_Line ("Ogldev_Basic_Mesh.Render, Invalid Material_Index.");
         end if;
      end Draw;
   begin
      theMesh.VAO.Bind;
      Iterate (theMesh.Entries, Draw'Access);

   exception
      when others =>
         Put_Line ("An exception occurred in Ogldev_Basic_Mesh.Render 1.");
         raise;
   end Render;

   -------------------------------------------------------------------------

   procedure Render (theMesh : Basic_Mesh;
                     WVP_Matrix, World_Matrix : Singles.Matrix4_Array) is

      use GL.Objects.Buffers;
      use Mesh_Entry_Package;
   begin
      Array_Buffer.Bind (theMesh.Buffers (WVP_Matrix_VB'Enum_Rep));
      Utilities.Load_Texture_Buffer (Array_Buffer, WVP_Matrix, Dynamic_Draw);

      Array_Buffer.Bind (theMesh.Buffers (World_Matrix_VB'Enum_Rep));
      Utilities.Load_Texture_Buffer (Array_Buffer, World_Matrix, Dynamic_Draw);

      Render (theMesh);

   exception
      when others =>
         Put_Line ("An exception occurred in Ogldev_Basic_Mesh.Render 2.");
         raise;
   end Render;

   --  -------------------------------------------------------------------------

   procedure Set_Base_Vertex (theEntry : in out Basic_Mesh_Entry;
                              Base_Vertex : UInt) is
   begin
      theEntry.Base_Vertex := Base_Vertex;
   end Set_Base_Vertex;

   --  -------------------------------------------------------------------------

   procedure Set_Base_Index (theEntry : in out Basic_Mesh_Entry;
                             Base_Index : UInt) is
   begin
      theEntry.Base_Index := Base_Index;
   end Set_Base_Index;

   --  -------------------------------------------------------------------------

   procedure Set_Entry (theEntry : in out Basic_Mesh_Entry;
                        Base_Index, Base_Vertex, Num_Indices : UInt;
                        Material : Material_Type) is
   begin
      theEntry.Num_Indices := Num_Indices;
      theEntry.Base_Vertex := Base_Vertex;
      theEntry.Base_Index := Base_Index;
      theEntry.Material_Index := Material;
   end Set_Entry;

   --  -------------------------------------------------------------------------

   procedure Set_Material_Type (theEntry : in out Basic_Mesh_Entry;
                                Material : Material_Type) is
   begin
      theEntry.Material_Index := Material;
   end Set_Material_Type;

   --  -------------------------------------------------------------------------

   procedure Set_Num_Indices (theEntry : in out Basic_Mesh_Entry; Num : UInt) is
   begin
      theEntry.Num_Indices := Num;
   end Set_Num_Indices;

   --  -------------------------------------------------------------------------

end Ogldev_Basic_Mesh;
