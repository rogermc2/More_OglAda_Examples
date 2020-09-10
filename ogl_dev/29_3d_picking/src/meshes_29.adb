with System;

with Interfaces.C;
with Interfaces.C.Pointers;

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Low_Level.Enums;

with Maths;
with Utilities;

with Assimp_Types; use Assimp_Types;
with Importer;

with Ogldev_Engine_Common;
with Ogldev_Math;
with Ogldev_Util;

with Assimp_Mesh;
with Material;
with Material_System;
with Post_Process;
with Scene;

package body Meshes_29 is
   use GL.Types;
   type Vertex is record
      Pos     : GL.Types.Singles.Vector3;
      Tex     : GL.Types.Singles.Vector2;
      Normal  : GL.Types.Singles.Vector3;
   end record;
   type Vertex_Array is array (Int range <>) of Vertex;

   Mesh_29_Exception : Exception;

   procedure Init_Materials (theMesh  : in out Mesh_29; File_Name : String;
                             theScene : Scene.AI_Scene);
   procedure Init_Mesh (Mesh_Index : UInt; Source_Mesh : Assimp_Mesh.AI_Mesh;
                        aMesh_29   : in out Mesh_29);

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
           (Vertices (index).Pos (X), Vertices (index).Pos (Y),
            Vertices (index).Pos (Z), Vertices (index).Tex (X),
            Vertices (index).Tex (Y), Vertices (index).Normal (X),
            Vertices (index).Normal (Y), Vertices (index).Normal (Z));
      end loop;

      Array_Buffer.Bind (theEntry.Vertex_Buffer);
      Utilities.Load_Vector8_Buffer (Array_Buffer, Vertices_Array, Static_Draw);
      Element_Array_Buffer.Bind (theEntry.Index_Buffer);
      Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices, Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in Meshes_29.Init_Buffers.");
         raise;

   end Init_Buffers;

   --  -------------------------------------------------------------------------

   procedure Init_From_Scene (Initialized_Mesh : in out Mesh_29;
                              File_Name        : String;
                              theScene         : Scene.AI_Scene) is
      use Assimp_Mesh.AI_Mesh_Package;
      Curs         : Cursor := theScene.Meshes.First;
      Mesh_Index   : UInt := 0;
      aMesh        : Assimp_Mesh.AI_Mesh;
   begin
      Put_Line ("Meshes_29.Init_From_Scene, initializing " &
                  File_Name);
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
         Put_Line ("An exception occurred in Meshes_29.Init_From_Scene.");
         raise;

   end Init_From_Scene;

   --  -------------------------------------------------------------------------

   procedure Init_Materials (theMesh  : in out Mesh_29; File_Name : String;
                             theScene : Scene.AI_Scene) is
      use Material.AI_Material_Package;
      use Assimp_Types;
      use Material;
      Dir           : constant String
        := Ada.Directories.Containing_Directory (File_Name) & "/";
      Path          : Ada.Strings.Unbounded.Unbounded_String;
      Materials_Map : constant AI_Material_Map := theScene.Materials;
      Result        : Assimp_Types.API_Return := Assimp_Types.API_Return_Success;

      procedure Load_Textures (Material_Curs : AI_Material_Package.Cursor) is
         use Ada.Strings.Unbounded;
         use Ogldev_Texture.Mesh_Texture_Package;
         aMaterial  : constant AI_Material := Element (Material_Curs);
         aTexture   : Ogldev_Texture.Ogl_Texture;
         Index      : constant GL.Types.UInt := Key (Material_Curs);
      begin
--           Put_Line ("Meshes_29.Init_Materials.Load_Textures: Texture_Count: "
--                     & UInt'Image (Get_Texture_Count (aMaterial, AI_Texture_Diffuse)));
         if Result = Assimp_Types.API_Return_Success and then
           Get_Texture_Count (aMaterial, AI_Texture_Diffuse) > 0 then
            Result := Material_System.Get_Texture
              (aMaterial, AI_Texture_Diffuse, 0, Path);
            Put_Line ("Meshes_29.Init_Materials.Load_Textures: Get_Texture result: "
                      & Assimp_Types.API_Return'Image (Result));

            if Result = Assimp_Types.API_Return_Success then
               if Ogldev_Texture.Init_Texture
                 (aTexture, GL.Low_Level.Enums.Texture_2D,
                  Dir & To_String (Path)) then
                  Ogldev_Texture.Load (aTexture);
                  theMesh.Textures.Insert (index, aTexture);
                  Put_Line ("Meshes_29.Init_Materials.Load_Textures loaded texture from "
                            & Dir & To_String (Path));
               elsif Ogldev_Texture.Init_Texture
                 (aTexture, GL.Low_Level.Enums.Texture_2D, Dir & "white.png") then
                  Ogldev_Texture.Load (aTexture);
                  theMesh.Textures.Insert (index, aTexture);
                  Put_Line ("Meshes_29.Init_Materials.Load_Textures loaded default texture from "
                            & Dir & "white.png");
               else
                  Put_Line ("Meshes_29.Init_Materials.Load_Textures default texture "
                            & Dir & "white.png not found.");
               end if;
            else
               Put_Line ("Meshes_29.Init_Materials.Load_Textures Get_Texture failed");
            end if;
         end if;
      end Load_Textures;

   begin
      Materials_Map.Iterate (Load_Textures'Access);

   exception
      when others =>
         Put_Line ("An exception occurred in Meshes_29.Assimp_Types.API_Return_Success.");
         raise;

   end Init_Materials;

   --  -------------------------------------------------------------------------

   procedure Init_Mesh (Mesh_Index : UInt; Source_Mesh : Assimp_Mesh.AI_Mesh;
                        aMesh_29   : in out Mesh_29) is
      use Ada.Containers;
      use Mesh_Entry_Package;
      Num_Vertices     : constant UInt := UInt (Source_Mesh.Vertices.Length);
      Vertices         : Vertex_Array (1 .. Int (Num_Vertices));
      Indices          : GL.Types.UInt_Array (1 .. Int (3 * Source_Mesh.Faces.Length));
      anEntry          : Mesh_Entry;
      Position         : GL.Types.Singles.Vector3;
      Normal           : GL.Types.Singles.Vector3;
      Tex_Coord_Map    : constant Assimp_Mesh.Texture_Coords_Map :=
                           Source_Mesh.Texture_Coords;
      Tex_Vertices_Map : Assimp_Mesh.Vertices_Map;
      Tex_Coord        : GL.Types.Singles.Vector3;
      Face             : Assimp_Mesh.AI_Face;
      Indices_Index    : Int := 0;
   begin
      anEntry.Material_Index := Source_Mesh.Material_Index;
      if Source_Mesh.Texture_Coords.Is_Empty then
         Put_Line ("Meshes_29.Init_Mesh, Tex_Coord_Map is empty.");
      else
         Tex_Vertices_Map := Tex_Coord_Map.Element (1);
      end if;

      for V_Index in 1 .. Num_Vertices loop
         Position := Source_Mesh.Vertices.Element (V_Index);
         Normal := Source_Mesh.Normals.Element (V_Index);

         if Tex_Vertices_Map.Contains (V_Index) then
            Tex_Coord := Tex_Vertices_Map (V_Index);
         else
            Put_Line ("Meshes_29.Init_Mesh, Tex_Vertices_Map is empty.");
            Tex_Coord := (0.0, 0.0, 0.0);
         end if;

         Vertices (Int (V_Index)) :=
           (Position, (Tex_Coord (GL.X), Tex_Coord (GL.Y)), Normal);
      end loop;

      if Source_Mesh.Faces.Is_Empty then
         Put_Line ("Meshes_29.Init_Mesh, Source_Mesh.Faces is empty.");
      else
--           Put_Line ("Meshes_29.Init_Mesh, Source_Mesh.Faces size: " &
--                       Count_Type'Image (Source_Mesh.Faces.Length));
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
      Init_Buffers (anEntry, Vertices, Indices);
      aMesh_29.Entries.Insert (Integer (Mesh_Index), anEntry);

   exception
      when others =>
         Put_Line ("An exception occurred in Meshes_29.Init_Mesh.");
         raise;
   end Init_Mesh;

   --  -------------------------------------------------------------------------

   procedure Load_Mesh (theMesh : in out Mesh_29; File_Name : String) is
      use Interfaces.C;
      use Post_Process;
      theScene   : Scene.AI_Scene;
      Load_Flags : constant unsigned :=
                     AI_Process_Triangulate'Enum_Rep + AI_Process_Gen_Smooth_Normals'Enum_Rep +
                       AI_Process_Flip_UVs'Enum_Rep + AI_Process_Calc_Tangent_Space'Enum_Rep;
      pragma Convention (C, Load_Flags);
   begin
      theScene :=
        Importer.Read_File (File_Name, UInt (Load_Flags));
      Init_From_Scene (theMesh, File_Name, theScene);

   exception
      when others =>
         Put_Line ("An exception occurred in Meshes_29.Load_Mesh.");
         raise;
   end Load_Mesh;

   --  --------------------------------------------------------------------------

   procedure  Render (theMesh : Mesh_29) is
      use Mesh_Entry_Package;
      Entry_Cursor   : Cursor := theMesh.Entries.First;
      anEntry        : Mesh_Entry;
      Textures       : Ogldev_Texture.Mesh_Texture_Map;
      aTexture       : Ogldev_Texture.Ogl_Texture;
   begin
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
--        GL.Attributes.Enable_Vertex_Attrib_Array (1);
--        GL.Attributes.Enable_Vertex_Attrib_Array (2);

      while Has_Element (Entry_Cursor) loop
         anEntry := Element (Entry_Cursor);
         GL.Objects.Buffers.Array_Buffer.Bind (anEntry.Vertex_Buffer);

         GL.Attributes.Set_Vertex_Attrib_Pointer
           (Index  => 0, Count => 3, Kind => Single_Type, Stride => 8, Offset => 0);
--           GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, 8, 3);  --  texture
--           GL.Attributes.Set_Vertex_Attrib_Pointer (2, 3, Single_Type, 8, 5);  --  normal

         GL.Objects.Buffers.Element_Array_Buffer.Bind (anEntry.Index_Buffer);
         if Textures.Contains (anEntry.Material_Index) then
            aTexture := Textures.Element (anEntry.Material_Index);
            Ogldev_Texture.Bind (aTexture, Ogldev_Engine_Common.Colour_Texture_Unit);
            Put_Line ("Meshes_29.Render aTexture bound.");
         end if;

--           Put_Line ("Meshes_29.Render anEntry.Num_Indices: " &
--                       UInt'Image (anEntry.Num_Indices));
         GL.Objects.Buffers.Draw_Elements
           (GL.Types.Triangles, GL.Types.Int (anEntry.Num_Indices), UInt_Type);
         Next (Entry_Cursor);
      end loop;

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
--        GL.Attributes.Disable_Vertex_Attrib_Array (1);
--        GL.Attributes.Disable_Vertex_Attrib_Array (2);
   end Render;

   --  --------------------------------------------------------------------------

   procedure  Render (theMesh             : Mesh_29;
                      Draw_Index, Prim_ID : GL.Types.UInt) is
      use Mesh_Entry_Package;
      Entry_Cursor   : Cursor := theMesh.Entries.First;
      Count          : GL.Types.UInt := 0;
      anEntry        : Mesh_Entry;
   begin
      if Draw_Index <= GL.Types.UInt (Length (theMesh.Entries)) then
         GL.Attributes.Enable_Vertex_Attrib_Array (0);
         GL.Attributes.Enable_Vertex_Attrib_Array (1);
         GL.Attributes.Enable_Vertex_Attrib_Array (2);

         while Has_Element (Entry_Cursor) and Count /= Draw_Index loop
            Count := Count + 1;
            if Count /= Draw_Index then
               Next (Entry_Cursor);
            end if;
         end loop;
         anEntry := Element (Entry_Cursor);

         GL.Objects.Buffers.Array_Buffer.Bind (anEntry.Vertex_Buffer);

         GL.Attributes.Set_Vertex_Attrib_Pointer
           (Index  => 0, Count => 3, Kind => Single_Type, Stride => 8, Offset => 0);
         GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, 8, 3);  --  texture
         GL.Attributes.Set_Vertex_Attrib_Pointer (2, 3, Single_Type, 8, 5);  --  normal

         GL.Objects.Buffers.Element_Array_Buffer.Bind (anEntry.Index_Buffer);

         GL.Objects.Buffers.Draw_Elements
           (GL.Types.Triangles, GL.Types.Int (3 * Prim_ID), UInt_Type);

         GL.Attributes.Disable_Vertex_Attrib_Array (0);
         GL.Attributes.Disable_Vertex_Attrib_Array (1);
         GL.Attributes.Disable_Vertex_Attrib_Array (2);
      else
         Put_Line ("Meshes_29.Render, invalid Draw_Index: " &
                     GL.Types.UInt'Image (Draw_Index));
      end if;

   end Render;

   --  --------------------------------------------------------------------------

end Meshes_29;