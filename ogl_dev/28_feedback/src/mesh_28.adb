with System;

with Interfaces.C.Pointers;

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

with Assimp_Types; use Assimp_Types;
with Importer;

with Ogldev_Util;

with Material;
with Mesh;
with Scene;

package body Mesh_28 is
    use GL.Types;

   type Vertex is record
        Position       : Singles.Vector3;
        Texture_Coords : Singles.Vector2;
        Normal         : Singles.Vector3;
        Tangent        : Singles.Vector3;
   end record;

   type Vertices_Array is array (UInt range <>) of aliased Vertex;

   Mesh_28_Exception : Exception;

   package Vertex_Pointers is new Interfaces.C.Pointers
     (UInt, Vertex, Vertices_Array, Vertex'(others => <>));

   procedure Init_Mesh (Index : Assimp_Types.Index_Int;
                        theMesh : in out Mesh_28;
                        PAI_Mesh : Mesh.AI_Mesh);
   procedure Init_Mesh_Entry (theMesh_Entry : in out Mesh_Entry;
                              Vertices : Vertices_Array;
                              Indices : UInt_Array);
   procedure Load_Vertex_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (Vertex_Pointers);

   --  -------------------------------------------------------------------------

   procedure Init_From_Scene (Initial_Mesh : in out Mesh_28;
                              File_Name : String;
                              theScene : Scene.AI_Scene) is
      Meshes   : Mesh_Array (1 .. theScene.Num_Meshes);
      PAI_Mesh : Mesh.AI_Mesh;
      aMesh    : Mesh_28;
   begin
      for index in 1 .. theScene.Num_Meshes loop
         PAI_Mesh := theScene.Meshes (index);
         Init_Mesh (index, aMesh, PAI_Mesh);
         Meshes (index) := aMesh;
      end loop;

   end Init_From_Scene;

   --  -------------------------------------------------------------------------

   procedure Init_Materials (theMesh : in out Mesh_28;
                             File_Name : String;
                             theScene : Scene.AI_Scene) is
      use Ada.Strings.Unbounded;
      Slash_Index : Integer := Ada.Strings.Fixed.Index (File_Name, "/");
      Dir         : Unbounded_String := To_Unbounded_String (".");
      aMaterial   : Material.AI_Material;

   begin
      if Slash_Index = 0 then
         Dir := To_Unbounded_String ("/");
      else
         Dir := To_Unbounded_String
           (File_Name (File_Name'First .. Slash_Index - 1));
      end if;
      for index in 1 .. theScene.Num_Materials loop
         aMaterial := theScene.Materials (index);
--           theMesh.Textures (Integer (index)) := System.Null_Address;
         if aMaterial
      end loop;

   end Init_Materials;

   --  -------------------------------------------------------------------------

   procedure Init_Mesh (Index : Assimp_Types.Index_Int;
                        theMesh : in out Mesh_28;
                        PAI_Mesh : Mesh.AI_Mesh) is
      use Mesh.Face_Index_Package;
      Vertices        : Vertices_Array (1 .. UInt (PAI_Mesh.Num_Vertices));
      Indices         : GL.Types.UInt_Array (1 .. Int (PAI_Mesh.Num_Vertices));
      Position        : GL.Types.Singles.Vector3;
      Normal          : GL.Types.Singles.Vector3;
      Tangent         : GL.Types.Singles.Vector3;
      Tex_Coords      : GL.Types.Singles.Vector3;
      aVertex         : Vertex;
      aFace           : Mesh.AI_Face;
      Face_Cursor     : Cursor;
      Index2          : GL.Types.Int := 0;
   begin
      theMesh.Entries (Index).Material_Index :=
        Ogldev_Basic_Mesh.Material_Type'Enum_Val (PAI_Mesh.Material_Index);
      for index in 1 .. PAI_Mesh.Num_Vertices loop
         Position := PAI_Mesh.Vertices (index);
         Normal := PAI_Mesh.Normals (index);
         Tangent := PAI_Mesh.Tangents (index);
         if PAI_Mesh.Texture_Coords'Length > 0 then
            Tex_Coords := PAI_Mesh.Texture_Coords (index);
         end if;
         aVertex.Position := (Position (GL.X), Position (GL.Y), Position (GL.Z));
         aVertex.Normal := (Normal (GL.X), Normal (GL.Y), Normal (GL.Z));
         aVertex.Tangent := (Tangent (GL.X), Tangent (GL.Y), Tangent (GL.Z));
         if PAI_Mesh.Texture_Coords'Length > 0 then
            aVertex.Texture_Coords := (Tex_Coords (GL.X), Tex_Coords (GL.Y));
         end if;
         Vertices (UInt (index)) := aVertex;
      end loop;

      for index in 1 .. PAI_Mesh.Num_Faces loop
         aFace := PAI_Mesh.Faces (index);
         Face_Cursor := aFace.First;
         while Has_Element (Face_Cursor) loop
            Index2 := Index2 + 1;
            if  Index2 > 3 then
               Put_Line ("Mesh_28.Init_Mesh, a Face has mor then 3 indices.");
               raise Mesh_28_Exception;
            end if;
            Indices (Index2) := Element (Face_Cursor);
            Next (Face_Cursor);
         end loop;
      end loop;
      Init_Mesh_Entry (theMesh.Entries (Index), Vertices, Indices);

   exception
      when others =>
         Put_Line ("An exception occurred in Mesh_28.Init_Mesh.");
         raise;
   end Init_Mesh;

   --  -------------------------------------------------------------------------

   procedure Init_Mesh_Entry (theMesh_Entry : in out Mesh_Entry;
                              Vertices : Vertices_Array;
                              Indices : UInt_Array) is
   use GL.Objects.Buffers;
   begin
        theMesh_Entry.Num_Indices := Indices'Length;
        theMesh_Entry.Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (theMesh_Entry.Vertex_Buffer);
        Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);

        theMesh_Entry.Index_Buffer.Initialize_Id;
        Element_Array_Buffer.Bind (theMesh_Entry.Index_Buffer);
        Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices, Static_Draw);
   end Init_Mesh_Entry;

   --  -------------------------------------------------------------------------

   procedure Load_Mesh (theMesh : in out Mesh_28; File_Name : String) is
        theScene : Scene.AI_Scene (1, 0, 0, theMesh.Textures'Length, 0, 1);
   begin
        theScene := Importer.Read_File (File_Name, Ogldev_Util.Assimp_Load_Flags);
        Init_From_Scene (theMesh, File_Name, theScene);
   end Load_Mesh;

   --  --------------------------------------------------------------------------

procedure  Render (theMesh : Mesh_28) is

   begin
        null;
   end Render;

   --  --------------------------------------------------------------------------

end Mesh_28;
