
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Low_Level.Enums;

with Utilities;

with Assimp_Types;

--  with C_Import;
with Material;

with Ogldev_Engine_Common;
with Ogldev_Basic_Mesh;
--  with Ogldev_Util;
with Scene;

package body Mesh_22 is

   Position_Location  : constant GL.Attributes.Attribute := 0;
   Tex_Coord_Location : constant GL.Attributes.Attribute := 1;
   Normal_Location    : constant GL.Attributes.Attribute := 2;

--     procedure Init_Materials (Initial_Mesh : in out Mesh_22;
--                               File_Name : String;
--                               theScene : Scene.AI_Scene);

   --  -------------------------------------------------------------------------

   procedure Init_From_Scene (Initial_Mesh : in out Mesh;
                              File_Name : String;
                              theScene : Scene.AI_Scene) is
      use Mesh_Entry_Package;
      Num_Vertices : constant UInt := 0;
      Num_Indices  : constant UInt := 0;
      Curs         : Cursor := Initial_Mesh.Entries.First;
      Index        : UInt := 0;
--        aMesh        : Mesh.AI_Mesh;
--        anEntry      : Mesh_Entry;
   begin
      while Has_Element (Curs) loop
         Index := Index + 1;
--           aMesh := theScene.Meshes (Index);
--           Set_Entry (anEntry, Num_Indices, Num_Vertices, 3 * aMesh.Num_Faces,
--                      Material_Type'Enum_Val (aMesh.Material_Index));
--           Mesh_Entry_Package.Replace_Element (Initial_Mesh.Entries, Curs, anEntry);
--           Num_Vertices := Num_Vertices + aMesh.Num_Vertices;
--           Num_Indices := Num_Indices + anEntry.Num_Indices;
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
--              aMesh := theScene.Meshes (Index);
--              Init_Mesh (aMesh, Positions, Normals, Tex_Coords, Indices);
            Next (Curs);
         end loop;

--           Init_Materials (Initial_Mesh, File_Name, theScene);
--           Init_Buffers (Initial_Mesh.Buffers, Positions, Normals,
--                         Tex_Coords, Indices);
      end; --  declare block;

   exception
      when others =>
         Put_Line ("An exception occurred in Ogldev_Basic_Mesh.Init_From_Scene.");
         raise;
   end Init_From_Scene;

   -------------------------------------------------------------------------

   procedure Init_Materials (Initial_Mesh : in out Mesh;
                             File_Name : String;
                             theScene : Scene.AI_Scene) is
      use Material.AI_Material_Package;
      use Assimp_Types;
      use Material;
--        Current_Dir : constant GNAT.Directory_Operations.Dir_Name_Str
--          := GNAT.Directory_Operations.Get_Current_Dir;
--        Dir         : constant GNAT.Directory_Operations.Dir_Name_Str
--          := GNAT.Directory_Operations.Dir_Name (File_Name);
      Path          : Ada.Strings.Unbounded.Unbounded_String;
--        Full_Path   : constant String := Current_Dir & Dir;
      Result        : Assimp_Types.AI_Return;
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
               Get_Texture (aMaterial, AI_Texture_Diffuse, UInt (index), Path, Result);
               if Result = Assimp_Types.AI_Return_Success then
                  Ogldev_Texture.Init_Texture
                    (aTexture, GL.Low_Level.Enums.Texture_2D, File_Name);
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

   -------------------------------------------------------------------------

--     procedure Init_Mesh (aMesh : Mesh.AI_Mesh;
--                          Positions, Normals : out GL.Types.Singles.Vector3_Array;
--                          Tex_Coords : out GL.Types.Singles.Vector2_Array;
--                          Indices : out GL.Types.UInt_Array) is
   procedure Init_Mesh (aMesh : in out Mesh; Index : GL.Types.UInt;
                        Base_Mesh : Ogldev_Basic_Mesh.AI_Mesh) is
      use Ada.Containers;
      use Mesh_Entry_Package;
      anEntry  : Mesh_Entry;
      Num_Vertices : Int := 1;
   begin
       anEntry := aMesh.Entries.Element (Index);
       declare
            Vertices : GL.Types.Singles.Vector3_Array (1 .. Num_Vertices);
            Indices : GL.Types.UInt_Array (1 .. Num_Vertices);
       begin
            null;
       end;


   exception
      when others =>
         Put_Line ("An exception occurred in Mesh.Init_Mesh.");
         raise;
   end Init_Mesh;

  --  -------------------------------------------------------------------------

   procedure Init_Mesh_Entry (aMesh : in out Mesh;
                              Vertices : GL.Types.Singles.Vector3_Array;
                              Indices : GL.Types.UInt_Array) is
        use GL.Objects.Buffers;
   begin
        aMesh.Basic_Entry.VBO.Initialize_Id;
        Array_Buffer.Bind (aMesh.Basic_Entry.VBO);
        Array_Buffer.Bind (aMesh.Basic_Entry.VBO);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);

        aMesh.Basic_Entry.IBO.Initialize_Id;
        Element_Array_Buffer.Bind (aMesh.Basic_Entry.IBO);
        Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices, Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in Mesh.Init_Mesh_Entry.");
         raise;
   end Init_Mesh_Entry;

  --  -------------------------------------------------------------------------

   procedure Load_Mesh (theMesh : in out Mesh; File_Name : String) is
      theScene : Scene.AI_Scene;
   begin
      Put_Line (" Mesh.Load_Mesh, import scene.");
--        theScene :=
--          C_Import.Import_File (File_Name, UInt (Ogldev_Util.Assimp_Load_Flags));
      theMesh.VAO.Initialize_Id;
      theMesh.VAO.Bind;
      --   Create the buffers for the vertices attributes
      for index in 1 .. Num_Buffers loop
         theMesh.Buffers (index).Initialize_Id;
      end loop;

--        Init_From_Scene (theMesh, File_Name, theScene);

   exception
      when others =>
         Put_Line ("An exception occurred in Ogldev_Basic_Mesh.Load_Mesh.");
         raise;
   end Load_Mesh;

   -------------------------------------------------------------------------

   procedure Render_Mesh (theMesh : Mesh) is
      use Ada.Containers;
      use Mesh_Entry_Package;
      procedure Draw (Entry_Cursor :  Mesh_Entry_Package.Cursor) is
         use Ogldev_Texture.Mesh_Texture_Package;
         Material_Kind  : constant Material_Type
           := Element (Entry_Cursor).Material_Index;
         Material_Index :  Material_Type;
         Tex_Curs       : Ogldev_Texture.Mesh_Texture_Package.Cursor;
         Num_Indices    : constant UInt := Element (Entry_Cursor).Num_Indices;
      begin
         GL.Objects.Buffers.Array_Buffer.Bind (Element (Entry_Cursor).VBO);
         GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);
         GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, 0, 12);
         GL.Attributes.Set_Vertex_Attrib_Pointer (2, 3, Single_Type, 0, 20);

         GL.Objects.Buffers.Element_Array_Buffer.Bind (Element (Entry_Cursor).IBO);
         Material_Index := Element (Entry_Cursor).Material_Index;
         if Material_Index'Enum_Rep < theMesh.Textures.Length then
            if not theMesh.Textures.Is_Empty then
               Ogldev_Texture.Bind (Element (Tex_Curs),
                                    Ogldev_Engine_Common.Colour_Texture_Unit_Index);
            end if;
            GL.Objects.Buffers.Draw_Elements_Base_Vertex
              (Triangles, Num_Indices, UInt_Type,
               Element (Entry_Cursor).Base_Index,
               Element (Entry_Cursor).Base_Vertex);
         else
            Put_Line ("Ogldev_Basic_Mesh.Render_Mesh, Invalid Material_Index.");
         end if;
      end Draw;
   begin
      theMesh.Basic_Entry.VAO.Bind;
         GL.Attributes.Enable_Vertex_Attrib_Array (0);
         GL.Attributes.Enable_Vertex_Attrib_Array (1);
         GL.Attributes.Enable_Vertex_Attrib_Array (2);

      Iterate (theMesh.Entries, Draw'Access);

         GL.Attributes.Disable_Vertex_Attrib_Array (0);
         GL.Attributes.Disable_Vertex_Attrib_Array (1);
         GL.Attributes.Disable_Vertex_Attrib_Array (2);

   exception
      when others =>
         Put_Line ("An exception occurred in Ogldev_Basic_Mesh.Render_Mesh .");
         raise;
   end Render_Mesh;

   --  -------------------------------------------------------------------------

   procedure Render (theMesh : Mesh;
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

   procedure Set_Base_Vertex (theEntry : in out Mesh_Entry;
                              Base_Vertex : UInt) is
   begin
      theEntry.Base_Vertex := Base_Vertex;
   end Set_Base_Vertex;

   --  -------------------------------------------------------------------------

   procedure Set_Base_Index (theEntry : in out Mesh_Entry;
                             Base_Index : UInt) is
   begin
      theEntry.Base_Index := Base_Index;
   end Set_Base_Index;

   --  -------------------------------------------------------------------------

   procedure Set_Entry (theEntry : in out Mesh_Entry;
                        Base_Index, Base_Vertex, Num_Indices : UInt;
                        Material : Material_Type) is
   begin
      theEntry.Num_Indices := Num_Indices;
      theEntry.Base_Vertex := Base_Vertex;
      theEntry.Base_Index := Base_Index;
      theEntry.Material_Index := Material;
   end Set_Entry;

   --  -------------------------------------------------------------------------

   procedure Set_Material_Type (theEntry : in out Mesh_Entry;
                                Material : Material_Type) is
   begin
      theEntry.Material_Index := Material;
   end Set_Material_Type;

   --  -------------------------------------------------------------------------

   procedure Set_Num_Indices (theEntry : in out Mesh_Entry; Num : UInt) is
   begin
      theEntry.Num_Indices := Num;
   end Set_Num_Indices;

   --  -------------------------------------------------------------------------

end Mesh_22;
