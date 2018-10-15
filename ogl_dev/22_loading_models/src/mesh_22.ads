
with Ada.Containers.Indefinite_Ordered_Maps;

with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;

with Ogldev_Texture;

Package Mesh_22 is

   type Material_Type is (Index_Buffer, Pos_VB, Normal_VB,
                          Tex_Coord_VB, WVP_Matrix_VB,
                          World_Matrix_VB, Invalid_Material);
   pragma Convention (C, Material_Type);

   type Mesh_22 is private;
   type Mesh_Entry is private;

   procedure Load_Mesh (theMesh : in out Mesh_22; File_Name : String);
   function Mesh_Entries_Size (aMesh : Mesh_22) return UInt;
   procedure Render_Mesh (theMesh : Mesh_22);

private
   Num_Buffers : constant UInt := 6;

   for Material_Type use (Index_Buffer     => 0,
                          Pos_VB           => 1,
                          Normal_VB        => 2,
                          Tex_Coord_VB     => 3,
                          WVP_Matrix_VB    => 4,
                          World_Matrix_VB  => 5,
                          Invalid_Material => 16#FFFFFFFF#);

   type Mesh_Entry is record
        VAO            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        VBO            : GL.Objects.Buffers.Buffer;
        IBO            : GL.Objects.Buffers.Buffer;
        Num_Indices    : UInt := 0;
        Material_Index : Material_Type := Invalid_Material;
   end record;

   package Mesh_Entry_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, Mesh_Entry);
   type Mesh_Entry_Map is new Mesh_Entry_Package.Map with
     null Record;

   type Mesh_22 is record
      Entries     : Mesh_Entry_Package.Map;
      Textures    : Ogldev_Texture.Mesh_Texture_Package.Map;
   end record;

end Mesh_22;
