
with Ada.Containers.Indefinite_Ordered_Maps;

with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;

with Ogldev_Texture;

Package Ogldev_Basic_Mesh is

   type Material_Type is (Index_Buffer, Pos_VB, Normal_VB,
                          Tex_Coord_VB, WVP_Matrix_VB,
                          World_Matrix_VB, Invalid_Material);
   pragma Convention (C, Material_Type);

   type Basic_Mesh is private;
   type Basic_Mesh_Entry is private;

   function Get_Base_Vertex (theEntry : Basic_Mesh_Entry) return UInt;
   function Get_Base_Index (theEntry : Basic_Mesh_Entry) return UInt;
   function Get_Material_Index (theEntry : Basic_Mesh_Entry) return Material_Type;
   function Get_Num_Indices (theEntry : Basic_Mesh_Entry) return UInt;
   function Get_Orientation  (theMesh : Basic_Mesh) return Orientation;
--     procedure Load_Mesh (theMesh : in out Basic_Mesh; File_Name : String);
   procedure Render (theMesh : Basic_Mesh);
   procedure Render (theMesh : Basic_Mesh;
                     WVP_Matrix, World_Matrix : Singles.Matrix4_Array);
   procedure Set_Base_Index (theEntry : in out Basic_Mesh_Entry; Base_Index : UInt);
   procedure Set_Base_Vertex (theEntry : in out Basic_Mesh_Entry; Base_Vertex : UInt);
   procedure Set_Entry (theEntry : in out Basic_Mesh_Entry;
                        Base_Index, Base_Vertex, Num_Indices : UInt;
                        Material : Material_Type);
   procedure Set_Material_Type (theEntry : in out Basic_Mesh_Entry;
                                Material : Material_Type);
   procedure Set_Num_Indices (theEntry : in out Basic_Mesh_Entry; Num : UInt);

private

   Num_Buffers : constant UInt := 6;

   for Material_Type use (Index_Buffer     => 0,
                          Pos_VB           => 1,
                          Normal_VB        => 2,
                          Tex_Coord_VB     => 3,
                          WVP_Matrix_VB    => 4,
                          World_Matrix_VB  => 5,
                          Invalid_Material => 16#FFFFFFFF#);

   type Basic_Mesh_Entry is record
      Num_Indices    : UInt := 0;
      Base_Vertex    : UInt := 0;
      Base_Index     : UInt := 0;
      Material_Index : Material_Type := Invalid_Material;
   end record;

   type Mesh_Buffer_Array is array (UInt range 1 .. Num_Buffers) of
     GL.Objects.Buffers.Buffer;

   package Mesh_Entry_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, Basic_Mesh_Entry);
   type Mesh_Entry_Map is new Mesh_Entry_Package.Map with
     null Record;

   type Basic_Mesh is record
      VAO            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Buffers        : Mesh_Buffer_Array;
      Basic_Entry    : Basic_Mesh_Entry;
      Entries        : Mesh_Entry_Package.Map;
      Textures       : Ogldev_Texture.Mesh_Texture_Package.Map;
      Direction      : Orientation;
   end record;

end Ogldev_Basic_Mesh;
