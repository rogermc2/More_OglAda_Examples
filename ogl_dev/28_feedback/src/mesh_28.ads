
--  with Ada.Containers.Doubly_Linked_Lists;
--  with Ada.Containers.Indefinite_Ordered_Maps;

with GL.Objects.Buffers;
--  with GL.Objects.Textures;
with GL.Types;

with Assimp_Types;
with Ogldev_Basic_Mesh;

package Mesh_28 is

   type Mesh_28 is private;

   procedure Load_Mesh (theMesh : in out Mesh_28; File_Name : String);
   procedure  Render (theMesh : Mesh_28);

private

   type Mesh_Entry is record
      Vertex_Buffer  : GL.Objects.Buffers.Buffer;
      Index_Buffer   : GL.Objects.Buffers.Buffer;
      Num_Indices    : GL.Types.UInt := 0;
      Material_Index : Ogldev_Basic_Mesh.Material_Type :=
                            Ogldev_Basic_Mesh.Invalid_Material;
   end record;
   type Mesh_Entry_Array is array (Assimp_Types.Index_Int range <>) of Mesh_Entry;

--     package Mesh_Entry_Package is new
--      Ada.Containers.Indefinite_Ordered_Maps (Natural, Mesh_Entry);
--     type Mesh_Entry_Map is new Mesh_Entry_Package.Map with
--       null Record;
--
--     type Mesh_Texture_Ptr is access GL.Objects.Textures.Texture;
--     package Mesh_Texture_Package is new
--       Ada.Containers.Doubly_Linked_Lists (Mesh_Texture_Ptr);
--     type Mesh_Texture_List is new Mesh_Texture_Package.List with
--       null Record;

   type Mesh_28 (Num_Entries : Assimp_Types.Index_Int := 0) is record
      M_Entry    : Mesh_Entry;
      Entries    : Mesh_Entry_Array (1 .. Num_Entries);
      Textures   : Assimp_Types.Texture_Coords_Array;
   end record;

   type Mesh_Array is array (Assimp_Types.Index_Int range <>) of Mesh_28;

end Mesh_28;
