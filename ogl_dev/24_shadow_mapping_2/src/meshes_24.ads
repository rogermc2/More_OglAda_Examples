
with Ada.Containers.Doubly_Linked_Lists;

with GL.Objects.Buffers;
with GL.Types;

with Ogldev_Texture;

package Meshes_24 is

   type Mesh_24 is private;

   Mesh_24_Error : Exception;

   procedure Load_Mesh (theMesh : in out Mesh_24; File_Name : String);
   procedure Render (theMesh : Mesh_24);

private

   Invalid_Material : constant GL.Types.UInt := 16#FFFFFFFF#;

   type Mesh_Entry is record
      Vertex_Buffer  : GL.Objects.Buffers.Buffer;
      Index_Buffer   : GL.Objects.Buffers.Buffer;
      Num_Indices    : GL.Types.UInt := 0;
      Material_Index : GL.Types.UInt := Invalid_Material;
   end record;

   package Mesh_Entry_Package is new
    Ada.Containers.Doubly_Linked_Lists (Mesh_Entry);
   type Mesh_Entry_List is new Mesh_Entry_Package.List with
     null Record;

   type Mesh_24 is record
      Entries    : Mesh_Entry_List;
      Textures   : Ogldev_Texture.Mesh_Texture_Map;
   end record;

end Meshes_24;
