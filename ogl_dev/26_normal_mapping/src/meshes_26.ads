
with Ada.Containers.Indefinite_Ordered_Maps;

with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types;

with Assimp_Types;
with Ogldev_Basic_Mesh;
with Ogldev_Texture;

package Meshes_26 is

   type Mesh_26 is private;

   procedure Load_Mesh (theMesh : in out Mesh_26; File_Name : String);
   procedure Render (theMesh : Mesh_26);

private

   Invalid_Material : constant GL.Types.UInt := 16#FFFFFFFF#;

   type Mesh_Entry is record
      Vertex_Buffer  : GL.Objects.Buffers.Buffer;
      Index_Buffer   : GL.Objects.Buffers.Buffer;
      Num_Indices    : GL.Types.UInt := 0;
      Material_Index : GL.Types.UInt := Invalid_Material;
   end record;

   package Mesh_Entry_Package is new
    Ada.Containers.Indefinite_Ordered_Maps (Natural, Mesh_Entry);
   type Mesh_Entry_Map is new Mesh_Entry_Package.Map with
     null Record;

   type Mesh_26 is record
      Entries    : Mesh_Entry_Package.Map;
      Textures   : Ogldev_Texture.Mesh_Texture_Map;
   end record;

end Meshes_26;
