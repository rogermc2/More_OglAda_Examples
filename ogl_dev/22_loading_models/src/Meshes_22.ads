
with Ada.Containers.Indefinite_Ordered_Maps;

with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;

with Ogldev_Texture;

Package Meshes_22 is

   type Mesh_22 is private;
   type Mesh_Entry is private;

   procedure Load_Mesh (theMesh : in out Mesh_22; File_Name : String);
   function Mesh_Entries_Size (aMesh : Mesh_22) return UInt;
   procedure Render_Mesh (theMesh : Mesh_22);

private
   Num_Buffers : constant UInt := 6;

   Invalid_Material : constant UInt := 16#FFFFFFFF#;

   type Mesh_Entry is record
        VAO            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
        VBO            : GL.Objects.Buffers.Buffer;
        IBO            : GL.Objects.Buffers.Buffer;
        Num_Indices    : UInt := 0;
        Material_Index : UInt := Invalid_Material;
   end record;

   package Mesh_Entry_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, Mesh_Entry);
   type Mesh_Entry_Map is new Mesh_Entry_Package.Map with
     null Record;

   type Mesh_22 is record
      Entries     : Mesh_Entry_Package.Map;
      Textures    : Ogldev_Texture.Mesh_Texture_Package.Map;
   end record;

end Meshes_22;
