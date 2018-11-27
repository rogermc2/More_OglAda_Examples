
with Interfaces.C;
with Interfaces.C.Pointers;

with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Ordered_Maps;

with GL.Objects.Buffers;
with GL.Types; use GL.Types;

with Assimp_Texture;
with Assimp_Types;
with API_Vectors_Matrices; use API_Vectors_Matrices;

package Assimp_Mesh is

    AI_Max_Face_Indices   : constant Int := 16#7FFF#;
    AI_Max_Bone_Weights   : constant Int := 16#7FFFFFFF#;
    AI_Max_Vertices       : constant Int := 16#7FFFFFFF#;
    AI_Max_Faces          : constant Int := 16#7FFFFFFF#;
    AI_Max_Texture_Coords : constant Int := 8;
    AI_Max_Colour_Sets    : constant Int := 8;

    type Mesh_Entry is record
        Vertex_Buffer  : GL.Objects.Buffers.Buffer;
        Index_Buffer   : GL.Objects.Buffers.Buffer;
        Num_Indices    : UInt;
        Material_Index : UInt;
    end record;

   package Entries_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, Mesh_Entry);
   subtype Entries_Map is Entries_Package.Map;

    type AI_Primitive_Type is
        (AI_Primitive_Type_Point, AI_Primitive_Type_Line, AI_Primitive_Type_Triangle,
        AI_Primitive_Type_Polygon, AI_Primitive_Type_Force32Bit);
   pragma Convention (C, AI_Primitive_Type);

    type AI_Vertex_Weight is record
        Vertex_ID  : UInt;
        Weight     : Single;
    end record;

    type AI_Colour_4D is record
            R, G, B, A : GL.Types.Single;
   end record;

   package Vertex_Weight_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, AI_Vertex_Weight);
   type Vertex_Weight_Map is new Vertex_Weight_Package.Map with
     null Record;

    type AI_Bone is record
        Name          : Ada.Strings.Unbounded.Unbounded_String :=
                         Ada.Strings.Unbounded.To_Unbounded_String ("");
        Weights       : Vertex_Weight_Map;
        Offset_Matrix : GL.Types.Singles.Matrix4;
    end record;

   package Bones_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, AI_Bone);
   type Bones_Map is new  Bones_Package.Map with
     null Record;

   package Indices_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, UInt);
   type Indices_Map is new Indices_Package.Map with
     null Record;

    type AI_Face is record
        Indices : Indices_Map;
    end record;

   package Faces_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, AI_Face);
   type Faces_Map is new Faces_Package.Map with null Record;

   use Singles;
   package Vertices_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, Singles.Vector3);
   type Vertices_Map is new  Vertices_Package.Map with null Record;

   package Colours_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, Singles.Vector4);
   type Colours_Map is new  Colours_Package.Map with null Record;

   package Colour_Coords_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, Colours_Map);
   type Colour_Coords_Map is new  Colour_Coords_Package.Map with null Record;

   package Texture_Coords_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, Vertices_Map);
   type Texture_Coords_Map is new  Texture_Coords_Package.Map with null Record;

    type AI_Mesh is record
        Name              : Ada.Strings.Unbounded.Unbounded_String :=
                              Ada.Strings.Unbounded.To_Unbounded_String ("");
        Vertices          : Vertices_Map;
        Normals           : Vertices_Map;
        Tangents          : Vertices_Map;
        Bit_Tangents      : Vertices_Map;
        Colours           : Colour_Coords_Map;
        Texture_Coords    : Texture_Coords_Map;
        Num_UV_Components : UInt_Array (1 .. API_Max_Texture_Coords);
        Faces             : Faces_Map;
        Bones             : Bones_Map;
        Material_Index    : UInt := 0;
    end record;

   package AI_Mesh_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, AI_Mesh);
   subtype  AI_Mesh_Map is AI_Mesh_Package.Map;

    type Mesh is record
        Entries   : Entries_Map;
        Textures  : Assimp_Texture.AI_Texture_Map;
    end record;

   procedure Load_Mesh (File_Name : String; theMesh : in out Mesh);
   procedure Render_Mesh (theMesh : Mesh);

private
   for AI_Primitive_Type use
        (AI_Primitive_Type_Point       => 1,
         AI_Primitive_Type_Line        => 2,
         AI_Primitive_Type_Triangle    => 4,
         AI_Primitive_Type_Polygon     => 8,
         AI_Primitive_Type_Force32Bit  => Integer'Last);

end Assimp_Mesh;
