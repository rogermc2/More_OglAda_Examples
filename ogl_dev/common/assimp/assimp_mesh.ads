
with Interfaces.C;
with Interfaces.C.Pointers;

with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Ordered_Maps;

with GL.Objects.Buffers;
with GL.Types; use GL.Types;

with Assimp_Colour;
with Assimp_Texture;
with Assimp_Types;
with API_Vectors_Matrices; use API_Vectors_Matrices;

package Assimp_Mesh is

    AI_Max_Face_Indices   : constant Int := 16#7FFF#;
    AI_Max_Bone_Weights   : constant Int := 16#7FFFFFFF#;
    AI_Max_Vertices       : constant Int := 16#7FFFFFFF#;
    AI_Max_Faces          : constant Int := 16#7FFFFFFF#;

    type Entry_Ptr is private;

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

   package Vertex_Weight_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, AI_Vertex_Weight);
   type Vertex_Weight_Map is new Vertex_Weight_Package.Map with
     null Record;

    type AI_Bone is record
        Name         : Ada.Strings.Unbounded.Unbounded_String :=
                         Ada.Strings.Unbounded.To_Unbounded_String ("");
        Weights      : Vertex_Weight_Map;
        Offset_Matrix : API_Vectors_Matrices.API_Matrix_4D;
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

    type API_Face is record
        Num_Indices : Interfaces.C.unsigned;
        Indices     : Unsigned_Array_Pointer;
    end record;

   type API_Faces_Array is array
     (Interfaces.C.unsigned range <>) of aliased API_Face;
   pragma Convention (C, API_Faces_Array);

   package Faces_Array_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Face, API_Faces_Array,
      API_Face'(others => <>));
   subtype Faces_Array_Pointer is Faces_Array_Pointers.Pointer;

--     type Vertices is record
--          X : UInt;
--          Y : UInt;
--          Z : UInt;
--     end record;
--     type Vertices_Array is array (UInt range <>) of Vertices;

   package Faces_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, AI_Face);
   type Faces_Map is new Faces_Package.Map with null Record;

   use Singles;
   package Vertices_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, Singles.Vector3);
   type Vertices_Map is new  Vertices_Package.Map with null Record;

    type Colour_Array is array (1 .. AI_Max_Colour_Sets) of Assimp_Colour.AI_Colour_4D;
   type Texture_Coords_Array is new
     Singles.Vector2_Array (1 .. AI_Max_Texture_Coords);
    type AI_Mesh is record
        Name         : Ada.Strings.Unbounded.Unbounded_String :=
                         Ada.Strings.Unbounded.To_Unbounded_String ("");
        Vertices          : Vertices_Map;
        Normals           : Vertices_Map;
        Tangents          : Vertices_Map;
        Bit_Tangents      : Vertices_Map;
        Colours           : Colour_Array := (others => (0.0, 0.0, 0.0,  0.0));
        Texture_Coords    : Texture_Coords_Array := (others => (0.0, 0.0, 0.0));
        Num_UV_Components : UInt;
        Faces             : Faces_Map;
        Bones             : Bones_Map;
        Material_Index    : UInt;
    end record;

   package AI_Mesh_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, AI_Mesh);
   subtype  AI_Mesh_Map is AI_Mesh_Package.Map;

    --  C arrays are constrained. To pass C arrays around function calls,
    --  either terminate them with a zero element or with a separate length parameter.

    type API_Mesh is record
        Primitive_Types   : Interfaces.C.unsigned := 0;
        Num_Vertices      : Interfaces.C.unsigned := 0;
        Num_Faces         : Interfaces.C.unsigned := 0;
        Vertices          : Vector_3D_Array_Pointer;
        Normals           : Vector_3D_Array_Pointer;
        Tangents          : Vector_3D_Array_Pointer;
        Bit_Tangents      : Vector_3D_Array_Pointer;
        Colours           : API_Colours_4D_Array (1 .. AI_Max_Colour_Sets);
        Texture_Coords    : API_Vector_3D_Array (1 .. AI_Max_Texture_Coords);
        Num_UV_Components : Interfaces.C.unsigned := 0;
        Faces             : Vector_3D_Array_Pointer;
        Num_Bones         : Interfaces.C.unsigned := 0;
        Bones             : Vector_3D_Array_Pointer;
        Material_Index    : Interfaces.C.unsigned := 0;
        Name              : Assimp_Types.AI_String;
        Num_Anim_Meshes   : Interfaces.C.unsigned := 0;
        Anim_Meshes       : Vector_3D_Array_Pointer;
    end record;
    pragma Convention (C_Pass_By_Copy, API_Mesh);

   type API_Mesh_Array is array
     (Interfaces.C.unsigned range <>) of aliased API_Mesh;
   pragma Convention (C, API_Mesh_Array);

   package Mesh_Array_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Mesh, API_Mesh_Array,
      API_Mesh'(others => <>));
   subtype Mesh_Array_Pointer is Mesh_Array_Pointers.Pointer;

    type Mesh is record
        Entries   : Entries_Map;
        Textures  : Assimp_Texture.AI_Texture_Map;
    end record;

--     type API_Mesh is record
--          Entries   : Entry_Ptr;
--          Textures  : Assimp_Texture.Texture_Pointers.Pointer;
--     end record;
--     pragma Convention (C_Pass_By_Copy, API_Mesh);

   procedure Load_Mesh (File_Name : String; theMesh : in out Mesh);
   procedure Render_Mesh (theMesh : Mesh);

    private
         for AI_Primitive_Type use
        (AI_Primitive_Type_Point       => 1,
         AI_Primitive_Type_Line        => 2,
         AI_Primitive_Type_Triangle    => 4,
         AI_Primitive_Type_Polygon     => 8,
         AI_Primitive_Type_Force32Bit  => Integer'Last);

    type API_Mesh_Entry is record
        Vertex_Buffer  : GL.Objects.Buffers.Buffer;
        Index_Buffer   : GL.Objects.Buffers.Buffer;
        Num_Indices    : UInt;
        Material_Index : UInt;
    end record;
    pragma Convention (C_Pass_By_Copy, API_Mesh_Entry);

    type API_Entries_Array is array (Interfaces.C.unsigned range <>) of aliased API_Mesh_Entry;
    pragma Convention (C, API_Entries_Array);

    type Entry_Ptr is access API_Entries_Array;

end Assimp_Mesh;
