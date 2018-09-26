
with Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;

with GL.Types; use GL.Types;

with Assimp_Types;
with API_Vectors_Matrices;

package Material is

   type AI_Property_Type_Info is (PTI_Float, PTI_Double, PTI_String,
                                  PTI_Integer, PTI_Buffer, PTI_Force32Bit);
   pragma Convention (C, AI_Property_Type_Info);

   type AI_Texture_Map_Mode is (AI_Texture_Map_Mode_Wrap,
                                AI_Texture_Map_Mode_Clamp,
                                AI_Texture_Map_Mode_Decal,
                                AI_Texture_Map_Mode_Mirror,
                                AI_Texture_Map_Mode_Force32Bit);
   pragma Convention (C, AI_Texture_Map_Mode);

   type AI_Texture_Mapping is (AI_Texture_Mapping_UV, AI_Texture_Mapping_Sphere,
                               AI_Texture_Mapping_Cylinder,
                               AI_Texture_Mapping_Box, AI_Texture_Mapping_Plane,
                               AI_Texture_Mapping_Other);
   pragma Convention (C, AI_Texture_Mapping);

   type AI_Texture_Op is (AI_Texture_Op_Multiply, AI_Texture_Op_Add,
                          AI_Texture_Op_Subtract, AI_Texture_Op_Divide,
                          AI_Texture_Op_Smooth_Add, AI_Texture_Op_Signed_Add);
   pragma Convention (C, AI_Texture_Op);

   type AI_Texture_Type is (AI_Texture_Type_None, AI_Texture_Diffuse,
                            AI_Texture_Specular, AI_Texture_Ambient,
                            AI_Texture_Emissive, AI_Texture_Height,
                            AI_Texture_Normals, AI_Texture_Shininess,
                            AI_Texture_Opacity, AI_Texture_Displacement,
                            AI_Texture_Light_Map, AI_Texture_Reflection,
                            AI_Texture_Unknown);
   pragma Convention (C, AI_Texture_Type);

   for AI_Property_Type_Info use (PTI_Float      => 16#1#,
                                  PTI_Double     => 16#2#,
                                  PTI_String     => 16#3#,
                                  PTI_Integer    => 16#4#,
                                  PTI_Buffer     => 16#5#,
                                  PTI_Force32Bit => 16#9fffffff#);

   type AI_Material_Property is record
      Key            : Ada.Strings.Unbounded.Unbounded_String;  --  Property name
      Semantic       : GL.Types.UInt := 0;  --  Usage, 0 for non_texture properties
      Texture_Index  : GL.Types.UInt := 0;  --  Index for textures
      Data_Type      : AI_Property_Type_Info := PTI_Float;
      --        case Data_Type is
      --           when PTI_Float =>
      --              Data_Single    : Single;
      --           when PTI_Double =>
      --              Data_Double    : Double;
      --           when PTI_String =>
      --              Data_String    : Ada.Strings.Unbounded.Unbounded_String;
      --           when PTI_Integer =>
      --              Data_Integer    : GL.Types.Int;
      --           when PTI_Buffer =>
      --              Data_Buffer    : Assimp_Types.Byte_Data_List;
      --           when PTI_Force32Bit  =>
      --              Data_Force32Bit    : GL.Types.Int;
      --        end case;
      Data_Buffer    : Assimp_Types.Byte_Data_List;
   end record;

   package AI_Material_Property_Package is new
     Ada.Containers.Doubly_Linked_Lists (AI_Material_Property);
   type AI_Material_Property_List is new AI_Material_Property_Package.List with null Record;

   type AI_Material is record
      Properties     : AI_Material_Property_List;
      Num_Allocated  : GL.Types.UInt := 0;
   end record;

   package AI_Material_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (GL.Types.UInt, AI_Material);
   type AI_Material_Map is new AI_Material_Package.Map with null Record;

   type Material_Property is record
      Key           : Assimp_Types.API_String;
      Semantic      : Interfaces.C.unsigned := 0;
      Texture_Index : Interfaces.C.unsigned := 0;
      Data_Length   : Interfaces.C.unsigned := 0;  --  Actual must not be 0
      --  Data_Type provides information for the property.
      --  It defines the data layout inside the data buffer.
      --  This is used by the library internally to perform debug checks and to
      --  utilize proper type conversions.
      Data_Type     : AI_Property_Type_Info := PTI_Float;
      --  Data holds the property's value. Its size is always Data_Length
      --        Data_Ptr      : Byte_Array_Pointer := null;
      Data_Ptr      : Interfaces.C.Strings.chars_ptr :=
                        Interfaces.C.Strings.Null_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Material_Property);

   type Material_Property_Array is array (Interfaces.C.unsigned range <>) of
     aliased Material_Property;
   pragma Convention (C, Material_Property_Array);

   --  Material data is stored using a key-value structure.
   --  A single key-value pair is called a 'material property'.

   type API_Material_Property is record
      Key           : Assimp_Types.API_String;  --  One of the AI_MATKEY_XXX constants
      --  http://assimp.sourceforge.net/lib_html/material_8h.html
      Semantic      : Interfaces.C.unsigned := 0;
      Texture_Index : Interfaces.C.unsigned := 0;  --  Texture index
      Data_Length   : Interfaces.C.unsigned := 0;  --  Actual must not be 0
      --  Data_Type provides information for the property.
      --  It defines the data layout inside the data buffer.
      --  This is used by the library internally to perform debug checks and to
      --  utilize proper type conversions.
      Data_Type     : AI_Property_Type_Info := PTI_Float;
      --  Data holds the property's value. Its size is always Data_Length
      Data_Ptr      : Assimp_Types.Data_Pointer := null;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Material_Property);

   type API_Property_Array is array (Interfaces.C.unsigned range <>)
     of aliased API_Material_Property;
   pragma Convention (C, API_Property_Array);

   Material_Property_Default : API_Material_Property :=
                                 ((0, (others => Interfaces.C.char'Val (0))),
                                  0, 0, 0, PTI_Float, Null);

   package API_Property_Array_Package is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Material_Property, API_Property_Array,
      Material_Property_Default);
   subtype API_Material_Property_Ptr is API_Property_Array_Package.Pointer;

    type API_Property_Array_Ptr is access API_Material_Property;
   pragma Convention (C, API_Property_Array_Ptr);

   type API_Property_Ptr_Array is array (Interfaces.C.unsigned range <>)
     of aliased API_Property_Array_Ptr;
   pragma Convention (C, API_Property_Ptr_Array);

   package Property_Ptr_Array_Package is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Property_Array_Ptr, API_Property_Ptr_Array,
      null);
   subtype Property_Ptr_Array_Pointer is Property_Ptr_Array_Package.Pointer;

   type API_Material is record
      Properties     : Property_Ptr_Array_Pointer := null;
--        Properties     : access API_Material_Property_Ptr := null;
      Num_Properties : Interfaces.C.unsigned := 0;
      Num_Allocated  : Interfaces.C.unsigned := 0;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Material);

   type API_Material_Array is array (Interfaces.C.unsigned range <>)
     of aliased API_Material;
   pragma Convention (C, API_Material_Array);

   package Material_Pointers_Package is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Material, API_Material_Array, API_Material'(others => <>));
   subtype Material_Array_Pointer is Material_Pointers_Package.Pointer;

   type API_UV_Transform is record
      Translation   : API_Vectors_Matrices.API_Vector_2D;
      Scaling       : API_Vectors_Matrices.API_Vector_2D;
      Rotation      : Interfaces.C.C_float := 0.0;
   end record;
   pragma Convention (C_Pass_By_Copy, API_UV_Transform);

   Material_Exception : Exception;

--     procedure Get_Texture (aMaterial : AI_Material; Tex_Type : AI_Texture_Type;
--                            Tex_Index : UInt := 0;
--                            Path      : out Ada.Strings.Unbounded.Unbounded_String);
   --     procedure Get_Texture (aMaterial : AI_Material; Tex_Type : AI_Texture_Type;
   --                            Tex_Index : UInt := 0;
   --                            Path : out Ada.Strings.Unbounded.Unbounded_String;
   --                            Mapping : AI_Texture_Mapping;
   --                            UV_Index : out UInt;
   --                            Blend : out GL.Types.Single;
   --                            Op : AI_Texture_Op;
   --                            Map_Mode : AI_Texture_Map_Mode;
   --                            Result : out Assimp_Types.API_Return);
   function Get_Texture_Count (aMaterial : AI_Material;
                               Tex_Type  : AI_Texture_Type)
                               return GL.Types.UInt;
private

   for AI_Texture_Map_Mode use (AI_Texture_Map_Mode_Wrap       => 0,
                                AI_Texture_Map_Mode_Clamp      => 1,
                                AI_Texture_Map_Mode_Decal      => 2,
                                AI_Texture_Map_Mode_Mirror     => 3,
                                AI_Texture_Map_Mode_Force32Bit => 16#9FFFFFFF#);

   for AI_Texture_Mapping use (AI_Texture_Mapping_UV       => 0,
                               AI_Texture_Mapping_Sphere   => 1,
                               AI_Texture_Mapping_Cylinder => 2,
                               AI_Texture_Mapping_Box      => 3,
                               AI_Texture_Mapping_Plane    => 4,
                               AI_Texture_Mapping_Other    => 5);

   for AI_Texture_Op use (AI_Texture_Op_Multiply   => 0,
                          AI_Texture_Op_Add        => 1,
                          AI_Texture_Op_Subtract   => 2,
                          AI_Texture_Op_Divide     => 3,
                          AI_Texture_Op_Smooth_Add => 4,
                          AI_Texture_Op_Signed_Add => 5);

   for AI_Texture_Type use (AI_Texture_Type_None     => 0,
                            AI_Texture_Diffuse       => 1,
                            AI_Texture_Specular      => 2,
                            AI_Texture_Ambient       => 3,
                            AI_Texture_Emissive      => 4,
                            AI_Texture_Height        => 5,
                            AI_Texture_Normals       => 6,
                            AI_Texture_Shininess     => 7,
                            AI_Texture_Opacity       => 8,
                            AI_Texture_Displacement  => 9,
                            AI_Texture_Light_Map     => 16#A#,
                            AI_Texture_Reflection    => 16#B#,
                            AI_Texture_Unknown       => 16#c#);
end Material;
