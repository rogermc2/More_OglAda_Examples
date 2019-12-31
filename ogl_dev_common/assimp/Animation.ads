
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;

with GL.Types; use GL.Types;

with Assimp_Types;
with API_Vectors_Matrices;
with Morph;

package Animation is

   type AI_Animation_Map is private;
   type AI_Channels_List is private;

   type AI_Anim_Behaviour is (Anim_Behaviour_Default, Anim_Behaviour_Constant,
                              Anim_Behaviour_Linear, Anim_Behaviour_Repeat,
                              Anim_Behaviour_Force32Bit);
   pragma Convention (C, AI_Anim_Behaviour);

   for AI_Anim_Behaviour use (Anim_Behaviour_Default    => 0,
                              Anim_Behaviour_Constant   => 1,
                              Anim_Behaviour_Linear     => 2,
                              Anim_Behaviour_Repeat     => 3,
                              Anim_Behaviour_Force32Bit => Integer'Last);

    type API_Quat_Key is record
      Time    : Interfaces.C.double;
      Value   : API_Vectors_Matrices.API_Quaternion;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Quat_Key);

   type API_Quat_Key_Array is array (Interfaces.C.unsigned range <>)
     of aliased API_Quat_Key;
   pragma Convention (C, API_Quat_Key_Array);

   package Quat_Key_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Quat_Key, API_Quat_Key_Array,
      API_Quat_Key'(others => <>));

   type API_Vector_Key is record
      Time    : Interfaces.C.double := 0.0;
      Value   : API_Vectors_Matrices.API_Vector_3D;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Vector_Key);

   type API_Vector_Key_Array is array (Interfaces.C.unsigned range <>)
     of aliased API_Vector_Key;
   pragma Convention (C, API_Vector_Key_Array);

   package Vector_Key_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Vector_Key, API_Vector_Key_Array,
      API_Vector_Key'(others => <>));

   type API_Mesh_Key is record
      Time    : Interfaces.C.double;
      Value   : unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Mesh_Key);

   type API_Mesh_Key_Array is array (Interfaces.C.unsigned range <>)
     of aliased API_Mesh_Key;
   pragma Convention (C, API_Mesh_Key_Array);

   package Mesh_Key_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Mesh_Key, API_Mesh_Key_Array,
      API_Mesh_Key'(others => <>));

   type API_Mesh_Anim is record
      Name      : Assimp_Types.API_String;
      Num_Keys  : unsigned := 0;
      Keys      : Mesh_Key_Pointers.Pointer;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Mesh_Anim);

   type API_Mesh_Anim_Array is array (Interfaces.C.unsigned range <>)
     of aliased API_Mesh_Anim;
   pragma Convention (C, API_Mesh_Anim_Array);

   package Mesh_Anim_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Mesh_Anim, API_Mesh_Anim_Array,
      API_Mesh_Anim'(others => <>));

   type API_Mesh_Morph_Anim is record
               Name      : Assimp_Types.API_String;
               Num_Keys  : unsigned;
               Keys      : Morph.Mesh_Morph_Key_Pointers.Pointer;
            end record;
            pragma Convention (C_Pass_By_Copy, API_Mesh_Morph_Anim);

     type API_Mesh_Morph_Anim_Array is array (Interfaces.C.unsigned range <>)
     of aliased API_Mesh_Morph_Anim;
   pragma Convention (C, API_Mesh_Morph_Anim_Array);

   package Mesh_Morph_Anim_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Mesh_Morph_Anim, API_Mesh_Morph_Anim_Array,
      API_Mesh_Morph_Anim'(others => <>));

   type API_Node_Anim is record
      Name              : Assimp_Types.API_String;
      Num_Position_Keys : unsigned := 0;
      Position_Keys     : Vector_Key_Pointers.Pointer;
      Num_Rotation_Keys : unsigned := 0;
      Rotation_Keys     : Quat_Key_Pointers.Pointer;
      Num_Scaling_Keys  : unsigned := 0;
      Scaling_Keys      : Vector_Key_Pointers.Pointer;
      Pre_State         : AI_Anim_Behaviour;
      Post_State        : AI_Anim_Behaviour;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Node_Anim);

   type API_Node_Anim_Array is array (Interfaces.C.unsigned range <>)
     of aliased API_Node_Anim;
   pragma Convention (C, API_Node_Anim_Array);

   package Node_Anim_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Node_Anim, API_Node_Anim_Array,
      API_Node_Anim'(others => <>));

   type API_Animation is record
      Name                      : Assimp_Types.API_String;
      Duration                  : Interfaces.C.double := -1.0;
      Ticks_Per_Second          : Interfaces.C.double := 0.0;
      Num_Channels              : unsigned := 0;
      Channels                  : Node_Anim_Pointers.Pointer;
      Num_Mesh_Channels         : unsigned := 0;
      Mesh_Channels             : Mesh_Anim_Pointers.Pointer;
      Num_Morphed_Mesh_Channels : unsigned := 0;
      Morphed_Mesh_Channels     : Mesh_Morph_Anim_Pointers.Pointer;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Animation);

   type API_Animation_Ptr is access API_Animation;
   pragma Convention (C, API_Animation_Ptr);

   type API_Animation_Ptr_Array is array (Interfaces.C.unsigned range <>)
     of aliased API_Animation_Ptr;
   pragma Convention (C, API_Animation_Ptr_Array);

   package Animation_Array_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Animation_Ptr, API_Animation_Ptr_Array, null);
   type Animation_Ptr_Array_Pointer is new Animation_Array_Pointers.Pointer;

   function To_AI_Animation_Map (Num_Animations : Interfaces.C.unsigned := 0;
                                 C_Ptr_Array_Ptr : Animation_Ptr_Array_Pointer)
                                  return AI_Animation_Map;
private

   type AI_Mesh_Key is record
      Time    : Single := 0.0;
      Value   : UInt := 0;
   end record;

   package AI_Mesh_Key_Package is new
     Ada.Containers.Doubly_Linked_Lists (AI_Mesh_Key);
   type AI_Mesh_Key_List is new AI_Mesh_Key_Package.List with null Record;

   type AI_Mesh_Anim is record
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Keys      : AI_Mesh_Key_List;
   end record;

   package AI_Mesh_Anim_Package is new
     Ada.Containers.Doubly_Linked_Lists (AI_Mesh_Anim);
   type AI_Mesh_Anim_List is new AI_Mesh_Anim_Package.List with null Record;

   type AI_Quat_Key is record
      Time    : Single := 0.0;
      Value   : GL.Types.Singles.Vector4;
   end record;

   package AI_Quat_Key_Package is new
     Ada.Containers.Doubly_Linked_Lists (AI_Quat_Key);
   type AI_Quat_Key_List is new AI_Quat_Key_Package.List with null Record;

   type AI_Vector_Key is record
      Time    : Single := 0.0;
      Value   : GL.Types.Singles.Vector3;
   end record;

   package AI_Vector_Key_Package is new
     Ada.Containers.Doubly_Linked_Lists (AI_Vector_Key);
   type AI_Vector_Key_List is new AI_Vector_Key_Package.List with null Record;

  type AI_Node_Anim is record
      Name              : Ada.Strings.Unbounded.Unbounded_String;
      Position_Keys     : AI_Vector_Key_List;
      Rotation_Keys     : AI_Quat_Key_List;
      Scaling_Keys      : AI_Vector_Key_List;
      Pre_State         : AI_Anim_Behaviour;
      Post_State        : AI_Anim_Behaviour;
   end record;

   package AI_Channels_Package is new
     Ada.Containers.Doubly_Linked_Lists (AI_Node_Anim);
   type AI_Channels_List is new AI_Channels_Package.List with null Record;

   type AI_Animation is record
      Name                : Ada.Strings.Unbounded.Unbounded_String;
      Duration            : GL.Types.Single;
      Ticks_Per_Second    : GL.Types.Single;
      Channels            : AI_Channels_List;
      Mesh_Channels       : AI_Mesh_Anim_List;
      Morph_Mesh_Channels : Morph.AI_Mesh_Morph_List;
   end record;

   package AI_Animation_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, AI_Animation);
   type AI_Animation_Map is new AI_Animation_Package.Map with null Record;

end Animation;
