
WITH System;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;

with Assimp_Types;
with API_Vectors;

package Animation_AS is

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
      Value   : API_Vectors.API_Quaternion;
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
      Value   : API_Vectors.API_Vector_3D;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Vector_Key);

   type API_Vector_Key_Array is array (Interfaces.C.unsigned range <>)
     of aliased API_Vector_Key;
   pragma Convention (C, API_Vector_Key_Array);

   package Vector_Key_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Vector_Key, API_Vector_Key_Array,
      API_Vector_Key'(others => <>));

   type AS_Mesh_Anim is record
      Name      : Assimp_Types.AI_String;
      Num_Keys  : unsigned := 0;
      Keys      : System.Address;
   end record;
   pragma Convention (C_Pass_By_Copy, AS_Mesh_Anim);

   type AS_Mesh_Anim_Array is array (Interfaces.C.unsigned range <>)
     of aliased AS_Mesh_Anim;
   pragma Convention (C, AS_Mesh_Anim_Array);

   package AS_Mesh_Anim_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, AS_Mesh_Anim, AS_Mesh_Anim_Array,
      AS_Mesh_Anim'(others => <>));

   type API_Node_Anim is record
      Name              : Assimp_Types.AI_String;
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

   type AS_Animation is record
      Name                      : Assimp_Types.AI_String;
      Duration                  : Interfaces.C.double := -1.0;
      Ticks_Per_Second          : Interfaces.C.double := 0.0;
      Num_Channels              : unsigned := 0;
      Channels                  : Node_Anim_Pointers.Pointer;
      Num_Mesh_Channels         : unsigned := 0;
      Mesh_Channels             : System.Address;
      Num_Morphed_Mesh_Channels : unsigned := 0;
      Morphed_Mesh_Channels     : System.Address;
   end record;
   pragma Convention (C_Pass_By_Copy, AS_Animation);

   type AS_Animation_Array is array (Interfaces.C.unsigned range <>)
     of aliased AS_Animation;
   pragma Convention (C, AS_Animation_Array);

   package AS_Animation_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, AS_Animation, AS_Animation_Array,
      AS_Animation'(others => <>));
   type AS_Animation_Ptr is new AS_Animation_Pointers.Pointer;

end Animation_AS;
