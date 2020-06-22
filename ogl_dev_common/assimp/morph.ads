
--  with System;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

with GL.Types; use GL.Types;

package Morph is
   type API_Morph_Value is new Interfaces.C.unsigned;
   type API_Morph_Weight is new Interfaces.C.double;

   type API_Morph_Values_Array is array
     (Interfaces.C.unsigned range <>) of aliased API_Morph_Value;
   pragma Convention (C, API_Morph_Values_Array);

   package Mesh_Morph_Value_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Morph_Value, API_Morph_Values_Array,
      API_Morph_Value'(0));

   type API_Morph_Weights_Array is array
     (Interfaces.C.unsigned range <>) of aliased API_Morph_Weight;
   pragma Convention (C, API_Morph_Weights_Array);

   package Mesh_Morph_Weight_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Morph_Weight, API_Morph_Weights_Array,
      API_Morph_Weight'(API_Morph_Weight'Last));

   type API_Mesh_Morph_Key is record
      Key_Time               : Interfaces.C.double := 0.0;
      Values                 : Mesh_Morph_Value_Pointers.Pointer;
      Weights                : Mesh_Morph_Weight_Pointers.Pointer;
      Num_Values_And_Weights : unsigned := 0;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Mesh_Morph_Key);

   type API_Mesh_Morph_Key_Array is array
     (Interfaces.C.unsigned range <>) of aliased API_Mesh_Morph_Key;
   pragma Convention (C, API_Mesh_Morph_Key_Array);

   package Mesh_Morph_Key_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, API_Mesh_Morph_Key, API_Mesh_Morph_Key_Array,
      API_Mesh_Morph_Key'(others => <>));

--     type SA_Mesh_Morph_Key is record
--        Key_Time               : Interfaces.C.double := 0.0;
--        Values                 : System.Address;
--        Weights                : System.Address;
--        Num_Values_And_Weights : unsigned := 0;
--     end record;
--     pragma Convention (C_Pass_By_Copy, SA_Mesh_Morph_Key);
--
--     type SA_Mesh_Morph_Key_Array is array
--       (Interfaces.C.unsigned range <>) of aliased SA_Mesh_Morph_Key;
--     pragma Convention (C, SA_Mesh_Morph_Key_Array);

   package Morph_Key_Values_Package is new
     Ada.Containers.Doubly_Linked_Lists (UInt);
   type AI_Mesh_Morph_Key_Values_List is new Morph_Key_Values_Package.List with null Record;

   package Morph_Key_Weights_Package is new
     Ada.Containers.Doubly_Linked_Lists (Single);
   type AI_Mesh_Morph_Key_Weights_List is new Morph_Key_Weights_Package.List with null Record;

   type AI_Mesh_Morph_Key is record
      Key_Time               : Single := 0.0;
      Values                 : AI_Mesh_Morph_Key_Values_List;
      Weights                : AI_Mesh_Morph_Key_Weights_List;
   end record;

   package Mesh_Morph_Key_Package is new
     Ada.Containers.Doubly_Linked_Lists (AI_Mesh_Morph_Key);
   type AI_Mesh_Morph_Key_List is new Mesh_Morph_Key_Package.List with null Record;

   type AI_Mesh_Morph_Anim is record
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Keys      : AI_Mesh_Morph_Key_List;
   end record;

   package Mesh_Morph_Anim_Package is new
     Ada.Containers.Doubly_Linked_Lists (AI_Mesh_Morph_Anim);
   type AI_Mesh_Morph_List is new Mesh_Morph_Anim_Package.List with null Record;

end Morph;
