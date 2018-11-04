
with System;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
--
with GL.Types;

with Assimp_Mesh;
with Scene;

package Importer is

   function Import_File (File_Name : String; Flags : GL.Types.UInt)
                         return Scene.AI_Scene;
   function Read_File (File_Name : String; Flags : GL.Types.UInt)
                       return Scene.AI_Scene;

private

   type SA_Scene is record
      Flags          : Interfaces.C.unsigned := 0;
      Root_Node      : Scene.API_Node_Ptr;
      Num_Meshes     : Interfaces.C.unsigned := 0;
      Meshes         : Assimp_Mesh.Mesh_Array_Pointers.Pointer;
--        Meshes         : System.Address;
      Num_Materials  : Interfaces.C.unsigned := 0;
      Materials      : System.Address;
      Num_Animations : Interfaces.C.unsigned := 0;
      Animations     : System.Address;
      Num_Textures   : Interfaces.C.unsigned := 0;
      Textures       : System.Address;
      Num_Lights     : Interfaces.C.unsigned := 0;
      Lights         : System.Address;
      Num_Cameras    : Interfaces.C.unsigned := 0;
      Cameras        : System.Address;
      Private_Data   : Interfaces.C.Strings.chars_ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, SA_Scene);

end Importer;
