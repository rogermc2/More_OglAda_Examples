
with Interfaces.C;

with Scene;

package Assimp.API is

   function Import_File (File_Name : Interfaces.C.char_array;
                         Flags : Interfaces.C.unsigned) return access Scene.API_Scene;
   pragma Import (C, Import_File, "aiImportFile");

   function Read_File (File_Name : Interfaces.C.char_array;
                       Flags : Interfaces.C.unsigned) return access Scene.API_Scene;
   pragma Import (C, Read_File, "aiReadFile");

end Assimp.API;
