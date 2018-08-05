
with Interfaces; use Interfaces;
with Interfaces.C;

with Post_Process; use Post_Process;

package Ogldev_Util is
   use Interfaces.C;

   Assimp_Load_Flags : constant unsigned :=
     AI_Process_Triangulate'Enum_Rep + AI_Process_Gen_Smooth_Normals'Enum_Rep +
       AI_Process_Flip_UVs'Enum_Rep + AI_Process_Join_Identical_Vertices'Enum_Rep;
   pragma Convention (C, Assimp_Load_Flags);

   Invalid_Uniform_Location : constant C.unsigned := 16#FFFFFFFF#;
   Invalid_OGL_Value        : constant C.unsigned := 16#FFFFFFFF#;

end Ogldev_Util;
