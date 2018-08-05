
with Ada.Strings.Unbounded;

with GL.Types; use GL.Types;

with Assimp_Types;

with API_Vectors;

package Assimp_Util is

   function To_Assimp_AI_String
     (UB_String :  Ada.Strings.Unbounded.Unbounded_String)
      return Assimp_Types.AI_String;
   function To_OGL_Vector2 (C_Vec : API_Vectors.API_Vector_2D)
                            return Singles.Vector2;
   function To_OGL_Vector3 (C_Vec : API_Vectors.API_Vector_3D)
                            return Singles.Vector3;
   function To_Colour3D (C_Colours : API_Vectors.API_Colour_3D)
                          return Singles.Vector3;
   function To_Colour4D (C_Colours : API_Vectors.API_Colour_4D)
                          return Singles.Vector4;

end Assimp_Util;
