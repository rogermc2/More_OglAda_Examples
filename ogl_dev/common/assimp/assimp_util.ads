
with Ada.Strings.Unbounded;

with GL.Types; use GL.Types;

with Assimp_Types;

with API_Vectors_Matrices;

package Assimp_Util is

   function To_Assimp_API_String
     (UB_String : Ada.Strings.Unbounded.Unbounded_String)
      return Assimp_Types.API_String;
   function To_OGL_Vector2 (C_Vec : API_Vectors_Matrices.API_Vector_2D)
                            return Singles.Vector2;
   function To_OGL_Vector3 (C_Vec : API_Vectors_Matrices.API_Vector_3D)
                            return Singles.Vector3;
   function To_Colour3D (C_Colours : API_Vectors_Matrices.API_Colour_3D)
                          return Singles.Vector3;
   function To_Colour4D (C_Colours : API_Vectors_Matrices.API_Colour_4D)
                         return Singles.Vector4;
   function To_Unbounded_String (API_String : Assimp_Types.API_String)
                                 return Ada.Strings.Unbounded.Unbounded_String;
   function To_String (API_String : Assimp_Types.API_String) return String;

end Assimp_Util;
