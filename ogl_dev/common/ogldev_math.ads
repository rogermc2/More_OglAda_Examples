
with Ada.Containers.Indefinite_Ordered_Maps;

with GL.Types; use GL.Types;

with API_Vectors_Matrices;
with Assimp_Types;

package Ogldev_Math is
   use GL.Types.Singles;

   package AI_2D_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, Singles.Vector2);
   subtype AI_2D_Map is AI_2D_Package.Map;

   package AI_3D_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, Singles.Vector3);
   subtype AI_3D_Map is AI_3D_Package.Map;

   type Perspective_Projection_Info is record
      FOV       : Single := 60.0;
      Width     : UInt := 800;
      Height    : UInt := 600;
      Z_Near    : Single := -1.0;
      Z_Far     : Single := 50.0;
   end record;

   type Orthographic_Projection_Info is record
      Right  : Single;
      Left   : Single;
      Bottom : Single;
      Top    : Single;
      Z_Near : Single;
      Z_Far  : Single;
   end record;

   package Vector3_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, Singles.Vector3);
   type Vector3D is new Vector3_Package.Map with null Record;

   function Init_Camera_Transform (Target, Up : Vector3) return Matrix4;
   function To_AI_Map3D (Num_Vecs : UInt := 0;
                         Vectors : Assimp_Types.Vector3_Array) return AI_3D_Map;
   function To_GL_Matrix4 (M4 : API_Vectors_Matrices.API_Matrix_4D)
                           return Singles.Matrix4;

end Ogldev_Math;
