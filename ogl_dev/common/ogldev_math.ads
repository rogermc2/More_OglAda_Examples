
with Interfaces.C.Pointers;

with Ada.Containers.Indefinite_Ordered_Maps;

with GL.Types; use GL.Types;

with API_Vectors_Matrices;
with Assimp_Types;

package Ogldev_Math is
   use GL.Types.Singles;

   package AI_3D_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, Singles.Vector3);
   subtype AI_3D_Map is AI_3D_Package.Map;

   type Index_11 is (X, Y, Z, U, V, NX, NY, NZ, TX, TY, TZ);
   type Vector11 is array (Index_11) of aliased Single;

   pragma Convention (C, Vector11);

   type Vector11_Array is array (Size range <>) of aliased Vector11;
   pragma Convention (C, Vector11_Array);

   package Vector11_Pointers is new Interfaces.C.Pointers
     (Size, Vector11, Vector11_Array, Vector11'(others => <>));

   type Perspective_Projection_Info is private;
   type Orthographic_Projection_Info is private;

   function Get_Orthograpic_Bottom (Info : Orthographic_Projection_Info)
                                    return Single;
   function Get_Orthograpic_Far (Info : Orthographic_Projection_Info)
                                 return Single;
   function Get_Orthograpic_Left (Info : Orthographic_Projection_Info)
                                  return Single;
   function Get_Orthograpic_Near (Info : Orthographic_Projection_Info)
                                  return Single;

   function Get_Orthograpic_Right (Info : Orthographic_Projection_Info)
                                   return Single;
   function Get_Orthograpic_Top (Info : Orthographic_Projection_Info)
                               return Single;

   function Get_Perspective_Far (Info : Perspective_Projection_Info) return Single;
   function Get_Perspective_FOV (Info : Perspective_Projection_Info) return Single;
   function Get_Perspective_Height (Info : Perspective_Projection_Info) return UInt;
   function Get_Perspective_Near (Info : Perspective_Projection_Info) return Single;
   function Get_Perspective_Width (Info : Perspective_Projection_Info) return UInt;
   function Init_Camera_Transform (Target, Up : Vector3) return Matrix4;
   procedure Set_Orthograpic_Info (Info      : out Orthographic_Projection_Info;
                                   Right, Left, Bottom, Top,
                                   Near, Far : Single);
   procedure Set_Perspective_Info (Info      : out Perspective_Projection_Info;
                                   FOV       : Single; Width, Height : UInt;
                                   Near, Far : Single);
   procedure Set_Perspective_Far (Info  : in out Perspective_Projection_Info;
                                  Far   : Single);
   procedure Set_Perspective_FOV (Info   : in out Perspective_Projection_Info;
                                  FOV    : Single);
   procedure Set_Perspective_Height (Info   : in out Perspective_Projection_Info;
                                     Height : UInt);
   procedure Set_Perspective_Near (Info  : in out Perspective_Projection_Info;
                                   Near  : Single);
   procedure Set_Perspective_Width (Info  : in out Perspective_Projection_Info;
                                    Width : UInt);
   function To_AI_Map3D (Num_Vecs : UInt := 0;
                         Vectors  : Assimp_Types.Vector3_Array) return AI_3D_Map;
   function To_GL_Matrix4 (M4 : API_Vectors_Matrices.API_Matrix_4D)
                           return Singles.Matrix4;
private

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

end Ogldev_Math;
