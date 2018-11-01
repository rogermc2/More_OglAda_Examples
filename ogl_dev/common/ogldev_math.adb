
with Maths;

package body Ogldev_Math is

   function Get_Orthograpic_Far (Info : Orthographic_Projection_Info)
                                 return Single is
   begin
      return Info.Z_Far;
   end Get_Orthograpic_Far;

   --  -------------------------------------------------------------------------

   function Get_Orthograpic_Bottom (Info : Orthographic_Projection_Info)
                                    return Single is
   begin
      return Info.Bottom;
   end Get_Orthograpic_Bottom;

   --  -------------------------------------------------------------------------

   function Get_Orthograpic_Left (Info : Orthographic_Projection_Info)
                                  return Single is
   begin
      return Info.Left;
   end Get_Orthograpic_Left;

   --  -------------------------------------------------------------------------

  function Get_Orthograpic_Near (Info : Orthographic_Projection_Info)
                                 return Single is
   begin
      return Info.Z_Near;
   end Get_Orthograpic_Near;

   --  -------------------------------------------------------------------------

   function Get_Orthograpic_Right (Info : Orthographic_Projection_Info)
                                  return Single is
   begin
      return Info.Right;
   end Get_Orthograpic_Right;

   --  -------------------------------------------------------------------------

 function Get_Orthograpic_Top (Info : Orthographic_Projection_Info)
                               return Single is
   begin
      return Info.Top;
   end Get_Orthograpic_Top;

   --  -------------------------------------------------------------------------

   function Get_Perspective_Far (Info : Perspective_Projection_Info)
                                 return Single is
   begin
      return Info.Z_Far;
   end Get_Perspective_Far;

   --  -------------------------------------------------------------------------

   function Get_Perspective_FOV (Info : Perspective_Projection_Info)
                                 return Single is
   begin
      return Info.FOV;
   end Get_Perspective_FOV;

   --  -------------------------------------------------------------------------

   function Get_Perspective_Height (Info : Perspective_Projection_Info)
                                 return UInt is
   begin
      return Info.Height;
   end Get_Perspective_Height;

   --  -------------------------------------------------------------------------

  function Get_Perspective_Near (Info : Perspective_Projection_Info)
                                 return Single is
   begin
      return Info.Z_Near;
   end Get_Perspective_Near;

   --  -------------------------------------------------------------------------

 function Get_Perspective_Width (Info : Perspective_Projection_Info)
                                 return UInt is
   begin
      return Info.Width;
   end Get_Perspective_Width;

   --  -------------------------------------------------------------------------

   function Init_Camera_Transform (Target, Up : Vector3) return Matrix4 is
      use GL;
      use Maths;
      N     : constant Vector3 := Normalized (Target);
      U     : constant Vector3 := Normalized (Cross_Product (Up, N));
      V     : constant Vector3 := Cross_Product (N, U);
      Trans : Matrix4 := Identity4;
   begin
      Trans (X, X) := U (X);
      Trans (X, Y) := U (Y);
      Trans (X, Z) := U (Z);
      Trans (Y, X) := V (X);
      Trans (Y, Y) := V (Y);
      Trans (Y, Z) := V (Z);
      Trans (Z, X) := N (X);
      Trans (Z, Y) := N (Y);
      Trans (Z, Z) := N (Z);
      return Trans;
   end Init_Camera_Transform;

   --  -------------------------------------------------------------------------

   procedure Set_Orthograpic_Info (Info      : out Orthographic_Projection_Info;
                                   Right, Left, Bottom, Top,
                                   Near, Far : Single) is
   begin
      Info.Right := Right;
      Info.Left := Left;
      Info.Bottom := Bottom;
      Info.Top := Top;
      Info.Z_Near := Near;
      Info.Z_Far := Far;
   end Set_Orthograpic_Info;

   --  -------------------------------------------------------------------------

   procedure Set_Perspective_Info (Info      : out Perspective_Projection_Info;
                                   FOV : Single; Width, Height: UInt;
                                   Near, Far : Single) is
   begin
      Info.FOV := FOV;
      Info.Width := Width;
      Info.Height := Height;
      Info.Z_Near := Near;
      Info.Z_Far := Far;
   end Set_Perspective_Info;

   --  -------------------------------------------------------------------------

   procedure Set_Perspective_Far (Info : in out Perspective_Projection_Info;
                                  Far  : Single) is
   begin
      Info.Z_Far := Far;
   end Set_Perspective_Far;

   --  -------------------------------------------------------------------------

   procedure Set_Perspective_FOV (Info : in out Perspective_Projection_Info;
                                  FOV  : Single) is
   begin
      Info.FOV := FOV;
   end Set_Perspective_FOV;

   --  -------------------------------------------------------------------------

   procedure Set_Perspective_Height (Info   : in out Perspective_Projection_Info;
                                     Height : UInt) is
   begin
      Info.Height := Height;
   end Set_Perspective_Height;

   --  -------------------------------------------------------------------------

   procedure Set_Perspective_Near (Info : in out Perspective_Projection_Info;
                                   Near : Single) is
   begin
      Info.Z_Near := Near;
   end Set_Perspective_Near;

   --  -------------------------------------------------------------------------


   procedure Set_Perspective_Width (Info  : in out Perspective_Projection_Info;
                                    Width : UInt) is
   begin
      Info.Width := Width;
   end Set_Perspective_Width;

   --  -------------------------------------------------------------------------


   function To_AI_Map3D (Num_Vecs : UInt := 0;
                         Vectors : Assimp_Types.Vector3_Array)
                         return AI_3D_Map is
      Vec_Map : AI_3D_Map;
      Vec     : Singles.Vector3;
   begin
      for index in 1 .. Num_Vecs loop
         Vec (GL.X) := Single (Vectors (index).X);
         Vec (GL.Y) := Single (Vectors (index).Y);
         Vec (GL.Z) := Single (Vectors (index).Z);
         Vec_Map.Insert (index, Vec);
      end loop;
      return Vec_Map;
   end To_AI_Map3D;

   --  -------------------------------------------------------------------------

   function To_GL_Matrix4 (M4 : API_Vectors_Matrices.API_Matrix_4D)
                           return Singles.Matrix4 is
      Mat : Singles.Matrix4;
   begin
      Mat (GL.X, GL.X) := Single (M4.A1);
      Mat (GL.X, GL.Y) := Single (M4.A2);
      Mat (GL.X, GL.Z) := Single (M4.A3);
      Mat (GL.X, GL.W) := Single (M4.A4);

      Mat (GL.Y, GL.X) := Single (M4.B1);
      Mat (GL.Y, GL.Y) := Single (M4.B2);
      Mat (GL.Y, GL.Z) := Single (M4.B3);
      Mat (GL.Y, GL.W) := Single (M4.B4);

      Mat (GL.Z, GL.X) := Single (M4.C1);
      Mat (GL.Z, GL.Y) := Single (M4.C2);
      Mat (GL.Z, GL.Z) := Single (M4.C3);
      Mat (GL.Z, GL.W) := Single (M4.C4);

      Mat (GL.W, GL.X) := Single (M4.D1);
      Mat (GL.W, GL.Y) := Single (M4.D2);
      Mat (GL.W, GL.Z) := Single (M4.D3);
      Mat (GL.W, GL.W) := Single (M4.D4);

      return Mat;
   end To_GL_Matrix4;

end Ogldev_Math;
