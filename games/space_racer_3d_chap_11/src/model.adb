
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Programs;
with GL.Types;

with Load_Obj_File;
with Maths;
--  with Utilities;

with Load_Buffers;
with Shader_Manager;

package body Model is
   use GL.Types;

   Program_3D : GL.Objects.Programs.Program;

   procedure Initialize_3D_VBOs (aModel : in out Model_Data);

   --  ------------------------------------------------------------------------

   procedure Bind_Model_VAO (aModel : in out Model_Data) is
   begin
      aModel.Model_VAO.Bind;
   end Bind_Model_VAO;

   --  ------------------------------------------------------------------------

   procedure Bind_Vertex_VBO (aModel : in out Model_Data) is
   begin
      aModel.Model_VAO.Bind;
      GL.Objects.Buffers.Array_Buffer.Bind (aModel.Model_Vertex_Buffer);
   end Bind_Vertex_VBO;

   --  ------------------------------------------------------------------------

   procedure Bind_Element_VBO (aModel : in out Model_Data) is
   begin
      aModel.Model_VAO.Bind;
      GL.Objects.Buffers.Array_Buffer.Bind (aModel.Model_Element_Buffer);
   end Bind_Element_VBO;

   --  ------------------------------------------------------------------------

   function Centre (aModel : Model_Data) return GL.Types.Singles.Vector3 is
      theCentre : Singles.Vector3 := aModel.Position;
   begin
      if aModel.Is_Ship then
         theCentre (GL.Y) := aModel.Position (GL.Z);
         theCentre (GL.Z) := -aModel.Position (GL.Y);
      end if;
      return theCentre;
   end Centre;

   --  ------------------------------------------------------------------------

   function Collided_With (thisModel, Target : Model.Model_Data)
                           return Boolean is
      P1        : constant Singles.Vector3 := thisModel.Centre;
      P2        : constant Singles.Vector3 := Target.Centre;
      P_Delta   : Singles.Vector3;
      Dist_Sq   : Single;
      Rad_1_Sq  : Single;
      Rad_2_Sq  : Single;
      Result    : Boolean := False;
   begin
      if thisModel.Is_Collidable and Target.Is_Collidable then
         P_Delta := P2 - P1;
         Dist_Sq := P_Delta (GL.X) ** 2 + P_Delta (GL.Y) ** 2 +
           P_Delta (GL.Z) ** 2;
         Rad_1_Sq := thisModel.Radius ** 2;
         Rad_2_Sq := Target.Radius ** 2;
         Result := Dist_Sq <= Rad_1_Sq + Rad_2_Sq;
      end if;
      return Result;
   end Collided_With;

   --  ------------------------------------------------------------------------

   function Heading (aModel : Model_Data) return GL.Types.Singles.Vector3 is
   begin
      return aModel.Heading;
   end Heading;

   --  ------------------------------------------------------------------------

   function Heading_Rotation (aModel : Model_Data)
                              return GL.Types.Singles.Vector3 is
   begin
      return aModel.Heading_Rotation;
   end Heading_Rotation;

   --  ------------------------------------------------------------------------

   procedure Initialize_3D (aModel : in out Model_Data; File_Path : String;
                            Colour : GL.Types.Colors.Basic_Color) is
      use Load_Obj_File;
      Vertices       : Obj_Array3;
      Normals        : Obj_Array3;
      UVs            : Obj_Array2;
      Vertex_Indices : Obj_Int3_Array;
      Normal_Indices : Obj_Int3_Array;
      UV_Indices     : Obj_Int3_Array;
   begin
      Shader_Manager.Init_Shaders (Program_3D);
      aModel.Model_Colour := Colour;

      Load_Object (File_Path, Vertices, Normals, UVs, Vertex_Indices,
                   Normal_Indices, UV_Indices);

      Initialize_3D_VBOs (aModel);
      Load_Buffers.Load_Buffers (aModel, Vertices, Vertex_Indices);

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Model.Initialize_3D.");
         Put_Line (Exception_Information (anError));
         raise;
   end Initialize_3D;

   --  ------------------------------------------------------------------------

   procedure Initialize_3D_VBOs (aModel : in out Model_Data) is
   begin
      aModel.Model_VAO.Initialize_Id;
      aModel.Model_VAO.Bind;

      aModel.Model_Vertex_Buffer.Initialize_Id;
      GL.Objects.Buffers.Array_Buffer.Bind (aModel.Model_Vertex_Buffer);
      aModel.Model_Element_Buffer.Initialize_Id;
      GL.Objects.Buffers.Element_Array_Buffer.Bind (aModel.Model_Element_Buffer);
   end Initialize_3D_VBOs;

   --  ------------------------------------------------------------------------

   function Position (aModel : Model_Data) return GL.Types.Singles.Vector3 is
   begin
      return aModel.Position;
   end Position;

   --  ------------------------------------------------------------------------

   procedure Render (aModel : in out Model_Data) is
   use GL.Attributes;
      use GL.Objects.Buffers;
      use Shader_Manager;
      Model_Matrix : Matrix4 := Singles.Identity4;
      View_Matrix  : constant Matrix4 := Singles.Identity4;
      Rot_Matrix   : Matrix4 := Singles.Identity4;
   begin
--        Put_Line ("Model.Render.");

      if aModel.Is_Visible then
         GL.Objects.Programs.Use_Program (Program_3D);
         Maths.Init_Rotation_Transform
           (aModel.Base_Rotation, Rot_Matrix);
         Model_Matrix :=  Rot_Matrix * Model_Matrix;
         if aModel.Is_Ship then
            Model_Matrix := Maths.Scaling_Matrix ((0.8, 0.8, 0.8))
              * Model_Matrix;
            Maths.Init_Rotation_Transform
              (aModel.Heading_Rotation, Rot_Matrix);
            Model_Matrix :=  Rot_Matrix * Model_Matrix;
            Model_Matrix :=
              Maths.Translation_Matrix (aModel.Position) * Model_Matrix;
         end if;

         if not aModel.Is_Ship then
            Model_Matrix := Maths.Translation_Matrix (aModel.Position) *
              Model_Matrix;
         end if;

         Set_Colour (Program_3D, aModel.Model_Colour);
         Set_Model_Matrix (Program_3D, Model_Matrix);
         Set_View_Matrix (Program_3D, View_Matrix);

         aModel.Model_VAO.Bind;
         Array_Buffer.Bind (aModel.Model_Vertex_Buffer);
         Element_Array_Buffer.Bind (aModel.Model_Element_Buffer);
         Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False, 0, 0);
         Enable_Vertex_Attrib_Array (0);

         Draw_Elements (Triangle_Strip_Adjacency, aModel.Indices_Size,
                        UInt_Type, 0);
--           GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, 1);
         Disable_Vertex_Attrib_Array (0);
      end if;

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Model.Render.");
         Put_Line (Exception_Information (anError));
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Set_Base_Rotation (aModel   : in out Model_Data;
                                Rotation : GL.Types.Singles.Vector3) is
   begin
      aModel.Base_Rotation := Rotation;
   end Set_Base_Rotation;

   --  ------------------------------------------------------------------------

   procedure Set_Heading (aModel  : in out Model_Data;
                          Heading : GL.Types.Singles.Vector3) is
   begin
      aModel.Heading := Heading;
   end Set_Heading;

   --  ------------------------------------------------------------------------

   procedure Set_Heading_Rotation (aModel   : in out Model_Data;
                                   Rotation : GL.Types.Singles.Vector3) is
   begin
      aModel.Heading_Rotation := Rotation;
   end Set_Heading_Rotation;

   --  ------------------------------------------------------------------------

   procedure Set_Indices_Size (aModel : in out Model_Data;
                               Size   : GL.Types.Int) is
   begin
      aModel.Indices_Size := Size;
   end Set_Indices_Size;

   --  -------------------------------------------------------------------------

   procedure Set_Is_Collidable (aModel : in out Model_Data; State : Boolean) is
   begin
      aModel.Is_Collidable := State;
   end Set_Is_Collidable;

   --  ------------------------------------------------------------------------

   procedure Set_Is_Ship (aModel : in out Model_Data; State : Boolean) is
   begin
      aModel.Is_Ship := State;
   end Set_Is_Ship;

   --  ------------------------------------------------------------------------

   procedure Set_Is_Visible (aModel : in out Model_Data; State : Boolean) is
   begin
      aModel.Is_Visible := State;
   end Set_Is_Visible;

   --  ------------------------------------------------------------------------

   procedure Set_Perspective (Projection_Matrix : GL.Types.Singles.Matrix4) is
   begin
      GL.Objects.Programs.Use_Program (Program_3D);
      Shader_Manager.Set_Projection_Matrix (Program_3D, Projection_Matrix);
   end Set_Perspective;

   --  ------------------------------------------------------------------------

   procedure Set_Position (aModel   : in out Model_Data;
                           Position : GL.Types.Singles.Vector3) is
   begin
      aModel.Position := Position;
   end Set_Position;

   --  ------------------------------------------------------------------------

   procedure Set_Velocity (aModel   : in out Model_Data;
                           Velocity : GL.Types.Single) is
   begin
      aModel.Velocity := Velocity;
   end Set_Velocity;

   --  ------------------------------------------------------------------------

   procedure Set_Vertex_Count (aModel : in out Model_Data;
                               Count  : GL.Types.Int) is
   begin
      aModel.Vertex_Count := Count;
   end Set_Vertex_Count;

   --  ------------------------------------------------------------------------

   function To_Radian (Degrees : GL.Types.Singles.Vector3)
                       return GL.Types.Singles.Vector3 is
      use Ada.Numerics;
   begin
      return Pi / 180.0 * Degrees;
   end To_Radian;

   ------------------------------------------------------------------------

   procedure Update (aModel : in out Model_Data; Delta_Time : Float) is
      use Maths;
      use Maths.Single_Math_Functions;
      Distance         : constant Single :=
                           aModel.Velocity * Single (Delta_Time);
      Target_Rotation  : constant Vector3 :=
                           To_Radian (aModel.Heading_Rotation);
      Delta_Position   : Vector3 := (0.0, 0.0, 0.0);
   begin
      Delta_Position (GL.Y) := Cos (Target_Rotation (GL.Z)) * Distance;
      Delta_Position (GL.X) := -Sin (Target_Rotation (GL.Z)) * Distance;
      Delta_Position (GL.Z) := Sin (Target_Rotation (GL.X)) * Distance;

      aModel.Position := aModel.Position + Delta_Position;

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Model.Render.");
         Put_Line (Exception_Information (anError));
         raise;
   end Update;

   --  ------------------------------------------------------------------------

   function Velocity (aModel : Model_Data) return GL.Types.Single is
   begin
      return aModel.Velocity;
   end Velocity;

   --  ------------------------------------------------------------------------

end Model;
