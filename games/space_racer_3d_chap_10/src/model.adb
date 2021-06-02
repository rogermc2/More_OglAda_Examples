
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Programs;
--  with GL.Toggles;
with GL.Types;

with Load_Obj_File;
with Maths;
--  with Utilities;

with Load_Buffers;
with Shader_Manager_Model;

package body Model is
   use GL.Types;

   Model_Program  : GL.Objects.Programs.Program;

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

   procedure Initialize (aModel : in out Model_Data; File_Path : String;
                         Colour : GL.Types.Colors.Basic_Color) is
      use Load_Obj_File;
      Vertices       : Obj_Array3;
      Normals        : Obj_Array3;
      UVs            : Obj_Array2;
      Vertex_Indices : Obj_Int3_Array;
      Normal_Indices : Obj_Int3_Array;
      UV_Indices     : Obj_Int3_Array;
   begin
      Shader_Manager_Model.Init_Shaders (Model_Program);
      aModel.Model_Colour := Colour;

      Load_Object (File_Path, Vertices, Normals, UVs, Vertex_Indices,
                   Normal_Indices, UV_Indices);

      Initialize_VBOs (aModel);
      Load_Buffers.Load_Buffers (aModel, Vertices, Vertex_Indices);

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Model.Initialize.");
         Put_Line (Exception_Information (anError));
         raise;
   end Initialize;

   --  ------------------------------------------------------------------------

   procedure Initialize_VBOs (aModel : in out Model_Data) is
   begin
      aModel.Model_VAO.Initialize_Id;
      aModel.Model_VAO.Bind;

      aModel.Model_Vertex_Buffer.Initialize_Id;
      GL.Objects.Buffers.Array_Buffer.Bind (aModel.Model_Vertex_Buffer);
      aModel.Model_Element_Buffer.Initialize_Id;
      GL.Objects.Buffers.Element_Array_Buffer.Bind (aModel.Model_Element_Buffer);
   end Initialize_VBOs;

   --  ------------------------------------------------------------------------

   procedure Render (aModel : in out Model_Data) is
--        use Maths;
      use GL.Objects.Buffers;
      use Shader_Manager_Model;
      Model_Matrix : Matrix4 := Singles.Identity4;
      View_Matrix  : Matrix4 := Singles.Identity4;
      Rot_Matrix   : Matrix4 := Singles.Identity4;
   begin
--        GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);
      GL.Objects.Programs.Use_Program (Model_Program);

      if aModel.Is_Visible then
--           Model_Matrix := Scaling_Matrix ((0.6, 0.6, 0.6));

         Maths.Init_Rotation_Transform
           (aModel.Base_Rotation, Rot_Matrix);
         Model_Matrix :=  Rot_Matrix * Model_Matrix;
         if aModel.Is_Ship then
            Maths.Init_Rotation_Transform
              (aModel.Base_Rotation, View_Matrix);
            View_Matrix :=
              Maths.Translation_Matrix (aModel.Position) * View_Matrix;
         end if;

         if not aModel.Is_Ship then
            Model_Matrix := Maths.Translation_Matrix (aModel.Position) *
              Model_Matrix;
         end if;

         Set_Colour (aModel.Model_Colour);
         Set_Model_Matrix (Model_Matrix);
         Set_View_Matrix (View_Matrix);

         aModel.Model_VAO.Bind;
         Array_Buffer.Bind (aModel.Model_Vertex_Buffer);
         Element_Array_Buffer.Bind (aModel.Model_Element_Buffer);

         Draw_Elements (Triangle_Strip_Adjacency, aModel.Indices_Size,
                        UInt_Type, 0);
--           GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, 1);
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

   procedure Set_Indices_Size (aModel : in out Model_Data;
                               Size   : GL.Types.Int) is
   begin
      aModel.Indices_Size := Size;
   end Set_Indices_Size;

   --  --------------------------------------------------------------
   procedure Set_Is_Ship (aModel : in out Model_Data; State : Boolean) is
   begin
      aModel.Is_Ship := State;
   end Set_Is_Ship;

   --  ------------------------------------------------------------------------

   procedure Set_Perspective (Projection_Matrix : GL.Types.Singles.Matrix4) is
   begin
      GL.Objects.Programs.Use_Program (Model_Program);
      Shader_Manager_Model.Set_Projection_Matrix (Projection_Matrix);
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

   procedure Update (aModel : in out Model_Data; Delta_Time : Float) is
      use Maths;
      use Maths.Single_Math_Functions;
      Distance         : constant Single :=
                           aModel.Velocity * Single (Delta_Time);
      Target_Position  : Vector3 := aModel.Position;
      Delta_Position   : Vector3;
   begin
      Delta_Position (GL.X) := Cos (Target_Position (GL.Z)) * Distance;
      Delta_Position (GL.Y) := -Sin (Target_Position (GL.Z)) * Distance;
      Delta_Position (GL.Z) := Sin (Target_Position (GL.X)) * Distance;

      Target_Position := Target_Position + Delta_Position;
      aModel.Position := Target_Position;

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Model.Render.");
         Put_Line (Exception_Information (anError));
         raise;
   end Update;

   --  ------------------------------------------------------------------------

end Model;
