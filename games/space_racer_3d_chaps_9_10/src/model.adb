
with GL.Attributes;
with GL.Types;

with Load_Object_File;
with Maths;
with Utilities;

package body Model is

   --  ------------------------------------------------------------------------

   procedure Bind_Model_VAO (aModel : in out Model_Data) is
   begin
      aModel.Model_VAO.Bind;
   end Bind_Model_VAO;

   --  --------------------------------------------------------------------------

   procedure Initialize (aModel : in out Model_Data; File_Path : String;
                         Colour : GL.Types.Colors.Basic_Color) is
      use GL.Objects.Buffers;
      use GL.Types;
      Vertex_Count : Int;
   begin
      aModel.Model_Colour := Colour;
      aModel.Model_VAO.Initialize_Id;
      aModel.Model_VAO.Bind;

      Vertex_Count := Load_Object_File.Mesh_Size (File_Path);
      declare
         Vertices         : Singles.Vector3_Array (1 .. Vertex_Count);
         UVs              : Singles.Vector2_Array (1 .. Vertex_Count);
         Normals          : Singles.Vector3_Array (1 .. Vertex_Count);
      begin
         Load_Object_File.Load_Object (File_Path, Vertices, UVs, Normals);

         aModel.Model_Vertex_Buffer.Initialize_Id;
         Array_Buffer.Bind (aModel.Model_Vertex_Buffer);
         Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);
         GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False,
                                                  0, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (0);

         aModel.Model_Normals_Buffer.Initialize_Id;
         Array_Buffer.Bind (aModel.Model_Normals_Buffer);
         Utilities.Load_Vertex_Buffer (Array_Buffer, Normals, Static_Draw);
         GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, False,
                                                  0, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (1);

         aModel.Model_UVs_Buffer.Initialize_Id;
         Array_Buffer.Bind (aModel.Model_UVs_Buffer);
         Utilities.Load_Vertex_Buffer (Array_Buffer, UVs, Static_Draw);
         GL.Attributes.Set_Vertex_Attrib_Pointer (2, 2, Single_Type, False,
                                                  0, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (2);

      end;

   end Initialize;

   --  ------------------------------------------------------------------------

   procedure Render is
   begin
      null;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Update (aModel : in out Model_Data; Delta_Time : Float) is
      use Maths;
      use Maths.Single_Math_Functions;
      use GL.Types;
      use GL.Types.Singles;
      Distance         : constant Single :=
                             aModel.Velocity * Single (Delta_Time);
      Target_Position  : Vector3 := aModel.Position;
      Delta_Position   : Vector3;
--        Target_Rotation  : Vector3 := aModel.Heading_Rotation;
   begin
      Delta_Position (GL.X) := Cos (Target_Position (GL.Z)) * Distance;
      Delta_Position (GL.Y) := -Sin (Target_Position (GL.Z)) * Distance;
      Delta_Position (GL.Z) := Sin (Target_Position (GL.X)) * Distance;

      Target_Position := Target_Position + Delta_Position;
      aModel.Position := Target_Position;

   end Update;

   --  ------------------------------------------------------------------------

end Model;
