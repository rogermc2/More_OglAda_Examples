
with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types;

with Load_Object_File;
with Utilities;

package body Model is

   Model_VAO            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Model_Vertex_Buffer  : GL.Objects.Buffers.Buffer;
   Model_Normals_Buffer : GL.Objects.Buffers.Buffer;
   Model_UVs_Buffer     : GL.Objects.Buffers.Buffer;

   --  ------------------------------------------------------------------------

   procedure Bind_Model_VAO is
   begin
      Model_VAO.Bind;
   end Bind_Model_VAO;

   --  --------------------------------------------------------------------------

   procedure Initialize (aModel : in out Model_Data; File_Path : String;
                         Colour : GL.Types.Colors.Basic_Color) is
      use GL.Objects.Buffers;
      use GL.Types;
      Vertex_Count : Int;
   begin
      aModel.Model_Colour := Colour;
      Model_VAO.Initialize_Id;
      Model_VAO.Bind;

      Vertex_Count := Load_Object_File.Mesh_Size (File_Path);
      declare
         Vertices         : Singles.Vector3_Array (1 .. Vertex_Count);
         UVs              : Singles.Vector2_Array (1 .. Vertex_Count);
         Normals          : Singles.Vector3_Array (1 .. Vertex_Count);
      begin
         Load_Object_File.Load_Object (File_Path, Vertices, UVs, Normals);

         Model_Vertex_Buffer.Initialize_Id;
         Array_Buffer.Bind (Model_Vertex_Buffer);
         Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);
         GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False,
                                                  0, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (0);

         Model_Normals_Buffer.Initialize_Id;
         Array_Buffer.Bind (Model_Normals_Buffer);
         Utilities.Load_Vertex_Buffer (Array_Buffer, Normals, Static_Draw);
         GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, False,
                                                  0, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (1);

         Model_UVs_Buffer.Initialize_Id;
         Array_Buffer.Bind (Model_UVs_Buffer);
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
      use GL.Types.Singles;
      Target_Position  : Vector3 := aModel.Position;
      Current_Position : Vector3 := aModel.Position;
      Target_Rotation  : Vector3 := aModel.Heading_Rotation;
      Distance         : Float := aModel.Velocity * Delta_Time;
   begin
      null;
   end Update;

   --  ------------------------------------------------------------------------

end Model;
