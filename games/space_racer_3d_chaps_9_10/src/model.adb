
with GL.Attributes;
with GL.Objects.Buffers;
with GL.Types;

with Load_Object_File;
with Utilities;

package body Model is

   Vertex_Buffer            : GL.Objects.Buffers.Buffer;
   Normals_Buffer           : GL.Objects.Buffers.Buffer;
   UVs_Buffer               : GL.Objects.Buffers.Buffer;

    --  ------------------------------------------------------------------------

    procedure Initialize (File_Path : String) is
        use GL.Objects.Buffers;
        use GL.Types;
        Vertex_Count : Int;
    begin
      Vertex_Count := Load_Object_File.Mesh_Size (File_Path);
      declare
         Vertices         : Singles.Vector3_Array (1 .. Vertex_Count);
         UVs              : Singles.Vector2_Array (1 .. Vertex_Count);
         Normals          : Singles.Vector3_Array (1 .. Vertex_Count);
      begin
         Load_Object_File.Load_Object ("Vertex_Count", Vertices, UVs, Normals);

         Vertex_Buffer.Initialize_Id;
         Array_Buffer.Bind (Vertex_Buffer);
         Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);
         GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False,
                                                  0, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (0);

         Normals_Buffer.Initialize_Id;
         Array_Buffer.Bind (Normals_Buffer);
         Utilities.Load_Vertex_Buffer (Array_Buffer, Normals, Static_Draw);
         GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, False,
                                                  0, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (1);

         UVs_Buffer.Initialize_Id;
         Array_Buffer.Bind (UVs_Buffer);
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

end Model;
