
with Ada.Text_IO; use Ada.Text_IO;

with Load_Object_File;
with Utilities;
with VBO_Indexer;

package body Load_Obj_Buffers is

   procedure Load_Buffers (Path : String;
                           Vertex_Buffer : in out GL.Objects.Buffers.Buffer;
                            UVs_Buffer : in out GL.Objects.Buffers.Buffer;
                            Normals_Buffer : in out GL.Objects.Buffers.Buffer;
                            Element_Buffer : in out GL.Objects.Buffers.Buffer;
                            Vertex_Count, Indices_Size : out GL.Types.Int) is
      use GL.Objects.Buffers;
      use GL.Types;

      Vertices_Size   : Int;
   begin
      Vertex_Count := Load_Object_File.Mesh_Size (Path);
      declare
         Vertices         : Singles.Vector3_Array (1 .. Vertex_Count);
         UVs              : Singles.Vector2_Array (1 .. Vertex_Count);
         Normals          : Singles.Vector3_Array (1 .. Vertex_Count);
         Indexed_Vertices : Singles.Vector3_Array (1 .. Vertex_Count);
         Indexed_UVs      : Singles.Vector2_Array (1 .. Vertex_Count);
         Indexed_Normals  : Singles.Vector3_Array (1 .. Vertex_Count);
         Temp_Indices     : UInt_Array (1 .. Vertex_Count);

      begin
         Load_Object_File.Load_Object (Path, Vertices, UVs, Normals);
         VBO_Indexer.Index_VBO (Vertices, UVs,  Normals,
                                Indexed_Vertices, Indexed_UVs, Indexed_Normals,
                                Temp_Indices, Indices_Size, Vertices_Size);
         declare
            Vertices_Indexed : constant Singles.Vector3_Array (1 .. Vertices_Size)
              := Indexed_Vertices  (1 .. Vertices_Size);
            UVs_Indexed      : constant Singles.Vector2_Array (1 .. Vertices_Size)
              := Indexed_UVs  (1 .. Vertices_Size);
            Normals_Indexed  : constant Singles.Vector3_Array (1 .. Vertices_Size)
              := Indexed_Normals  (1 .. Vertices_Size);
            Indices          : constant GL.Types.UInt_Array (1 .. Indices_Size)
              := Temp_Indices  (1 .. Indices_Size);
         begin
            Vertex_Buffer.Initialize_Id;
            Array_Buffer.Bind (Vertex_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices_Indexed, Static_Draw);

            UVs_Buffer.Initialize_Id;
            Array_Buffer.Bind (UVs_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, UVs_Indexed, Static_Draw);

            Normals_Buffer.Initialize_Id;
            Array_Buffer.Bind (Normals_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Normals_Indexed, Static_Draw);

            Element_Buffer.Initialize_Id;
            Element_Array_Buffer.Bind (Element_Buffer);
            Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices, Static_Draw);
         end;
      end;

   exception
      when others =>
         Put_Line ("An exception occurred in Load_Obj_Buffers.Load_Buffers.");
         raise;
   end Load_Buffers;

   --  ------------------------------------------------------------------------

end Load_Obj_Buffers;
