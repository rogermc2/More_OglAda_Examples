
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Types;

with Utilities;
with Cube_Data;

package body Buffers_Manager is

   Vertex_Buffer  : GL.Objects.Buffers.Buffer;
   Colour_Buffer  : GL.Objects.Buffers.Buffer;

  --  --------------------------------------------------------------------------

   procedure Setup_Buffers is
      use GL.Types;
      use GL.Objects.Buffers;

   begin
      Cube_VAO.Initialize_Id;
      Cube_VAO.Bind;

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Utilities.Load_Vertex_Buffer
        (Array_Buffer, Cube_Data.Vertex_Data, Static_Draw);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => 3,
                                               Kind   => GL.Types.Single_Type,
                                               Normalized => False,
                                               Stride => 0, Offset => 0);

      Colour_Buffer.Initialize_Id;
      Array_Buffer.Bind (Colour_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Cube_Data.Colour_Data,
                                    Static_Draw);
      GL.Attributes.Enable_Vertex_Attrib_Array (1);
      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, GL.Types.Single_Type,
                                               False, 0, 0);

   exception
      when others =>
         Put_Line ("An exception occurred in Buffers_Manager.Setup_Buffers.");
         raise;
   end Setup_Buffers;

   --  -------------------------------------------------------------------------

end Buffers_Manager;
