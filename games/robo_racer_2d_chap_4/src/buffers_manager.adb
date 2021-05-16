
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Types;

with Utilities;
with Vertex_Data;

package body Buffers_Manager is

  --  --------------------------------------------------------------------------

   procedure Make_Buffer (Target : GL.Objects.Buffers.Buffer_Target;
                          Buffer : in out GL.Objects.Buffers.Buffer) is
   begin
      Buffer.Initialize_Id;
      Target.Bind (Buffer);
   end Make_Buffer;

   --  -------------------------------------------------------------------------

   procedure Setup_Buffers (Vertex_Buffer  : in out GL.Objects.Buffers.Buffer;
                            Element_Buffer : in out GL.Objects.Buffers.Buffer) is
      use GL.Types;
      use GL.Objects.Buffers;

      Stride : constant GL.Types.Size := Singles.Vector2'Size / 8;
   begin
      Make_Buffer (GL.Objects.Buffers.Array_Buffer, Vertex_Buffer);
      Utilities.Load_Vertex_Buffer
        (Array_Buffer, Vertex_Data.Vertices, Static_Draw);

      Make_Buffer (GL.Objects.Buffers.Element_Array_Buffer, Element_Buffer);
      Vertex_Data.Load_Element_Buffer (Element_Array_Buffer,
                                       Vertex_Data.Elements, Static_Draw);

      GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => 3,
                                               Kind   => GL.Types.Single_Type,
                                               Normalized => False,
                                               Stride => Stride, Offset => 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);

      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, GL.Types.Single_Type,
                                               False, Stride, 3);
      GL.Attributes.Enable_Vertex_Attrib_Array (1);
   exception
      when others =>
         Put_Line ("An exception occurred in Setup_Buffers.");
         raise;
   end Setup_Buffers;

   --  -------------------------------------------------------------------------

end Buffers_Manager;
