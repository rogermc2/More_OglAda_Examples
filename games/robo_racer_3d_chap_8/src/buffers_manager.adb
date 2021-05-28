
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Types;

with Utilities;
with Cube_Data;

package body Buffers_Manager is

  --  --------------------------------------------------------------------------

   procedure Make_Buffer (Target : GL.Objects.Buffers.Buffer_Target;
                          Buffer : in out GL.Objects.Buffers.Buffer) is
   begin
      Buffer.Initialize_Id;
      Target.Bind (Buffer);
   end Make_Buffer;

   --  -------------------------------------------------------------------------

   procedure Setup_Buffers
     (Vertex_Buffer, Colour_Buffer : in out GL.Objects.Buffers.Buffer) is
      use GL.Types;
      use GL.Objects.Buffers;

   begin
      Make_Buffer (GL.Objects.Buffers.Array_Buffer, Vertex_Buffer);
      Utilities.Load_Vertex_Buffer
        (Array_Buffer, Cube_Data.Vertex_Data, Static_Draw);

      Make_Buffer (GL.Objects.Buffers.Array_Buffer, Colour_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Cube_Data.Colour_Data,
                                    Static_Draw);

      GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => 3,
                                               Kind   => GL.Types.Single_Type,
                                               Normalized => False,
                                               Stride => 0, Offset => 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);

      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, GL.Types.Single_Type,
                                               False, 0, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (1);
   exception
      when others =>
         Put_Line ("An exception occurred in Setup_Buffers.");
         raise;
   end Setup_Buffers;

   --  -------------------------------------------------------------------------

end Buffers_Manager;
