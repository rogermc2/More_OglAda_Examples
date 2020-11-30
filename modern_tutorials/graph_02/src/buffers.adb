
with Interfaces.C.Pointers;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with Maths;
with Utilities;

package body Buffers is

   --  ------------------------------------------------------------------------

   procedure Create_Vertex_Buffer (VBO : in out GL.Objects.Buffers.Buffer) is
      use GL.Objects.Buffers;
      use GL.Types;
      use Singles;
      Line  : Single_Array (1 .. 101);
   begin

      for x_index in Line'Range loop
         Line (x_index) := Single (x_index - 51) / 50.0;
      end loop;

      VBO.Initialize_Id;
      Array_Buffer.Bind (VBO);
      Utilities.Load_Singles_Buffer (Array_Buffer, Line, Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in Buffers.Create_Vertex_Buffer.");
         raise;
   end Create_Vertex_Buffer;

   --  ------------------------------------------------------------------------

end Buffers;
