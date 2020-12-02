
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
      use Maths.Single_Math_Functions;
      Graph : Vector2_Array (1 .. 2000);
      X     : Single;
   begin
      for index in Graph'Range loop
         X := (Single (index) - 1000.0) / 100.0;
         Graph (index) (GL.X) := X;
         Graph (index) (GL.Y) := Sin (10.0 * X) / (1.0 + X ** 2);
      end loop;

      VBO.Initialize_Id;
      Array_Buffer.Bind (VBO);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Graph, Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in BuffersCreate_Vertex_Buffer.");
         raise;
   end Create_Vertex_Buffer;

   --  ------------------------------------------------------------------------

end Buffers;
