
with Interfaces.C.Pointers;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with Maths;
with Utilities;

package body Buffers is

   --  ------------------------------------------------------------------------

   procedure Create_Vertex_Buffers
     (Data_VBO, Border_VBO, Ticks_VBO : in out GL.Objects.Buffers.Buffer) is
      use GL.Objects.Buffers;
      use GL.Types;
      use Singles;
      use Maths.Single_Math_Functions;
      Graph  : Singles.Vector2_Array (1 .. 2000);
      Border : constant Singles.Vector2_Array (1 .. 4) :=
                 ((-1.0, -1.0), (1.0, -1.0), (1.0, 1.0), (-1.0, 1.0));
      X      : Single;
   begin

      for index in Graph'Range loop
         X := Single (index - 1000) / 100.0;
         Graph (index) := (X, Sin (10.0 * X) / (1.0 + X ** 2));
      end loop;

      Data_VBO.Initialize_Id;
      Array_Buffer.Bind (Data_VBO);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Graph, Static_Draw);

      Border_VBO.Initialize_Id;
      Array_Buffer.Bind (Border_VBO);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Border, Static_Draw);

      Ticks_VBO.Initialize_Id;
      Array_Buffer.Bind (Ticks_VBO);

   exception
      when others =>
         Put_Line ("An exception occurred in Buffers.Create_Vertex_Buffers.");
         raise;
   end Create_Vertex_Buffers;

   --  ------------------------------------------------------------------------

end Buffers;
