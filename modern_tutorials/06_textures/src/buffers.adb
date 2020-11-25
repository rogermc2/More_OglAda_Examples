
with Interfaces.C.Pointers;

with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

package body Buffers is
   use GL.Types;

   --  ------------------------------------------------------------------------

   procedure Create_Vertex_Buffer (VBO      : in out GL.Objects.Buffers.Buffer;
                                   Vertices : Graph_Data.Point_Data) is
      use GL.Objects.Buffers;
   begin
      VBO.Initialize_Id;
      Array_Buffer.Bind (VBO);
      Utilities.Load_Vertex_Buffer
          (Array_Buffer,  Singles.Vector2_Array (Vertices), Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in BuffersCreate_Vertex_Buffers.");
         raise;
   end Create_Vertex_Buffer;

   --  ------------------------------------------------------------------------

   procedure Create_Elements_Buffer (IBO     : in out GL.Objects.Buffers.Buffer;
                                     Indices : GL.Types.Int_Array) is
      use GL.Objects.Buffers;
   begin
      IBO.Initialize_Id;
      Element_Array_Buffer.Bind (IBO);
      Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices, Static_Draw);
   exception
      when others =>
         Put_Line ("An exception occurred in Buffers.Create_Elements_Buffer.");
         raise;
   end Create_Elements_Buffer;

   --  ------------------------------------------------------------------------

end Buffers;
