
with Interfaces.C.Pointers;

with Ada.Text_IO; use Ada.Text_IO;

with Maths;
with Utilities;

with Teacup_Maths;

package body Buffers is
   use GL.Types;

   --  ------------------------------------------------------------------------

   procedure Create_Colour_Buffer (Colour_Buffer : in out GL.Objects.Buffers.Buffer;
                                    Colours : MT_Teapot.Colours_Array) is
      use GL.Objects.Buffers;
   begin
      Colour_Buffer.Initialize_Id;
      Array_Buffer.Bind (Colour_Buffer);
      Utilities.Load_Singles_Buffer (Array_Buffer, GL.Types.Single_Array (Colours), Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in Buffers.Create_Colour_Buffer.");
         raise;
   end Create_Colour_Buffer;

   --  ------------------------------------------------------------------------


   procedure Create_Vertex_Buffer (VBO      : in out GL.Objects.Buffers.Buffer;
                                   Vertices : MT_Teapot.Vertices_Array) is
      use GL.Objects.Buffers;
   begin
      VBO.Initialize_Id;
      Array_Buffer.Bind (VBO);
      Utilities.Load_Vertex_Buffer
          (Array_Buffer,  Singles.Vector3_Array (Vertices), Static_Draw);

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
