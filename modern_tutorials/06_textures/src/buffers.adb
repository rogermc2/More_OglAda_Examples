
with Interfaces.C.Pointers;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Types; use GL.Types;

with Utilities;

with Cube_Data; use Cube_Data;

package body Buffers is

   --  ------------------------------------------------------------------------

   procedure Create_Elements_Buffer (IBO : in out GL.Objects.Buffers.Buffer) is
      use GL.Objects.Buffers;
   begin
      IBO.Initialize_Id;
      Element_Array_Buffer.Bind (IBO);
      Utilities.Load_Element_Buffer (Element_Array_Buffer, Elements, Static_Draw);
   exception
      when others =>
         Put_Line ("An exception occurred in Buffers.Create_Elements_Buffer.");
         raise;
   end Create_Elements_Buffer;

   --  ------------------------------------------------------------------------

   procedure Create_Tex_Coords_Buffer (VBO : in out GL.Objects.Buffers.Buffer) is
      use GL.Objects.Buffers;
   begin
      VBO.Initialize_Id;
      Array_Buffer.Bind (VBO);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Texture_Coords, Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in Buffers.Create_Vertex_Buffers.");
         raise;
   end Create_Tex_Coords_Buffer;

   --  ------------------------------------------------------------------------

   procedure Create_Vertex_Buffer (VBO : in out GL.Objects.Buffers.Buffer) is
      use GL.Objects.Buffers;
   begin
      VBO.Initialize_Id;
      Array_Buffer.Bind (VBO);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in Buffers.Create_Vertex_Buffers.");
         raise;
   end Create_Vertex_Buffer;

   --  ------------------------------------------------------------------------

end Buffers;
