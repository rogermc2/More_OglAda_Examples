
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

package body Buffers is
   use GL.Types;

   procedure Create_CP_Colour_Buffer (CP_Colour_Buffer : in out GL.Objects.Buffers.Buffer;
                                      CP_Colours : Pascal_Teapot.CP_Colours_Array) is
      use GL.Objects.Buffers;
   begin
      CP_Colour_Buffer.Initialize_Id;
      Array_Buffer.Bind (CP_Colour_Buffer);
      Utilities.Load_Singles_Buffer
        (Array_Buffer, GL.Types.Single_Array (CP_Colours), Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in CP Buffers.Create_Colour_Buffer.");
         raise;
   end Create_CP_Colour_Buffer;

   --  ------------------------------------------------------------------------

   procedure Create_Vertex_Buffer (VBO      : in out GL.Objects.Buffers.Buffer;
                                   Vertices : GL.Types.Singles.Vector3_Array) is
      use GL.Objects.Buffers;
   begin
      VBO.Initialize_Id;
      Array_Buffer.Bind (VBO);
      Utilities.Load_Vertex_Buffer
          (Array_Buffer, Vertices, Static_Draw);
      Utilities.Print_GL_Array3 ("Create_Vertex_Buffer, Vertices", Vertices);

   exception
      when others =>
         Put_Line ("An exception occurred in Buffers.Create_Vertex_Buffers.");
         raise;
   end Create_Vertex_Buffer;

   --  ------------------------------------------------------------------------

end Buffers;
