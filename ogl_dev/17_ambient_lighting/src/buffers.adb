
with Ada.Text_IO; use Ada.Text_IO;

with Maths;
with Utilities;

package body Buffers is

   use GL.Types.Singles;

    procedure Create_Index_Buffer (Index_Buffer : in out GL.Objects.Buffers.Buffer;
                                   Indices : out GL.Types.UInt_Array);
    procedure Create_Vertex_Buffer (Vertex_Buffer : in out GL.Objects.Buffers.Buffer;
                                    Indices : GL.Types.UInt_Array);

   --  ------------------------------------------------------------------------

    procedure Create_Buffers (Vertex_Buffer, Index_Buffer : in out GL.Objects.Buffers.Buffer) is
        Indices : GL.Types.UInt_Array (1 .. 12);
    begin
        Create_Index_Buffer (Index_Buffer, Indices);
        Create_Vertex_Buffer (Vertex_Buffer, Indices);
    end Create_Buffers;

   --  ------------------------------------------------------------------------

    procedure Create_Vertex_Buffer (Vertex_Buffer : in out GL.Objects.Buffers.Buffer;
                                    Indices : GL.Types.UInt_Array) is
    use GL.Objects.Buffers;
    use GL.Types;
        Vertex_Data  : constant Maths.Vector5_Array  (1 .. 4) :=
        ((-1.0, -1.0, 0.5773,  0.0, 0.0),
         (0.0, -1.0, -1.15475, 0.5, 0.0),
         (1.0, -1.0, 0.5773,   1.0, 0.0),
         (0.0, 1.0, 0.0,       0.5, 1.0));
    begin
         Vertex_Buffer.Initialize_Id;
         Array_Buffer.Bind (Vertex_Buffer);
         Utilities.Load_Vector5_Buffer (Array_Buffer, Vertex_Data, Static_Draw);

    exception
      when others =>
         Put_Line ("An exception occurred in Create_Vertex_Buffer.");
         raise;
    end Create_Vertex_Buffer;

    --  ------------------------------------------------------------------------

  procedure Create_Index_Buffer (Index_Buffer : in out GL.Objects.Buffers.Buffer;
                                 Indices : out GL.Types.UInt_Array) is
    use GL.Objects.Buffers;
    use GL.Types;
    begin
        Indices := (0, 3, 1,
                    1, 3, 2,
                    2, 3, 0,
                    1, 2, 0);
        Index_Buffer.Initialize_Id;
        Element_Array_Buffer.Bind (Index_Buffer);
        Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices, Static_Draw);
    exception
      when others =>
         Put_Line ("An exception occurred in Create_Index_Buffer.");
         raise;
    end Create_Index_Buffer;

    --  ------------------------------------------------------------------------

 end Buffers;
