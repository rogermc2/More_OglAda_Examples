
with Maths;
with Utilities;

package body Buffers is

    procedure Create_Vertex_Buffer (VBO : in out GL.Objects.Buffers.Buffer) is
    use GL.Objects.Buffers;
    use GL.Types;
        Vertices : constant GL.Types.Singles.Vector3_Array :=
         ((-1.0, -1.0, 0.5773),
           (0.0, -1.0, -1.15475),
           (1.0, -1.0, 0.5773),
           (0.0,  1.0, 0.0));
    begin
        VBO.Initialize_Id;
        Array_Buffer.Bind (VBO);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);
    end Create_Vertex_Buffer;

    --  ------------------------------------------------------------------------

  procedure Create_Index_Buffer (IBO : in out GL.Objects.Buffers.Buffer) is
    use GL.Objects.Buffers;
    use GL.Types;
        Indices : constant UInt_Array := (0, 3, 1,
                                          1, 3, 2,
                                          2, 3, 0,
                                          0, 1, 2);
    begin
        IBO.Initialize_Id;
        Element_Array_Buffer.Bind (IBO);
        Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices, Static_Draw);
    end Create_Index_Buffer;

    --  ------------------------------------------------------------------------


 end Buffers;
