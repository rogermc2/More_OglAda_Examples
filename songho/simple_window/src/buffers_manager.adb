
--  with Utilities;

package body Buffers_Manager is

    procedure Create_Vertex_Buffer (VBO_1 : in out GL.Objects.Buffers.Buffer) is
    use GL.Objects.Buffers;
     begin
        VBO_1.Initialize_Id;
        Array_Buffer.Bind (VBO_1);
--          Utilities.Load_Vector8_Buffer
--            (Array_Buffer, Sphere.Get_Interleaved_Vertices (Sphere_1), Static_Draw);

    end Create_Vertex_Buffer;

    --  ------------------------------------------------------------------------

  procedure Create_Index_Buffer (IBO_1 : in out GL.Objects.Buffers.Buffer) is
    use GL.Objects.Buffers;
    begin
        IBO_1.Initialize_Id;
        Element_Array_Buffer.Bind (IBO_1);
--          Utilities.Load_Element_Buffer
--          (Element_Array_Buffer, Sphere.Get_Indices (Sphere_1), Static_Draw);

    end Create_Index_Buffer;

    --  ------------------------------------------------------------------------

    procedure Load_Vertex_Buffer (Vertex_Buffer : GL.Objects.Buffers.Buffer) is
    use GL.Objects.Buffers;
     begin
        Array_Buffer.Bind (Vertex_Buffer);
--          Utilities.Load_Vector8_Buffer
--            (Array_Buffer, Sphere.Get_Interleaved_Vertices (aSphere), Static_Draw);
    end Load_Vertex_Buffer;

    --  ------------------------------------------------------------------------

 end Buffers_Manager;
