
with Maths;
with Utilities;

package body Buffers_Manager is

    procedure Create_Vertex_Buffers (VBO_1, VBO_2 : in out GL.Objects.Buffers.Buffer;
                                     Sphere_1, Sphere_2 : Sphere.Sphere) is
    use GL.Objects.Buffers;
        Vertices : Maths.Vector8_Array (1 .. Sphere.Get_Interleaved_Size (Sphere_1));
    begin
        VBO_1.Initialize_Id;
        Array_Buffer.Bind (VBO_1);
        Vertices := Sphere.Get_Interleaved_Vertices (Sphere_1);
        Utilities.Load_Vector8_Buffer (Array_Buffer, Vertices, Static_Draw);

        VBO_2.Initialize_Id;
        Array_Buffer.Bind (VBO_2);
        Vertices := Sphere.Get_Interleaved_Vertices (Sphere_2);
        Utilities.Load_Vector8_Buffer (Array_Buffer, Vertices, Static_Draw);
    end Create_Vertex_Buffers;

    --  ------------------------------------------------------------------------

  procedure Create_Index_Buffers (IBO_1, IBO_2 : in out GL.Objects.Buffers.Buffer;
                                  Sphere_1, Sphere_2 : Sphere.Sphere) is
    use GL.Objects.Buffers;
    use GL.Types;
        theIndices : Int_Array (1 .. Sphere.Get_Indices_Size (Sphere_1));
    begin
        IBO_1.Initialize_Id;
        Element_Array_Buffer.Bind (IBO_1);
        theIndices := Sphere.Get_Indices (Sphere_1);
        Utilities.Load_Element_Buffer (Element_Array_Buffer, theIndices, Static_Draw);

        IBO_2.Initialize_Id;
        Element_Array_Buffer.Bind (IBO_2);
        theIndices := Sphere.Get_Indices (Sphere_2);
        Utilities.Load_Element_Buffer (Element_Array_Buffer, theIndices, Static_Draw);

    end Create_Index_Buffers;

    --  ------------------------------------------------------------------------

 end Buffers_Manager;
