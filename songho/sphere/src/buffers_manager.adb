
with Maths;
with Utilities;

package body Buffers_Manager is

    procedure Create_Vertex_Buffers (VBO_1, VBO_2 : in out GL.Objects.Buffers.Buffer;
                                     Sphere_1, Sphere_2 : Sphere.Sphere) is
    use GL.Objects.Buffers;
        Vertices_1 : Maths.Vector8_Array (1 .. Sphere.Get_Interleaved_Size (Sphere_1));
        Vertices_2 : Maths.Vector8_Array (1 .. Sphere.Get_Interleaved_Size (Sphere_2));
    begin
        VBO_1.Initialize_Id;
        Array_Buffer.Bind (VBO_1);
        Vertices_1 := Sphere.Get_Interleaved_Vertices (Sphere_1);
        Utilities.Load_Vector8_Buffer (Array_Buffer, Vertices_1, Static_Draw);

        VBO_2.Initialize_Id;
        Array_Buffer.Bind (VBO_2);
        Vertices_2 := Sphere.Get_Interleaved_Vertices (Sphere_2);
        Utilities.Load_Vector8_Buffer (Array_Buffer, Vertices_2, Static_Draw);
    end Create_Vertex_Buffers;

    --  ------------------------------------------------------------------------

  procedure Create_Index_Buffers (IBO_1, IBO_2 : in out GL.Objects.Buffers.Buffer;
                                  Sphere_1, Sphere_2 : Sphere.Sphere) is
    use GL.Objects.Buffers;
    use GL.Types;
        Indices_1 : Int_Array (1 .. Sphere.Get_Indices_Size (Sphere_1));
        Indices_2 : Int_Array (1 .. Sphere.Get_Indices_Size (Sphere_2));
    begin
        IBO_1.Initialize_Id;
        Element_Array_Buffer.Bind (IBO_1);
        Indices_1 := Sphere.Get_Indices (Sphere_1);
        Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices_1, Static_Draw);

        IBO_2.Initialize_Id;
        Element_Array_Buffer.Bind (IBO_2);
        Indices_2 := Sphere.Get_Indices (Sphere_2);
        Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices_2, Static_Draw);

    end Create_Index_Buffers;

    --  ------------------------------------------------------------------------

 end Buffers_Manager;
