
with Ada.Text_IO; use Ada.Text_IO;

with  GL.Objects.Buffers;
with  GL.Types;

with Utilities;

package body Load_Buffers is

    --      function To_Array2 (Array_In : Load_Obj_File.Obj_Array2)
    --                          return GL.Types.Singles.Vector2_Array;
    function To_Array3 (Vertices : Load_Obj_File.Obj_Array3)
                        return GL.Types.Singles.Vector3_Array;
    function To_UInt_Array (Array_In : Load_Obj_File.Obj_Int3_Array)
                            return GL.Types.UInt_Array;

    --  ----------------------------------------------
    --      procedure Load_Buffers
    --        (Path   : String; aModel : in out Model.Model_Data;
    --         Vertices, Normals : Load_Obj_File.Obj_Array3;
    --         UVs  : Load_Obj_File.Obj_Array2;
    --         Vertex_Indices, Normal_Indices,
    --         UV_Indices : Load_Obj_File.Obj_Int3_Array)

    procedure Load_Buffers
      (aModel : in out Model.Model_Data;
       Vertices : Load_Obj_File.Obj_Array3;
       Vertex_Indices : Load_Obj_File.Obj_Int3_Array) is
        use GL.Types;
        use GL.Objects.Buffers;
        use Model;

        Vertices_Array         : Singles.Vector3_Array (1 .. Int (Vertices.Length));
        Indexed_Vertices_Array : UInt_Array (1 .. 3 * Int (Vertex_Indices.Length));
    begin
        Vertices_Array := To_Array3 (Vertices);
        Indexed_Vertices_Array := To_UInt_Array (Vertex_Indices);

        Bind_Vertex_VBO (aModel);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices_Array, Static_Draw);

        Bind_Element_VBO (aModel);
        Utilities.Load_Element_Buffer
          (Element_Array_Buffer, Indexed_Vertices_Array, Static_Draw);

    exception
        when others =>
            Put_Line ("An exception occurred in Load_Buffers.Load_Buffers.");
            raise;
    end Load_Buffers;

    --  ------------------------------------------------------------------------

    function To_Array3 (Vertices : Load_Obj_File.Obj_Array3)
                        return GL.Types.Singles.Vector3_Array is
        use GL.Types;
        anArray : Singles.Vector3_Array
          (Int (Vertices.First_Index) .. Int (Vertices.Last_Index));
    begin
        for index in Vertices.First_Index .. Vertices.Last_Index loop
            anArray (Int (index)) := Vertices.Element (index);
        end loop;
        return anArray;
    end To_Array3;

    --  ------------------------------------------------------------------------

    --      function To_Array2 (Array_In : Load_Obj_File.Obj_Array2)
    --                          return GL.Types.Singles.Vector2_Array is
    --          use GL.Types;
    --          anArray : Singles.Vector2_Array
    --            (Int (Array_In.First_Index) .. Int (Array_In.Last_Index));
    --      begin
    --          for index in Array_In.First_Index .. Array_In.Last_Index loop
    --              anArray (Int (index)) := Array_In.Element (index);
    --          end loop;
    --          return anArray;
    --      end To_Array2;

    --  ------------------------------------------------------------------------

    function To_UInt_Array (Array_In : Load_Obj_File.Obj_Int3_Array)
                            return GL.Types.UInt_Array is
        use GL.Types;
        anArray : UInt_Array
          (Int (Array_In.First_Index) .. 3 * Int (Array_In.Last_Index));
        Value   : Ints.Vector3;
    begin
        for index in Array_In.First_Index .. Array_In.Last_Index loop
            Value := Array_In.Element (index);
            for u_index in 1 .. 3 loop
                case u_index is
                    when 1 =>
                        anArray (Int (3 * (index - 1) + u_index)) :=
                          UInt (Value (GL.X));
                    when 2 =>
                        anArray (Int (3 * (index - 1) + u_index)) :=
                          UInt (Value (GL.Y));
                    when 3 =>
                        anArray (Int (3 * (index - 1) + u_index)) :=
                          UInt (Value (GL.Z));
                end case;
            end loop;
        end loop;
        return anArray;
    end To_UInt_Array;

    --  ------------------------------------------------------------------------

end Load_Buffers;
