
with Ada.Text_IO; use Ada.Text_IO;

with Maths;
with Utilities;

package body Buffers is

    use GL.Types.Singles;
    type Vertex is record
        Position : Vector3;
        Texture  : Vector2;
        Normal   : Vector3 := (0.0, 0.0, 0.0);
    end record;

    type Vertices_Array is array (GL.Types.UInt range <>) of Vertex;

    procedure Create_Index_Buffer (IBO : in out GL.Objects.Buffers.Buffer;
                                   Indices : out GL.Types.UInt_Array);
    procedure Create_Vertex_Buffer (Vertex_Buffer, UVs_Buffer,
                                    Normals_Buffer : in out GL.Objects.Buffers.Buffer;
                                    Indices : GL.Types.UInt_Array);

   --  ------------------------------------------------------------------------

   procedure Calculate_Normals (Vertices : GL.Types.Singles.Vector3_Array;
                                Normals  : in out GL.Types.Singles.Vector3_Array;
                                Indices  : GL.Types.UInt_Array) is
       use GL.Types;
       use GL.Types.Singles;
       use Maths;
       Index  : Int := 1;
       Index1 : Int;
       Index2 : Int;
       Index3 : Int;
       V1     : Singles.Vector3;
       V2     : Singles.Vector3;
       Normal : Singles.Vector3;
   begin
        for index in Int range  1 .. Normals'Length loop
            Normals (index) := (0.0, 0.0, 0.0);
        end loop;
        for count in 1 .. Vertices'Length loop
            Index1 := Int (Indices (Index)) + 1;
            Index2 := Int (Indices (Index + 1)) + 1;
            Index3 := Int (Indices (Index + 2)) + 1;

            V1 := Vertices (Index2) - Vertices (Index1);
            V2 := Vertices (Index3) - Vertices (Index2);
            Normal := Normalized (Singles.Cross_Product (V1, V2));

            Normals (Index1) := Normals (Index1) + Normal;
            Normals (Index2) := Normals (Index2) + Normal;
            Normals (Index3) := Normals (Index3) + Normal;
            Index := Index + 3;
        end loop;
        New_Line;
        for index in Int range  1 .. Vertices'Length loop
            Normals (index) := Normalized (Normals (index));
        end loop;
   exception
      when others =>
         Put_Line ("An exception occurred in Calculate_Normals.");
         raise;
   end Calculate_Normals;

   --  ------------------------------------------------------------------------

    procedure Create_Buffers (Vertex_Buffer, UVs_Buffer, Normals_Buffer, IBO : in out GL.Objects.Buffers.Buffer) is
        Indices : GL.Types.UInt_Array (1 .. 12);
    begin
        Create_Index_Buffer (IBO, Indices);
        Create_Vertex_Buffer (Vertex_Buffer, UVs_Buffer, Normals_Buffer, Indices);
    end Create_Buffers;

   --  ------------------------------------------------------------------------

    procedure Create_Vertex_Buffer (Vertex_Buffer, UVs_Buffer, Normals_Buffer : in out GL.Objects.Buffers.Buffer;
                                    Indices : GL.Types.UInt_Array) is
    use GL.Objects.Buffers;
    use GL.Types;
--          Vertices : Vertices_Array (1 .. 4);
        Vertex_Data  : constant Singles.Vector3_Array (1 .. 4) :=
        ((-1.0, -1.0, 0.5773),
         (0.0, -1.0, -1.15475),
         (1.0, -1.0, 0.5773),
         (0.0, 1.0, 0.0));
        Texture_Data : constant Singles.Vector2_Array (1 .. 4) :=
        ((0.0, 0.0),
         (0.5, 0.0),
         (1.0, 0.0),
         (0.5, 1.0));
        Normal_Data  : Singles.Vector3_Array (1 .. 4);
    begin
        Calculate_Normals (Vertex_Data, Normal_Data, Indices);

         Vertex_Buffer.Initialize_Id;
         Array_Buffer.Bind (Vertex_Buffer);
         Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data, Static_Draw);

         UVs_Buffer.Initialize_Id;
         Array_Buffer.Bind (UVs_Buffer);
         Utilities.Load_Vertex_Buffer (Array_Buffer, Texture_Data, Static_Draw);

         Normals_Buffer.Initialize_Id;
         Array_Buffer.Bind (Normals_Buffer);
         Utilities.Load_Vertex_Buffer (Array_Buffer, Normal_Data, Static_Draw);
    exception
      when others =>
         Put_Line ("An exception occurred in Create_Vertex_Buffer.");
         raise;
    end Create_Vertex_Buffer;

    --  ------------------------------------------------------------------------

  procedure Create_Index_Buffer (IBO : in out GL.Objects.Buffers.Buffer;
                                 Indices : out GL.Types.UInt_Array) is
    use GL.Objects.Buffers;
    use GL.Types;
    begin
        Indices := (0, 3, 1,
                    1, 3, 2,
                    2, 3, 0,
                    0, 1, 2);
        IBO.Initialize_Id;
        Element_Array_Buffer.Bind (IBO);
        Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices, Static_Draw);
    exception
      when others =>
         Put_Line ("An exception occurred in Create_Index_Buffer.");
         raise;
    end Create_Index_Buffer;

    --  ------------------------------------------------------------------------

 end Buffers;
