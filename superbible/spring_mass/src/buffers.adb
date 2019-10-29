
with Interfaces.C.Pointers;

with Ada.Text_IO; use  Ada.Text_IO;

with GL.Attributes;
with GL.Pixels;
with GL.Types; use GL.Types;

with Maths;
with Utilities;

--  with Vertex_Data;

package body Buffers is

    --     procedure Load_Vertex_Buffer is new GL.Objects.Buffers.Load_To_Buffer (Vertex_Data.pVertex_Pointers);
    --     procedure Load_Index_Buffer is new GL.Objects.Buffers.Load_To_Buffer (Vertex_Data.pIndex_Pointers);

    --  Buffer types
    Position_A           : constant Integer := 1;
    Position_B           : constant Integer := 2;
    Velocity_A           : constant Integer := 3;
    --     Velocity_B           : constant Integer := 4;
    Connection           : constant Integer := 5;

    Points_X             : constant Int := 50;
    Points_Y             : constant Int := 50;
    Total_Points         : constant Int :=  Points_X * Points_Y;
    Total_Connections    : constant Int
      := (Points_X - 1) * Points_Y + Points_X * (Points_Y - 1);

    --  -----------------------------------------------------------------------------------------------------------------------

    procedure Setup_Buffers (VBO : in out Buffer_Array;
                             Index_Buffer : in out GL.Objects.Buffers.Buffer;
                             Position_Tex_Buffer : in out Buffer_Array) is
        use GL.Objects.Buffers;
        use Maths.Single_Math_Functions;

        package Lines_Package is new Interfaces.C.Pointers
          (GL.Types.Size, UInt, UInt_Array, UInt'(others => <>));
        procedure Map_Lines is new
          GL.Objects.Buffers.Map_Range (Lines_Package);

        procedure Load_Connections_Buffer is new
          GL.Objects.Buffers.Load_To_Buffer (Ints.Vector4_Pointers);

        Num_X               : constant Single := Single (Points_X);
        Num_Y               : constant Single := Single (Points_Y);
        Num_Lines           : constant UInt := UInt (Total_Connections);
        Initial_Positions   : Singles.Vector4_Array (1 .. Total_Points);
        Initial_Velocities  : constant Singles.Vector3_Array (1 .. Total_Points)
          := (others => (0.0, 0.0, 0.0));
        Initial_Connections : Ints.Vector4_Array (1 .. Total_Points)
          := (others => (-1, -1, -1, -1));

        X_Value             : Single;
        Y_Value             : Single;
        Value               : Singles.Vector4;
        Initial_Index       : Int := 0;
        --        Stride              : constant GL.Types.Size := 5;
        Map_Access          : Map_Bits;
        Lines               : UInt_Array (1 .. 1);
        e                   : Lines_Package.Pointer;
    begin
        for index_Y in 1 .. Points_Y loop
            Y_Value := Single (index_Y) / Single (Points_Y);
            for index_X in 1 .. Points_X loop
                X_Value := Single (index_X) / Single (Points_X);
                Value (GL.X) := (X_Value - 0.5) * Num_X;
                Value (GL.Y) := (Y_Value - 0.5) * Num_Y;
                Value (GL.Z) := 0.6 * Sin (X_Value) * Cos (Y_Value);
                Value (GL.W) := 1.0;
                Initial_Positions (Initial_Index) := Value;
                if index_Y /= Points_Y - 1 then
                    if index_X /= 1 then
                        Initial_Connections (Initial_Index) (GL.X) := Initial_Index - 1;
                    end if;
                    if index_Y /= 1 then
                        Initial_Connections (Initial_Index) (GL.Y) := Initial_Index - Points_X;
                    end if;
                    if index_X /= Points_X - 1 then
                        Initial_Connections (Initial_Index) (GL.Z) := Initial_Index + 1;
                    end if;
                    Initial_Connections (Initial_Index) (GL.W) := Initial_Index + Points_X;
                end if;
                Initial_Index := Initial_Index + 1;
            end loop;
        end loop;

        for index in VBO'Range loop
            VBO (index).Initialize_Id;
        end loop;
        for index in VBO'Range loop
            Array_Buffer.Bind (VBO (Position_A + 1));
            Utilities.Load_Vertex_Buffer
              (Array_Buffer, Initial_Positions, Dynamic_Copy);
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (Index  => 0, Count  => 4, Kind  => Single_Type,
               Normalized => True, Stride => 0, Offset => 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (0);

            Array_Buffer.Bind (VBO (Velocity_A + 1));
            Utilities.Load_Vertex_Buffer
              (Array_Buffer, Initial_Velocities, Dynamic_Copy);
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (1, 3, Single_Type, True, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (1);

            Array_Buffer.Bind (VBO (Connection));
            Load_Connections_Buffer
              (Array_Buffer, Initial_Connections, Static_Draw);
            GL.Attributes.Set_Vertex_Attrib_Pointer
              (2, 4, Int_Type, True, 0, 0);
            GL.Attributes.Enable_Vertex_Attrib_Array (2);
        end loop;

        for index in Position_Tex_Buffer'Range loop
            Position_Tex_Buffer (index).Initialize_Id;
            GL.Objects.Buffers.Texture_Buffer.Bind (Position_Tex_Buffer (index));
            if index = 1 then
                GL.Objects.Buffers.Texture_Buffer.Allocate (GL.Pixels.RGBA32F, VBO (Position_A));
            else
                GL.Objects.Buffers.Texture_Buffer.Allocate (GL.Pixels.RGBA32F, VBO (Position_B));
            end if;
        end loop;

        Lines (1) := UInt (2 * Num_Lines * Int'Size / 8);
        Index_Buffer.Initialize_Id;
        Element_Array_Buffer.Bind (Index_Buffer);
        Utilities.Load_Element_Buffer (Element_Array_Buffer, Lines, Static_Draw);

        Map_Access.Write := True;
        Map_Access.Invalidate_Buffer := True;
        Map_Lines (Target => Element_Array_Buffer, Access_Type => Map_Access,
                   Offset => 0, Size => Int (2 * Num_Lines * Int'Size / 8),
                   Pointer => e);

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Buffers.Setup_Buffers.");
            raise;
    end Setup_Buffers;

    --  ------------------------------------------------------------------------

end Buffers;
