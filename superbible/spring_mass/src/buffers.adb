
with Interfaces.C.Pointers;

with Ada.Text_IO; use  Ada.Text_IO;

with GL.Attributes;
with GL.Pixels;

--  with Maths;
with Utilities;

package body Buffers is
   use GL.Types;

   package Element_Buffer_Package is new Interfaces.C.Pointers
     (GL.Types.Size, Int, Int_Array, Int'Last);
   procedure Map_Element_Buffer_Range is new
     GL.Objects.Buffers.Map_Range (Element_Buffer_Package);

   procedure Load_Connections_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (Ints.Vector4_Pointers);

   Points_X        : constant Int := 6;
   Points_Y        : constant Int := 6;
   Num_Points      : constant Int :=  Points_X * Points_Y;
   Num_Connections : constant Int
     := (Points_X - 1) * Points_Y + Points_X * (Points_Y - 1);

   --  -----------------------------------------------------------------------------------------------------------------------

   procedure Print_Ints_Vector (Name : String; aVector : GL.Types.Ints.Vector4) is
   begin
      if Name = "" then
         Put ("  ");
      else
         Put (Name & ":  ");
      end if;
      for Index in aVector'Range loop
         Put (GL.Types.Int'Image (aVector (Index)) & "   ");
      end loop;
      New_Line;
   end Print_Ints_Vector;

   --  -------------------------------------------------------------------

   procedure Print_GL_Array4 (Name : String; anArray : GL.Types.Ints.Vector4_Array) is
   begin
      Put_Line (Name & ": ");
      for Index in anArray'First .. anArray'Last loop
         Print_Ints_Vector ("", anArray (Index));
      end loop;
      New_Line;
   end Print_GL_Array4;

   --  ------------------------------------------------------------------------

   procedure Initialize_Vertex_Data (Initial_Positions : out Singles.Vector4_Array;
                                     Connections : out Ints.Vector4_Array) is
--        use Maths.Single_Math_Functions;
      Num_X         : constant Single := Single (Points_X);
      Num_Y         : constant Single := Single (Points_Y);
      X_Value       : Single;
      Y_Value       : Single;
      Value         : Singles.Vector4;
      Vector_Index  : Int range 0 .. Initial_Positions'Last := 0;
   begin
      for index in Connections'Range loop
         Connections (index) := (-1, -1, -1, -1);
      end loop;

      for index_Y in 0 .. Points_Y - 1 loop
         Y_Value := Single (index_Y) / Num_Y;
         for index_X in 0 .. Points_X - 1 loop
            X_Value := Single (index_X) / Num_X;
            Value (GL.X) := 10.0 * (X_Value - 0.5) * Num_X;
            Value (GL.Y) := 10.0 * (Y_Value - 0.5) * Num_Y;
            Value (GL.Z) := 0.6;
--              Value (GL.Z) := 0.6 * Sin (X_Value) * Cos (Y_Value);
            Value (GL.W) := 1.0;

            Vector_Index := Vector_Index + 1;
            Initial_Positions (Vector_Index) := Value;

            if index_X > 0 then
               --  not at bottom row and not at start of row; leave start (X) at -1
               --  otherwise, set X to n - 1 (previous item)
               Connections (Vector_Index) (GL.X) := Vector_Index - 2;
            end if;
            if index_Y > 0 then
               --  not in either first or last row; leave top (Y) at -1
               --  otherwise, set Y to n - Points_X (item in previous row)
               Connections (Vector_Index) (GL.Y) := Vector_Index - 1 - Points_X;
            end if;
            if index_X < Points_X - 1 then
               --  not at bottom row and not not at end of row; leave end (X) at -1
               --  otherwise, set Z to n + 1 (next item)
               Connections (Vector_Index) (GL.Z) := Vector_Index;
            end if;
            if index_Y < Points_Y - 1 then
               --  anywhere except last row
               --  set W to n + Points_X (item in next row)
               Connections (Vector_Index) (GL.W) := Vector_Index - 1 + Points_X;
            end if;
         end loop;
      end loop;
      Print_GL_Array4 ("Initial_Connections", Connections);

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Buffers.Initialize_Vertex_Data.");
         raise;
   end Initialize_Vertex_Data;

   --  ----------------------------------------------------------------------------------

   procedure Setup_Index_Buffer (Index_Buffer : out GL.Objects.Buffers.Buffer) is
      use GL.Objects.Buffers;
      Num_Lines         : constant Int := Num_Connections;
      Buffer_Size       : constant GL.Types.Size :=
                            GL.Types.Size (2 * Num_Lines * Int'Size / 8);
      Map_Access        : Map_Bits;
      Mapped_Buffer_Ptr : Element_Buffer_Package.Pointer;  -- int * e
   begin
      Map_Access.Write := True;
      Map_Access.Invalidate_Buffer := True;

      Index_Buffer.Initialize_Id;
      Element_Array_Buffer.Bind (Index_Buffer);

      Element_Array_Buffer.Allocate (Long (Buffer_Size), Static_Draw);
      Map_Element_Buffer_Range (Target      => Element_Array_Buffer,
                                Access_Type => Map_Access,
                                Offset      => 0, Size => Buffer_Size,
                                Pointer     => Mapped_Buffer_Ptr);

      for index_Y in 0 .. Points_Y - 1 loop
         for index_X in 0 .. Points_X - 2 loop
            Mapped_Buffer_Ptr.all := Int (index_X + index_Y * Points_X);
            Element_Buffer_Package.Increment (Mapped_Buffer_Ptr);
            Mapped_Buffer_Ptr.all := Int (1 + index_Y * Points_X);
            if (index_Y /= Points_Y - 1) and (index_X <  Points_X - 2) then
               Element_Buffer_Package.Increment (Mapped_Buffer_Ptr);
            end if;
         end loop;
      end loop;

      for index_X in 0 .. Points_X - 1 loop
         for index_Y in 0 .. Points_Y - 2 loop
            Mapped_Buffer_Ptr.all :=
              Int (index_X  + index_Y * Points_X);
            Element_Buffer_Package.Increment (Mapped_Buffer_Ptr);
            Mapped_Buffer_Ptr.all :=
              Int (Points_X + index_X  + index_Y * Points_X);
            if (index_X /= Points_X - 1) and (index_Y < Points_Y - 2) then
               Element_Buffer_Package.Increment (Mapped_Buffer_Ptr);
            end if;
         end loop;
      end loop;
      Unmap (Element_Array_Buffer);

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Buffers.Setup_Index_Buffer.");
         raise;
   end Setup_Index_Buffer;

   --  ----------------------------------------------------------------------------------

   procedure Setup_Tex_Buffers (Position_Tex_Buffer : out Buffer_Array;
                                VBO                 : Buffer_Array) is
      use GL.Objects.Buffers;
   begin
      --  Attach the vertex buffers to a pair of texture buffers
      for index in Position_Tex_Buffer'Range loop
         Position_Tex_Buffer (index).Initialize_Id;
         Texture_Buffer.Bind (Position_Tex_Buffer (index));
         --  Attach the data store of a specified VBO to a Position_Tex_Buffer
         if index = Position_Tex_Buffer'First then
            Texture_Buffer.Allocate (GL.Pixels.RGBA32F, VBO (Position_A));
         else
            Texture_Buffer.Allocate (GL.Pixels.RGBA32F, VBO (Position_B));
         end if;
      end loop;

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Buffers.Setup_Tex_Buffers.");
         raise;
   end Setup_Tex_Buffers;

   --  ----------------------------------------------------------------------------------

   procedure Setup_Vertex_Buffers (VAO : Vertex_Buffer_Array;
                                   VBO : out Buffer_Array) is
      use GL.Objects.Buffers;
      Initial_Positions   : Singles.Vector4_Array (1 .. Num_Points);
      Initial_Velocities  : constant Singles.Vector3_Array (1 .. Num_Points)
        := (others => (0.0, 0.0, 0.0));
      Connections         : Ints.Vector4_Array (1 .. Num_Points);
      Stride              : constant GL.Types.Size := 0;  --  11?
   begin
      Initialize_Vertex_Data (Initial_Positions, Connections);
      for index in VBO'Range loop
         VBO (index).Initialize_Id;
         Array_Buffer.Bind (VBO (index));
      end loop;

      for index in 0 .. 1 loop
         VAO (index + 1).Bind;

         Array_Buffer.Bind (VBO (Position_A + index));
         Utilities.Load_Vertex_Buffer
           (Array_Buffer, Initial_Positions, Dynamic_Copy);
         GL.Attributes.Set_Vertex_Attrib_Pointer
           (Index      => 0, Count  => 4, Kind  => Single_Type,
            Normalized => False, Stride => Stride, Offset => 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (0);

         Array_Buffer.Bind (VBO (Velocity_A + index));
         Utilities.Load_Vertex_Buffer
           (Array_Buffer, Initial_Velocities, Dynamic_Copy);
         GL.Attributes.Set_Vertex_Attrib_Pointer
           (1, 3, Single_Type, False, Stride, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (1);

         Array_Buffer.Bind (VBO (Connection));
         Load_Connections_Buffer (Array_Buffer, Connections, Static_Draw);
         GL.Attributes.Set_Vertex_Attrib_Pointer
           (2, 4, Int_Type, False, Stride, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (2);
      end loop;

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Buffers.Setup_Vertex_Buffers.");
         raise;
   end Setup_Vertex_Buffers;

   --  ----------------------------------------------------------------------------------

   procedure Setup_Buffers (VAO_Array            : Vertex_Buffer_Array;
                            VBO_Array            : out Buffer_Array;
                            Index_Buffer         : out GL.Objects.Buffers.Buffer;
                            Position_Tex_Buffers : out Buffer_Array) is
   begin
      Setup_Vertex_Buffers (VAO_Array, VBO_Array);
      Setup_Tex_Buffers (Position_Tex_Buffers, VBO_Array);
      Setup_Index_Buffer (Index_Buffer);
   end Setup_Buffers;

   --  ----------------------------------------------------------------------------------

   function Total_Connections return GL.Types.Int is
   begin
      return Num_Connections;
   end Total_Connections;

   --  -----------------------------------------------------------------------------------------------------------------------

   function Total_Points return GL.Types.Int is
   begin
      return Num_Points;
   end Total_Points;

   --  -----------------------------------------------------------------------------------------------------------------------

end Buffers;
