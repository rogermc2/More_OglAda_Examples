
with Interfaces.C.Pointers;

with Ada.Text_IO; use  Ada.Text_IO;

with GL.Attributes;
with GL.Pixels;

with Maths;
with Utilities;

package body Buffers is
   use GL.Types;

   package Element_Buffer_Package is new Interfaces.C.Pointers
     (GL.Types.Size, Int, Int_Array, Int'Last);
   procedure Map_Element_Buffer_Range is new
     GL.Objects.Buffers.Map_Range (Element_Buffer_Package);

   procedure Load_Connections_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (Ints.Vector4_Pointers);

   Points_X        : constant Int := 50;
   Points_Y        : constant Int := 50;
   Num_Points      : constant Int :=  Points_X * Points_Y;
   Num_Connections : constant Int
     := (Points_X - 1) * Points_Y + Points_X * (Points_Y - 1);

   --  -----------------------------------------------------------------------------------------------------------------------

   procedure Initialize_Vertex_Data (Initial_Positions   : in out Singles.Vector4_Array;
                                     Initial_Connections : in out Ints.Vector4_Array) is
      use Maths.Single_Math_Functions;
      Num_X               : constant Single := Single (Points_X);
      Num_Y               : constant Single := Single (Points_Y);
      X_Value             : Single;
      Y_Value             : Single;
      Value               : Singles.Vector4;
      Initial_Index       : Int := 1;
   begin
      for index in Initial_Connections'Range loop
         Initial_Connections (index) := (-1, -1, -1, -1);
      end loop;

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

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Buffers.Initialize_Vertex_Data.");
         raise;
   end Initialize_Vertex_Data;

   --  ----------------------------------------------------------------------------------

   procedure Setup_Index_Buffer (Index_Buffer : in out GL.Objects.Buffers.Buffer) is
      use GL.Objects.Buffers;
      Num_Lines         : constant Int := Num_Connections;
      Buffer_Size       : constant GL.Types.Size :=
                            GL.Types.Size (2 * Num_Lines * Int'Size / 8);
      Map_Access        : Map_Bits;
      Mapped_Buffer_Ptr : Element_Buffer_Package.Pointer;  -- int * e
   begin
      Index_Buffer.Initialize_Id;
      Element_Array_Buffer.Bind (Index_Buffer);

      Map_Access.Write := True;
      Map_Access.Invalidate_Buffer := True;

      Element_Array_Buffer.Allocate (Long (Buffer_Size), Static_Draw);
      Map_Element_Buffer_Range (Target      => Element_Array_Buffer,
                                Access_Type => Map_Access,
                                Offset      => 0, Size => Buffer_Size,
                                Pointer     => Mapped_Buffer_Ptr);

      for index_Y in 1 .. Points_Y loop
         for index_X in 1 .. Points_X - 1 loop
            Element_Buffer_Package.Increment (Mapped_Buffer_Ptr);
            Mapped_Buffer_Ptr.all := Int (index_X - 1 + (index_Y - 1) * Points_X);
            Element_Buffer_Package.Increment (Mapped_Buffer_Ptr);
            Mapped_Buffer_Ptr.all := Int (1 + (index_Y - 1) * Points_X);
         end loop;
      end loop;

      for index_X in 1 .. Points_X loop
         for index_Y in 1 .. Points_Y - 1 loop
            Element_Buffer_Package.Increment (Mapped_Buffer_Ptr);
            Mapped_Buffer_Ptr.all := Int (index_X - 1 + (index_Y - 1) * Points_X);
            Element_Buffer_Package.Increment (Mapped_Buffer_Ptr);
            Mapped_Buffer_Ptr.all := Int (Points_X + (index_X - 1) + (index_Y - 1) * Points_X);
         end loop;
      end loop;
      Unmap (Element_Array_Buffer);

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Buffers.Setup_Index_Buffer.");
         raise;
   end Setup_Index_Buffer;

   --  ----------------------------------------------------------------------------------

   procedure Setup_Tex_Buffers (Position_Tex_Buffer : in out Buffer_Array;
                                VBO                 : in out Buffer_Array) is
      use GL.Objects.Buffers;
   begin
      for index in Position_Tex_Buffer'Range loop
         Position_Tex_Buffer (index).Initialize_Id;
         Texture_Buffer.Bind (Position_Tex_Buffer (index));
         if index = 1 then
            Texture_Buffer.Allocate (GL.Pixels.RGBA32F, VBO (Position_A));
         else
            GL.Objects.Buffers.Texture_Buffer.Allocate (GL.Pixels.RGBA32F, VBO (Position_B));
         end if;
      end loop;

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Buffers.Setup_Tex_Buffers.");
         raise;
   end Setup_Tex_Buffers;

   --  ----------------------------------------------------------------------------------

   procedure Setup_Vertex_Buffers (VBO : in out Buffer_Array) is
      use GL.Objects.Buffers;
      Initial_Positions   : Singles.Vector4_Array (1 .. Num_Points);
      Initial_Velocities  : constant Singles.Vector3_Array (1 .. Num_Points)
        := (others => (0.0, 0.0, 0.0));
      Initial_Connections : Ints.Vector4_Array (1 .. Num_Points);
      --        Stride              : constant GL.Types.Size := 5;
   begin
      Initialize_Vertex_Data (Initial_Positions, Initial_Connections);

      for index in VBO'Range loop
         VBO (index).Initialize_Id;
         Array_Buffer.Bind (VBO (index));
      end loop;

      for index in 0 .. 1 loop
         Array_Buffer.Bind (VBO (Position_A + index));
         Utilities.Load_Vertex_Buffer
           (Array_Buffer, Initial_Positions, Dynamic_Copy);
         GL.Attributes.Set_Vertex_Attrib_Pointer
           (Index      => 0, Count  => 4, Kind  => Single_Type,
            Normalized => True, Stride => 0, Offset => 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (0);

         Array_Buffer.Bind (VBO (Velocity_A + index));
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

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Buffers.Setup_Vertex_Buffers.");
         raise;
   end Setup_Vertex_Buffers;

   --  ----------------------------------------------------------------------------------

   procedure Setup_Buffers (VBO_Array            : in out Buffer_Array;
                            Index_Buffer         : in out GL.Objects.Buffers.Buffer;
                            Position_Tex_Buffers : in out Buffer_Array) is
   begin
      Setup_Vertex_Buffers (VBO_Array);
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
