
with Interfaces.C;
with Interfaces.C.Pointers;

with GL.Objects.Buffers;

Package body Billboard_List is
   use GL.Types;
   Num_Rows : constant Int := 10;
   Num_Cols : constant Int := 10;

   type Position_Vector is array (1 .. Num_Cols) of aliased Singles.Vector3;
   pragma Convention (C, Position_Vector);

   type Positions_Array is array (Size range <>) of aliased Position_Vector;
   pragma Convention (C, Positions_Array);

    package Position_Vector_Pointers is new Interfaces.C.Pointers
     (Size, Position_Vector, Positions_Array, Position_Vector'(others => <>));

    procedure Load_Positions_Buffer is new GL.Objects.Buffers.Load_To_Buffer
     (Position_Vector_Pointers);

--     type Positions_Array is array (1 .. Num_Rows * Num_Cols) of
--       aliased Singles.Vector3;
--     pragma Convention (C, Positions_Array);

   procedure Create_Position_Buffer (VBO : in out GL.Objects.Buffers.Buffer;
                                     VP  : GL.Types.Singles.Matrix4) is
      use GL.Objects.Buffers;
      Positions : Positions_Array (1 .. Num_Rows);
      Position  : Position_Vector;
   begin
      for Row in 1 .. Num_Rows loop
         for Col in 1 .. Num_Cols loop
            Position (Col) := (Single (Row), 0.0, Single (Col));
         end loop;
         Positions (Row) := Position;
      end loop;

      VBO.Initialize_Id;
      Array_Buffer.Bind (VBO);
      Load_Positions_Buffer (Array_Buffer, Positions, Static_Draw);
   end Create_Position_Buffer;

   --  ----------------------------------------------------------------------

   function Init (Tex_File_Name : String) return Boolean is
   begin
      return False;
   end Init;

   --  ----------------------------------------------------------------------

   procedure Render (VP : GL.Types.Singles.Matrix4) is
   begin
      null;
   end Render;

   --  ----------------------------------------------------------------------

end Billboard_List;
