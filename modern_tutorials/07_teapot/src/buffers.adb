
with Interfaces.C.Pointers;

with Ada.Text_IO; use Ada.Text_IO;

with Maths;
with Utilities;

package body Buffers is
   use GL.Types;

   type FLat_CP_Element_Array_Type is array (GL.Types.Int range <>) of
     aliased GL.Types.Int;
   type Flattend_Array is new FLat_CP_Element_Array_Type
      (1 .. CP_Element_Array'Length * Order * Order);

   package CP_Element_Pointers is new Interfaces.C.Pointers
     (GL.Types.Int, GL.Types.Int, FLat_CP_Element_Array_Type, GL.Types.Int'Last);
   procedure Load_CP_Element_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (CP_Element_Pointers);

   function Flatten (anArray : CP_Element_Array) return FLat_CP_Element_Array_Type;

   --  ------------------------------------------------------------------------

    procedure Create_CP_Index_Buffer (IBO : in out GL.Objects.Buffers.Buffer;
                                      Indices : CP_Element_Array) is
      use GL.Objects.Buffers;
      use GL.Types;
      Flat_Array : FLat_CP_Element_Array_Type
      (1 .. CP_Element_Array'Size * Order * Order) := Flatten (Indices);
   begin
      IBO.Initialize_Id;
      Element_Array_Buffer.Bind (IBO);
      Load_CP_Element_Buffer (Element_Array_Buffer, Flat_Array, Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in Create_Index_Buffer.");
         raise;
   end Create_CP_Index_Buffer;

   --  ------------------------------------------------------------------------

   procedure Create_Vertex_Buffer (VBO      : in out GL.Objects.Buffers.Buffer;
                                   Vertices : GL.Types.Singles.Vector3_Array) is
      use GL.Objects.Buffers;
   begin
      VBO.Initialize_Id;
      Array_Buffer.Bind (VBO);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in Create_Vertex_Buffers.");
         raise;
   end Create_Vertex_Buffer;

   --  ------------------------------------------------------------------------

   procedure Create_Index_Buffer (IBO     : in out GL.Objects.Buffers.Buffer;
                                  Indices : GL.Types.Int_Array) is
      use GL.Objects.Buffers;
      use GL.Types;
   begin
      IBO.Initialize_Id;
      Element_Array_Buffer.Bind (IBO);
      Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices, Static_Draw);
   exception
      when others =>
         Put_Line ("An exception occurred in Create_Index_Buffer.");
         raise;
   end Create_Index_Buffer;

   --  ------------------------------------------------------------------------

   function Flatten (anArray : CP_Element_Array) return FLat_CP_Element_Array_Type is
      use GL.Types;
      Flat    : FLat_CP_Element_Array_Type (1 .. CP_Element_Array'Length);
      F_Index : Int := 0;
   begin
      for I1 in Int range 1 .. CP_Element_Array'Size loop
         F_Index := Order ** 2 * (I1 - 1) + 1;
         for I2 in Int range 1 .. Order loop
            F_Index := F_Index + Order * (I2 - 1);
            for I3 in Int range 1 .. Order loop
               F_Index := F_Index + (I3 - 1);
               Flat (F_Index) := anArray (I1, I2, I3);
            end loop;
         end loop;
      end loop;
      return Flat;
   end Flatten;

   --  ------------------------------------------------------------------------

end Buffers;
