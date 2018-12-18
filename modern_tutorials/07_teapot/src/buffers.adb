
with Interfaces.C.Pointers;

with Ada.Text_IO; use Ada.Text_IO;

with Maths;
with Utilities;

with Teacup_Maths;

package body Buffers is
   use GL.Types;

   Flattend_Array_Length : constant Int
      := MT_Teapot.Patch_Element_Array'Length * MT_Teapot.CP_Element_Array'Length *
         Teapot_Data.Bezier_Patch'Length * Teapot_Data.Bezier_Patch'Length (2);
   type FLat_CP_Element_Array_Type is array (GL.Types.Int range <>) of
     aliased GL.Types.Int;
   type Flattend_Array is new FLat_CP_Element_Array_Type (1 .. Flattend_Array_Length);

   package CP_Element_Pointers is new Interfaces.C.Pointers
     (Int, Int, FLat_CP_Element_Array_Type, Int'Last);
   procedure Load_CP_Element_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (CP_Element_Pointers);

   package CP_Vertex_Pointers is new Interfaces.C.Pointers
     (Size, Singles.Vector3, Singles.Vector3_Array, Singles.Vector3'(others => <>));
   procedure Load_CP_Element_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (CP_Vertex_Pointers);

   function Flatten (anArray : MT_Teapot.Patch_Element_Array)
                     return FLat_CP_Element_Array_Type;

   --  ------------------------------------------------------------------------

   procedure Create_Colour_Buffer (Colour_Buffer : in out GL.Objects.Buffers.Buffer;
                                    Colours : MT_Teapot.Colours_Array) is
      use GL.Objects.Buffers;
   begin
      Colour_Buffer.Initialize_Id;
      Array_Buffer.Bind (Colour_Buffer);
      Utilities.Load_Singles_Buffer (Array_Buffer, GL.Types.Single_Array (Colours), Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in Buffers.Create_Colour_Buffer.");
         raise;
   end Create_Colour_Buffer;

   --  ------------------------------------------------------------------------

   procedure Create_CP_Colour_Buffer (CP_Colour_Buffer : in out GL.Objects.Buffers.Buffer;
                                      CP_Colours : MT_Teapot.CP_Colours_Array) is
      use GL.Objects.Buffers;
   begin
      CP_Colour_Buffer.Initialize_Id;
      Array_Buffer.Bind (CP_Colour_Buffer);
      Utilities.Load_Vertex_Buffer
        (Array_Buffer, GL.Types.Singles.Vector3_Array(CP_Colours), Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in CP Buffers.Create_CP_Colour_Buffer.");
         raise;
   end Create_CP_Colour_Buffer;

   --  ------------------------------------------------------------------------

   procedure Create_CP_Elements_Buffer (CP_IBO : in out GL.Objects.Buffers.Buffer;
                                        CP_Indices : MT_Teapot.Patch_Element_Array) is
      use GL.Objects.Buffers;
      Flat_Array : FLat_CP_Element_Array_Type (1 .. Flattend_Array_Length)
        := Flatten (CP_Indices);
   begin
      CP_IBO.Initialize_Id;
      Element_Array_Buffer.Bind (CP_IBO);
      Load_CP_Element_Buffer (Element_Array_Buffer, Flat_Array, Static_Draw);
--        Utilities.Print_GL_Int_Array
--          ("Buffers.Create_CP_Elements_Buffer CP_Element_Array", Int_Array (Flat_Array));
   exception
      when others =>
         Put_Line ("An exception occurred in Buffers.Create_CP_Elements_Buffer.");
         raise;
   end Create_CP_Elements_Buffer;

   --  ------------------------------------------------------------------------

   procedure Create_CP_Vertex_Buffer (CP_VBO : in out GL.Objects.Buffers.Buffer;
                                   CP_Vertices : Teapot_Data.CP_Data) is
      use GL.Objects.Buffers;
   begin
      CP_VBO.Initialize_Id;
      Array_Buffer.Bind (CP_VBO);
      Utilities.Load_Vertex_Buffer
        (Array_Buffer, GL.Types.Singles.Vector3_Array (CP_Vertices), Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in Buffers.Create_CP_Vertex_Buffer.");
         raise;
   end Create_CP_Vertex_Buffer;

   --  ------------------------------------------------------------------------

   procedure Create_Vertex_Buffer (VBO      : in out GL.Objects.Buffers.Buffer;
                                   Vertices : MT_Teapot.Vertices_Array) is
      use GL.Objects.Buffers;
   begin
      VBO.Initialize_Id;
      Array_Buffer.Bind (VBO);
      Utilities.Load_Vertex_Buffer
          (Array_Buffer,  Singles.Vector3_Array (Vertices), Static_Draw);

   exception
      when others =>
         Put_Line ("An exception occurred in BuffersCreate_Vertex_Buffers.");
         raise;
   end Create_Vertex_Buffer;

   --  ------------------------------------------------------------------------

   procedure Create_Elements_Buffer (IBO     : in out GL.Objects.Buffers.Buffer;
                                     Indices : GL.Types.Int_Array) is
      use GL.Objects.Buffers;
   begin
      IBO.Initialize_Id;
      Element_Array_Buffer.Bind (IBO);
      Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices, Static_Draw);
   exception
      when others =>
         Put_Line ("An exception occurred in Buffers.Create_Elements_Buffer.");
         raise;
   end Create_Elements_Buffer;

   --  ------------------------------------------------------------------------

   function Flatten (anArray : MT_Teapot.Patch_Element_Array)
                     return FLat_CP_Element_Array_Type is
      Order      : constant Int := Teapot_Data.Order;
      Flat       : FLat_CP_Element_Array_Type (1 .. Flattend_Array_Length);
      Elem_Index : Int;
      Ord1_Index : Int;
   begin
      for Elem_Count in Int range 1 .. anArray'Length loop
         Elem_Index := (Elem_Count - 1) * Order * Order;

         for Ord1_Count in Int range 1 .. Order loop
            Ord1_Index := Elem_Index + (Ord1_Count - 1) * Order + 1;

            for Ord2_Count in Int range 1 .. Order loop
               Flat (Ord1_Index + (Ord2_Count - 1)) := anArray (Elem_Count) (Ord1_Count, Ord2_Count);
            end loop;
         end loop;
      end loop;
      return Flat;

   exception
      when others =>
         Put_Line ("An exception occurred in Buffers.Flatten.");
         raise;
   end Flatten;

   --  ------------------------------------------------------------------------

end Buffers;