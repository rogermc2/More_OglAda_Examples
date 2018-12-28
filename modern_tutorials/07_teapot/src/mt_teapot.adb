
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

with Teacup_Maths; use Teacup_Maths;

package body MT_Teapot is

   subtype Vertex is GL.Types.Singles.Vector3;
   type Control_Point_Array is array
     (Int range 1 .. Teapot_Data.Bezier_Patch'Length,
      Int range 1 .. Teapot_Data.Bezier_Patch'Length (2)) of Vertex;

   function Compute_Position (Control_Points : Control_Point_Array; U, V : Single)
                               return Singles.Vector3;
   procedure Build_Vertices (Vertices : out Vertices_Array;
                             Colours  : out Colours_Array);
   procedure Print_Control_Points (Patch : Int; Points : Control_Point_Array);
   procedure Print_CP_Elements_Array (Patch_Num   : Int;
                                      CP_Elements : Patch_Element_Array);

   --  --------------------------------------------------------------------------------

   procedure Build_Control_Points (Patch          : Int;
                                   Control_Points : out Control_Point_Array) is
      use GL.Types;
      thePatch : Teapot_Data.Bezier_Patch := Teapot_Data.Patchs (Patch);
   begin
      for Index_I in Int range 1 .. thePatch'Length loop
         for Index_J in Int range 1 .. thePatch'Length (2) loop
            Control_Points (Index_I, Index_J) :=
              Teapot_Data.Control_Points
                (thePatch (Int (Index_I), Int (Index_J)));
         end loop;
      end loop;

   exception
      when  others =>
         Put_Line ("An exception occurred in MT_Teapot.Build_Control_Points.");
         raise;
   end Build_Control_Points;

   --  --------------------------------------------------------------------------------

   --  For debugging
   procedure Build_CP_Colours (CP_Colours : out CP_Colours_Array) is
   begin
      for Index in CP_Colours'First .. CP_Colours'Last loop
               CP_Colours (Index) :=  (1.0, 0.0, 0.0);  --  Red
      end loop;

   exception
      when  others =>
         Put_Line ("An exception occurred in MT_Teapot.Build_CP_Colours.");
         raise;
   end Build_CP_Colours;

   --  --------------------------------------------------------------------------------

   --  For debugging
   procedure Build_CP_Elements (CP_Elements : out Patch_Element_Array) is
      Patch : Teapot_Data.Bezier_Patch;
   begin
      for Patch_Num in Teapot_Data.Patchs'First .. Teapot_Data.Patchs'Last loop
         Patch := Teapot_Data.Patchs (Patch_Num);
         for I in Int range 1 .. Patch'Length loop
            for J in Int range 1 .. Patch'Length (2) loop
               CP_Elements (Patch_Num) (I, J) :=  Patch (I, J);
            end loop;
         end loop;
         Print_CP_Elements_Array (Patch_Num, CP_Elements);
      end loop;

   exception
      when  others =>
         Put_Line ("An exception occurred in MT_Teapot.Build_CP_Elements.");
         raise;
   end Build_CP_Elements;

   --  --------------------------------------------------------------------------------

   procedure Build_Elements (Elements : out Element_Array) is
      use GL.Types;
      N      : Int := 0;
      Res_UV : constant Int := Res_U * Res_V;
      PUV    : Int;
   begin
      --  1 square ABCD = 2 triangles ABC + CDA
      for Patch_Num in 0 .. Int (Teapot_Data.Patchs'Length - 1) loop
         PUV := Patch_Num * Res_UV;
         for Ru in 0 .. Res_U - 1 loop
            for Rv in 0 .. Res_V - 1 loop
               --  ABC
               N := N + 1;
               Elements (N) := PUV + Ru * Res_V + rv;
               N := N + 1;
               Elements (N) := PUV + Ru * Res_V + rv + 1;
               N := N + 1;
               Elements (N) := PUV + (Ru + 1) * Res_V + rv + 1;
               --  CDA
               N := N + 1;
               Elements (N) := PUV + (Ru + 1) * Res_V + rv + 1;
               N := N + 1;
               Elements (N) := PUV + (Ru + 1) * Res_V + rv;
               N := N + 1;
               Elements (N) := PUV + Ru * Res_V + rv;
            end loop;
         end loop;
      end loop;
--        Utilities.Print_GL_Int_Array ("MT_Teapot.Build_Elements Elements", Elements);
      Put_Line ("MT_Teapot.Build_Elements, last N" & Int'Image (N));

   exception
      when  others =>
         Put_Line ("An exception occurred in MT_Teapot.Build_Elements.");
         raise;
   end Build_Elements;

   --  --------------------------------------------------------------------------------

   procedure Build_Teapot (Vertices : out Vertices_Array;
                           Colours  : out Colours_Array;
                           Elements : out Element_Array) is
   begin
      Build_Vertices (Vertices, Colours);
      Build_Elements (Elements);
   exception
      when  others =>
         Put_Line ("An exception occurred in MT_Teapot.Build_Teapot.");
         raise;
   end Build_Teapot;

   --  --------------------------------------------------------------------------------

   procedure Build_Vertices (Vertices : out Vertices_Array;
                             Colours  : out Colours_Array) is
      Control_Points : Control_Point_Array;
      Res_UV         : constant Int := Res_U * Res_V;
      U              : Single;
      V              : Single;
      Patch_Part     : Int;
      PU_Part        : Int;
      Colour_P_Part  : Int;
      Colour_PU_Part : Int;
   begin
      --  Evaluate the Bézier surface, with a resolution of 10x10.
      --  For each 4x4 patch, compute each point in the 10x10 grid
      --  with u and v progressing in 1/10 steps.
--        for Patch_Count in 0 .. Int (Teapot_Data.Patchs'Length - 1) loop
      for Patch_Count in Int range 1 .. 1 loop
         Build_Control_Points (Patch_Count, Control_Points);
         Print_Control_Points (Patch_Count, Control_Points);

         Patch_Part := Patch_Count * Res_UV;
         Colour_P_Part := 3 * Patch_Count * Res_UV;
         for Ru in 0 .. Res_U - 1 loop
            U := Single (Ru) / Single (Res_U - 1);
            PU_Part := Patch_Part + Ru * Res_V;
            Colour_PU_Part := Colour_P_Part + 3 * Ru * Res_V;
            for Rv in 0 .. Res_V - 1 loop
               V := Single (Rv) / Single (Res_V - 1);
--                 Put ("MT_Teapot.Build_Vertices, PU_Part + Rv " &
--                        Int'Image (PU_Part + Rv) & "  ");
               Vertices (PU_Part + Rv) :=
                Compute_Position (Control_Points, U, V);
--                   Utilities.Print_Vector ("Position", Vertices (PU_Part + Rv));
               Colours (Colour_PU_Part + 3 * rv) :=
                 Single (Patch_Count) / Single (Teapot_Data.Num_Patchs);
               Colours (Colour_PU_Part+ 3 * rv + 1) :=
                 Single (Patch_Count) / Single (Teapot_Data.Num_Patchs);
               Colours (Colour_PU_Part + 3 * rv + 2) := 0.8;
            end loop;
         end loop;
      end loop;
--        Utilities.Print_GL_Array3 ("MT_Teapot.Build_Vertices Vertices", Singles.Vector3_Array (Vertices));

   exception
      when  others =>
         Put_Line ("An exception occurred in MT_Teapot.Build_Vertices.");
         raise;
   end Build_Vertices;

   --  --------------------------------------------------------------------------------

   function Compute_Position (Control_Points : Control_Point_Array; U, V : Single)
                              return Singles.Vector3 is
      Order      : constant Int := Teapot_Data.Bezier_Patch'Length - 1;
      Position   : Singles.Vector3 := (0.0, 0.0, 0.0);
      Poly_I     : Single;
      Poly_J     : Single;
      Poly_IJ    : Single;
      I_Index    : Int;
      J_Index    : Int;
   begin
      for I in Int range 0 .. Order loop
         I_Index := I + 1;
         Poly_I := Bernstein_Polynomial (I, Order, U);
         for J in Int range 0 .. Order loop
            J_Index := J + 1;
            Poly_J := Bernstein_Polynomial (J, Order, V);
            Poly_IJ := Poly_I * Poly_J;
            Position (GL.X) := Position (GL.X) +
              Poly_IJ * Control_Points (I_Index, J_Index) (GL.X);
            Position (GL.Y) := Position (GL.Y) +
              Poly_IJ * Control_Points (I_Index, J_Index) (GL.Y);
            Position (GL.Z) := Position (GL.Z) +
              Poly_IJ * Control_Points (I_Index, J_Index) (GL.Z);
         end loop;
      end loop;
      return Position;

   exception
      when  others =>
         Put_Line ("An exception occurred in MT_Teapot.Compute_Position.");
         raise;
   end Compute_Position;

   --  --------------------------------------------------------------------------------

 procedure Print_Control_Points (Patch : Int; Points : Control_Point_Array) is
   begin
      Put_Line ("Control Points for patch:" & Int'Image (Patch));
      for Row in Int range 1 .. Points'Length loop
         for Column in Int range 1 .. Points'Length (2) loop
                Put ("Row, Column:" & Int'Image (Row) & "  " & Int'Image (Column));
            Utilities.Print_Vector ("", Points (Row, Column));
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Control_Points;

   --  ------------------------------------------------------------------------

   procedure Print_CP_Elements_Array (Patch_Num   : Int;
                                      CP_Elements : Patch_Element_Array) is
   begin
      Put_Line ("Elements for patch:" & Int'Image (Patch_Num));
      for Row in Int range 1 .. CP_Elements (Patch_Num)'Length loop
         for Column in Int range 1 .. CP_Elements (Patch_Num)'Length (2) loop
            Put_Line ("Row, Column:" & Int'Image (Row) & " " &
                        Int'Image (Column) & " " &
                        Int'Image (CP_Elements (Patch_Num) (Row, Column)));
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_CP_Elements_Array;

   --  ------------------------------------------------------------------------

end MT_Teapot;
