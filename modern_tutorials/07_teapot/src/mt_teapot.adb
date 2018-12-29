
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

with Teacup_Maths; use Teacup_Maths;

package body MT_Teapot is

   subtype Vertex is GL.Types.Singles.Vector3;
   type Control_Point_Matrix is array
     (Int range 1 .. Teapot_Data.Bezier_Patch'Length,
      Int range 1 .. Teapot_Data.Bezier_Patch'Length (2)) of Vertex;

   function Compute_Position (Control_Points : Control_Point_Matrix; U, V : Single)
                               return Singles.Vector3;
   procedure Build_Vertices (Vertices : out Vertices_Array;
                             Colours  : out Colours_Array);
   procedure Get_Control_Points (Patch          : Int;
                                 Control_Points : out Control_Point_Matrix);
   procedure Print_Control_Points (Patch : Int; Points : Control_Point_Matrix);

   --  --------------------------------------------------------------------------------

   procedure Build_Elements (Elements : out Element_Array) is
      use GL.Types;
      N      : Int := 0;
      Res_UV : constant Int := Res_U * Res_V;
      PUV    : Int;
   begin
      --  1 square ABCD = 2 triangles ABC + CDA
      for Patch_Num in 0 .. Int (Teapot_Data.Patch_Data'Length - 1) loop
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
      Control_Points  : Control_Point_Matrix;
      Res_UV          : constant Int := Res_U * Res_V;
      U               : Single;
      V               : Single;
      Patch_Index     : Int := 1;
      PU_Index        : Int;
      Colour_P_Index  : Int := 1;
      Colour_PU_Index : Int := 1;
      Colour_Index    : Int;
   begin
      --  Evaluate the Bézier surface, with a resolution of 10x10.
      --  For each 4x4 patch, compute each point in the 10x10 grid
      --  with u and v progressing in 1/10 steps.
      for Patch_Num in 1 .. Int (Teapot_Data.Num_Patches) loop
         Get_Control_Points (Patch_Num, Control_Points);
         for Ru in 0 .. Res_U + 1  loop
            U := Single (Ru) / Single (Res_U - 1);
            PU_Index := Patch_Index + Ru * Res_V;
            Colour_Index := Colour_PU_Index;
            for Rv in 0 .. Res_V + 1 loop
               V := Single (Rv) / Single (Res_V - 1);
               Vertices (PU_Index + Rv) := Compute_Position (Control_Points, U, V);
               Colours (Colour_Index) :=
                 Single (Patch_Num) / Single (Teapot_Data.Num_Patches);
               Colours (Colour_Index + 1) :=
                 Single (Patch_Num) / Single (Teapot_Data.Num_Patches);
               Colours (Colour_Index + 2) := 0.8;
               Colour_Index := Colour_Index + 3;
            end loop;
            Colour_PU_Index := Colour_PU_Index + 3 * Res_V;
         end loop;
         Patch_Index := Patch_Index + Res_UV;
         Colour_P_Index := Colour_P_Index + 3 * Res_UV;
      end loop;

   exception
      when  others =>
         Put_Line ("An exception occurred in MT_Teapot.Build_Vertices.");
         raise;
   end Build_Vertices;

   --  --------------------------------------------------------------------------------

   function Compute_Position (Control_Points : Control_Point_Matrix; U, V : Single)
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

   procedure Get_Control_Points (Patch          : Int;
                                 Control_Points : out Control_Point_Matrix) is
      use GL.Types;
      thePatch : Teapot_Data.Bezier_Patch := Teapot_Data.Patch_Data (Patch);
   begin
      for Index_I in thePatch'Range loop
         for Index_J in Int range thePatch'Range (2) loop
            Control_Points (Index_I + 1, Index_J + 1) :=
              Teapot_Data.Control_Points (thePatch (Int (Index_I), Int (Index_J)));
         end loop;
      end loop;

   exception
      when  others =>
         Put_Line ("An exception occurred in MT_Teapot.Get_Control_Points.");
         raise;
   end Get_Control_Points;

   --  --------------------------------------------------------------------------------

 procedure Print_Control_Points (Patch : Int; Points : Control_Point_Matrix) is
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

end MT_Teapot;
