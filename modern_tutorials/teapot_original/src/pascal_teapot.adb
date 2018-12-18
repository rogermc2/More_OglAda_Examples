
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

with Teapot_Maths; use Teapot_Maths;

package body Pascal_Teapot is
   use GL.Types;

   subtype Vertex is GL.Types.Singles.Vector3;
   type Control_Point_Array is array
     (Int range 1 .. Teapot_Data.Bezier_Patch'Length,
      Int range 1 .. Teapot_Data.Bezier_Patch'Length (2)) of Vertex;

   function Compute_Position (Control_Points : Control_Point_Array; U, V : Single)
                               return GL.Types.Singles.Vector3;
   procedure Build_Vertices (Vertices : out Vertices_Array;
                             Colours  : out Colours_Array);
   procedure Print_Control_Points (Patch : Int; Points : Control_Point_Array);

   --  --------------------------------------------------------------------------------

   function  Blend_Vector (D0, D1, D2, D3 : Singles.Vector3;
                           T : GL.Types.Single) return Singles.Vector3 is
       use GL;
       T1     : Single := 1.0 - T;
       T_Cub  : Single := T ** 3;
       T1_Cub : Single := (1.0 - T) ** 3;
       T1_Sq  : Single := (1.0 - T) ** 2;
       T3     : Single := 3.0 * T;
      T3_Sq   : Single := 3.0 * T * T;
      Result  : Singles.Vector3;
   begin
        Result (X) :=  T1_Cub * D0 (X) + T3 * T1_SQ * D1 (X) +
          T3_Sq * T1 * D2 (X) + T_Cub * D3 (X);
        Result (Y) :=  T1_Cub * D0 (Y) + T3 * T1_SQ * D1 (Y) +
          T3_Sq * T1 * D2 (Y) + T_Cub * D3 (Y);
        Result (Z) :=  T1_Cub * D0 (Z) + T3 * T1_SQ * D1 (Z) +
        T3_Sq * T1 * D2 (Z) + T_Cub * D3 (Z);
      return Result;
   end Blend_Vector;

  --  --------------------------------------------------------------------------------

   procedure Build_Control_Points (Patch          : Int;
                                   Control_Points : out Control_Point_Array) is
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
         Put_Line ("An exception occurred in Pascal_Teapot.Build_Control_Points.");
         raise;
   end Build_Control_Points;

   --  --------------------------------------------------------------------------------

   function Build_Curve (D0, D1, D2, D3 : Singles.Vector3;
                         Num_Steps      : Positive)
                         return Singles.Vector3_Array is
      Step : constant Single := 1.0 / Single (Num_Steps);
      T    : Single := Step;
      Temp : Singles.Vector3;
      Curve : Singles.Vector3_Array (1 .. int (Num_Steps + 1));
   begin
      Curve (1) := D0;
      while T < 1.0 + Step / 2.0 loop
         Temp := Blend_Vector (D0, D1, D2, D3, T);
         T := T + Step;
      end loop;
      return Curve;

   exception
      when  others =>
         Put_Line ("An exception occurred in Pascal_Teapot.Build_Curve.");
         raise;
   end Build_Curve;

   --  --------------------------------------------------------------------------------

   procedure Build_Elements (Elements : out Element_Array) is
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
      Put_Line ("Pascal_Teapot.Build_Elements, last N" & Int'Image (N));

   exception
      when  others =>
         Put_Line ("An exception occurred in Pascal_Teapot.Build_Elements.");
         raise;
   end Build_Elements;

   --  --------------------------------------------------------------------------------

   procedure Build_CP_Colours (CP_Colours : out CP_Colours_Array) is
   begin
      for Index in CP_Colours'First .. CP_Colours'Last loop
               CP_Colours (Index) :=  0.0;
      end loop;

   exception
      when  others =>
         Put_Line ("An exception occurred in Pascal_Teapot.Build_CP_Colours.");
         raise;
   end Build_CP_Colours;

   --  --------------------------------------------------------------------------------

   procedure Build_CP_Elements (CP_Elements : out Patch_Element_Array) is
      Patch : Teapot_Data.Bezier_Patch;
   begin
      for Patch_Num in Teapot_Data.Patchs'First .. Teapot_Data.Patchs'Last loop
         Patch := Teapot_Data.Patchs (Patch_Num);
         for I in Int range 1 .. Patch'Length loop
            for J in Int range 1 .. Patch'Length (2) loop
               CP_Elements (Patch_Num) (I, J) :=  Patch (I, J) - 1;
            end loop;
         end loop;

      end loop;

   exception
      when  others =>
         Put_Line ("An exception occurred in Pascal_Teapot.Build_CP_Elements.");
         raise;
   end Build_CP_Elements;

   --  --------------------------------------------------------------------------------

   function Build_Patch (Patch : Teapot_Data.Bezier_Patch; Num_Steps : Int)
                         return Singles.Vector3_Array is
      use Teapot_Data;
      Step        : constant Single := 1.0 / Single (Num_Steps);
      Step_Count  : Int := 0;
      Index       : Int;
      T           : Single := 0.0;
      Patch_Array : Singles.Vector3_Array (1 .. 4 * Int (Num_Steps + 1));
   begin
      while T < 1.0 + Step / 2.0 loop
         Index := 4 * Step_Count;
         Patch_Array (Index + 1) := Blend_Vector (Control_Points (Patch (1, 1)),
                                                  Control_Points (Patch (1, 2)),
                                                  Control_Points (Patch (1, 3)),
                                                  Control_Points (Patch (1, 4)), T);
         Patch_Array (Index + 2) := Blend_Vector (Control_Points (Patch (2, 1)),
                                                  Control_Points (Patch (2, 2)),
                                                  Control_Points (Patch (2, 3)),
                                                  Control_Points (Patch (2, 4)), T);
         Patch_Array (Index + 3) := Blend_Vector (Control_Points (Patch (3, 1)),
                                                  Control_Points (Patch (3, 2)),
                                                  Control_Points (Patch (3, 3)),
                                                  Control_Points (Patch (3, 4)), T);
         Patch_Array (Index + 4) := Blend_Vector (Control_Points (Patch (4, 1)),
                                                  Control_Points (Patch (4, 2)),
                                                  Control_Points (Patch (4, 3)),
                                                  Control_Points (Patch (4, 4)), T);
         T := T + Step;
         Step_Count := Step_Count + 1;
      end loop;

      return Patch_Array;

   exception
      when  others =>
         Put_Line ("An exception occurred in Pascal_Teapot.Build_Patch.");
         raise;
   end Build_Patch;

   --  --------------------------------------------------------------------------------

   function Build_Teapot (Patchs : Teapot_Data.Patch_Data;  Num_Steps : Int)
                          return Singles.Vector3_Array is
      Patch_Array_Length : Int := Int (4 * (Num_Steps + 1));
      theTeapot : Singles.Vector3_Array (1 .. Patchs'Length * Patch_Array_Length);
      aPatch    : Singles.Vector3_Array (1 .. Patch_Array_Length);
   begin
      for Index in Patchs'Range loop
         aPatch := Build_Patch (Patchs (Index), Num_Steps);
         for Patch_Count in aPatch'Range loop
         theTeapot (Index + Patch_Count - 1) :=
              aPatch (Patch_Count);
         end loop;
      end loop;
      return theTeapot;

   exception
      when  others =>
         Put_Line ("An exception occurred in Pascal_Teapot.Build_Teapot.");
         raise;
   end Build_Teapot;

   --  --------------------------------------------------------------------------------

--     procedure Build_Teapot (Vertices : out Vertices_Array;
--                             Colours  : out Colours_Array;
--                             Elements : out Element_Array) is
--     begin
--        Build_Vertices (Vertices, Colours);
--        Build_Elements (Elements);
--     exception
--        when  others =>
--           Put_Line ("An exception occurred in Pascal_Teapot.Build_Teapot.");
--           raise;
--     end Build_Teapot;

   --  --------------------------------------------------------------------------------

   procedure Build_Vertices (Vertices : out Vertices_Array;
                             Colours  : out Colours_Array) is
      use GL.Types;
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
      for Patch_Count in 0 .. Int (Teapot_Data.Patchs'Length - 1) loop
         Build_Control_Points (Patch_Count + 1, Control_Points);
         Print_Control_Points (Patch_Count + 1, Control_Points);

         Patch_Part := 1 + Patch_Count * Res_UV;
         Colour_P_Part := 1 + 3 * Patch_Count * Res_UV;
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
      Utilities.Print_GL_Array3 ("Pascal_Teapot.Build_Vertices Vertices", Singles.Vector3_Array (Vertices));

   exception
      when  others =>
         Put_Line ("An exception occurred in Pascal_Teapot.Build_Vertices.");
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
         Put_Line ("An exception occurred in Pascal_Teapot.Compute_Position.");
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

 procedure Print_CP_Elements_Array (CP_Elements : Patch_Element_Array; Patch_Num : Int) is
   begin
      Put_Line ("Elements for patch:" & Int'Image (Patch_Num));
      for Row in Int range 1 .. CP_Elements (Patch_Num)'Length loop
         for Column in Int range 1 .. CP_Elements (Patch_Num)'Length (2) loop
                Put ("Row, Column:" & Int'Image (Row) & "  " & Int'Image (Column) &
            Int'Image (CP_Elements (Patch_Num) (Row, Column)));
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_CP_Elements_Array;

   --  ------------------------------------------------------------------------

end Pascal_Teapot;
