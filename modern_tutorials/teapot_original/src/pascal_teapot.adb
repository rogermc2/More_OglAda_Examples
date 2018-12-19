
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

package body Pascal_Teapot is
   use GL.Types;

   function U_Element (Patch : Teapot_Data.Bezier_Patch;
                       Index : Int; T : Single)
                       return Singles.Vector3;
   function V_Element (Patch : Teapot_Data.Bezier_Patch;
                       Index : Int; T : Single)
                       return Singles.Vector3;

   --  --------------------------------------------------------------------------------
   --  Blend_Vector calculates the vector cubic Bezier spline value at the parameter T
   function  Blend_Vector (D0, D1, D2, D3 : Singles.Vector3;
                           T : GL.Types.Single) return Singles.Vector3 is
       use GL;
       T_Cub  : Single := T ** 3;
       T1     : Single := 1.0 - T;
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
  --  Build_Curve generates the Num_Steps sehments of a spline
   procedure Build_Curve (D0, D1, D2, D3 : Singles.Vector3;
                          Num_Steps: Int;  Curve : out Singles.Vector3_Array) is
      Step : constant Single := 1.0 / Single (Num_Steps);
      T    : Single := 0.0;
   begin
      Curve (1) := D0;                   --  Start of spline
      for count in 1 .. Num_Steps loop   --  Build spline
         T := T + Step;
         Curve (count + 1) := Blend_Vector (D0, D1, D2, D3, T);
      end loop;

   exception
      when  others =>
         Put_Line ("An exception occurred in Pascal_Teapot.Build_Curve.");
         raise;
   end Build_Curve;

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

   procedure Build_Patch (Patch : Teapot_Data.Bezier_Patch; Num_Steps : Int;
                          Patch_Array : out Singles.Vector3_Array) is
      use Teapot_Data;
      D0, D1, D2, D3 : Singles.Vector3 := (0.0, 0.0, 0.0);
      Step        : constant Single := 1.0 / Single (Num_Steps);
      Index       : Int;
      Index2      : Int;
      T           : Single := 0.0;
      Curve       : Singles.Vector3_Array (1 .. Num_Steps + 1);
   begin
      for Step_Count in 1 ..Num_Steps loop
         Index := (Step_Count - 1) * 2 * (Num_Steps + 1) + 1;
         Index2 := Index + Num_Steps + 1;
         --  Splines of constant U
         D0 := U_Element (Patch, 1, T);
         D1 := U_Element (Patch, 2, T);
         D2 := U_Element (Patch, 3, T);
         D3 := U_Element (Patch, 4, T);
         Build_Curve (D0, D1, D2, D3, Num_Steps, Curve);
         Patch_Array (Index .. Index + Num_Steps) := Curve;
         --  Splines of constant V
         D0 := V_Element (Patch, 1, T);
         D1 := V_Element (Patch, 2, T);
         D2 := V_Element (Patch, 3, T);
         D3 := V_Element (Patch, 4, T);
         Build_Curve (D0, D1, D2, D3, Num_Steps, Curve);
         Patch_Array (Index2 .. Index2 + Num_Steps) := Curve;
         T := T + Step;
      end loop;

   exception
      when  others =>
         Put_Line ("An exception occurred in Pascal_Teapot.Build_Patch.");
         raise;
   end Build_Patch;

   --  --------------------------------------------------------------------------------

   procedure Build_Teapot (Patchs : Teapot_Data.Patch_Data;  Num_Steps : Int;
                          theTeapot : out Singles.Vector3_Array) is
      Patch_Array_Length : Int := 2 * (Num_Steps + 1) ** 2;
      aPatch    : Singles.Vector3_Array (1 .. Patch_Array_Length);
   begin
--        for Index in Patchs'Range loop
      for Index in Int range 1 .. 1 loop
--           Put_Line ("Pascal_Teapot.Build_Teapot Patch_Array_Length: " &
--                       Int'Image (Patch_Array_Length));
         Build_Patch (Patchs (Index), Num_Steps, aPatch);
         Utilities.Print_GL_Array3 ("aPatch", aPatch);
         New_Line;
         for Patch_Count in aPatch'Range loop
                theTeapot (Index + Patch_Count - 1) :=
                  aPatch (Patch_Count);
         end loop;
--           Utilities.Print_Vector ("aPatch (1)", aPatch (1));
--           Utilities.Print_Vector ("aPatch (2)", aPatch (2));
--           Utilities.Print_Vector ("aPatch (3)", aPatch (3));
--           Utilities.Print_Vector ("aPatch (4)", aPatch (4));
      end loop;

   exception
      when  others =>
         Put_Line ("An exception occurred in Pascal_Teapot.Build_Teapot.");
         raise;
   end Build_Teapot;

   --  --------------------------------------------------------------------------------

   function U_Element (Patch : Teapot_Data.Bezier_Patch; Index : Int; T : Single)
                       return Singles.Vector3 is
      use Teapot_Data;
   begin
      return Blend_Vector (Control_Points (Patch (Index, 1)),
                           Control_Points (Patch (Index, 2)),
                           Control_Points (Patch (Index, 3)),
                           Control_Points (Patch (Index, 4)), T);
    end U_Element;

   --------------------------------------------------------------------------------

   function V_Element (Patch : Teapot_Data.Bezier_Patch; Index : Int; T : Single)
                      return Singles.Vector3 is
      use Teapot_Data;
   begin
      return Blend_Vector (Control_Points (Patch (1, Index)),
                           Control_Points (Patch (2, Index)),
                           Control_Points (Patch (3, Index)),
                           Control_Points (Patch (4, Index)), T);
    end V_Element;

   --  --------------------------------------------------------------------------------

end Pascal_Teapot;
