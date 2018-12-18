
with Ada.Text_IO; use Ada.Text_IO;

package body Pascal_Teapot is
   use GL.Types;

   function U_Element (Patch : Teapot_Data.Bezier_Patch;
                       Index : Int; T : Single)
                       return Singles.Vector3;
   function V_Element (Patch : Teapot_Data.Bezier_Patch;
                       Index : Int; T : Single)
                       return Singles.Vector3;

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

   function Build_Curve (D0, D1, D2, D3 : Singles.Vector3;
                         Num_Steps      : Int)
                         return Singles.Vector3_Array is
      Step : constant Single := 1.0 / Single (Num_Steps);
      T    : Single := Step;
      Temp : Singles.Vector3;
      Curve : Singles.Vector3_Array (1 .. Num_Steps + 1);
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

   function Build_Patch (Patch : Teapot_Data.Bezier_Patch; Num_Steps : Int)
                         return Singles.Vector3_Array is
      use Teapot_Data;
      D0, D1, D2, D3 : Singles.Vector3;
      Step        : constant Single := 1.0 / Single (Num_Steps);
      Step_Count  : Int := 0;
      Index       : Int;
      T           : Single := 0.0;
      Patch_Array : Singles.Vector3_Array (1 .. 4 * (Num_Steps + 1));
   begin
      while T < 1.0 + Step / 2.0 loop
         Index := 4 * Step_Count;
         D0 := U_Element (Patch, 1, T);
         D1 := U_Element (Patch, 2, T);
         D2 := U_Element (Patch, 3, T);
         D3 := U_Element (Patch, 4, T);
         Patch_Array (1 .. Num_Steps + 1) := Build_Curve (D0, D1, D2, D3, Num_Steps);
         D0 := V_Element (Patch, 1, T);
         D1 := V_Element (Patch, 2, T);
         D2 := V_Element (Patch, 3, T);
         D3 := V_Element (Patch, 4, T);
         Patch_Array (Num_Steps + 2 .. Num_Steps + 2 + Num_Steps + 1) :=
           Build_Curve (D0, D1, D2, D3, Num_Steps) (1 .. Num_Steps + 1);

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

   function U_Element (Patch : Teapot_Data.Bezier_Patch;
                                 Index : Int; T : Single)
                                 return Singles.Vector3 is
      use Teapot_Data;
   begin
      return Blend_Vector (Control_Points (Patch (Index, 1)),
                           Control_Points (Patch (Index, 2)),
                           Control_Points (Patch (Index, 3)),
                           Control_Points (Patch (Index, 4)), T);
    end U_Element;

   --  --------------------------------------------------------------------------------

   function V_Element (Patch : Teapot_Data.Bezier_Patch;
                                 Index : Int; T : Single)
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
