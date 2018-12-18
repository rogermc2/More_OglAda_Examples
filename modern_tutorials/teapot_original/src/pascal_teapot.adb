
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

package body Pascal_Teapot is
   use GL.Types;

   subtype Vertex is GL.Types.Singles.Vector3;
   type Control_Point_Array is array
     (Int range 1 .. Teapot_Data.Bezier_Patch'Length,
      Int range 1 .. Teapot_Data.Bezier_Patch'Length (2)) of Vertex;

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
