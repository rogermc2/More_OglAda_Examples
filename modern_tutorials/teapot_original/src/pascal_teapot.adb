
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

package body Pascal_Teapot is
   use GL.Types;

   procedure Print_Patch (Name     : String;
                          thePatch : Teapot_Data.Patch_Type);
   function U_Coord (Patch : Teapot_Data.Patch_Type;
                     Index : Int; T : Single) return Singles.Vector3;
   function V_Cord (Patch : Teapot_Data.Patch_Type;
                    Index : Int; T : Single) return Singles.Vector3;

   --  --------------------------------------------------------------------------------
   --  Blend_Vectors calculates the vector cubic Bezier spline value at the parameter T
   function  Blend_Vectors (CP0, CP1, CP2, CP3 : Singles.Vector3;
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
        Result (X) :=  T1_Cub * CP0 (X) + T3 * T1_SQ * CP1 (X) +
          T3_Sq * T1 * CP2 (X) + T_Cub * CP3 (X);
        Result (Y) :=  T1_Cub * CP0 (Y) + T3 * T1_SQ * CP1 (Y) +
          T3_Sq * T1 * CP2 (Y) + T_Cub * CP3 (Y);
        Result (Z) :=  T1_Cub * CP0 (Z) + T3 * T1_SQ * CP1 (Z) +
        T3_Sq * T1 * CP2 (Z) + T_Cub * CP3 (Z);
      return Result;
   end Blend_Vectors;

  --  --------------------------------------------------------------------------------
  --  Build_Curve generates the Num_Steps segments of a spline
   procedure Build_Curve (CP0, CP1, CP2, CP3 : Singles.Vector3;
                          Num_Steps          : Int;
                          Curve_Coords       : out Singles.Vector3_Array) is
      Step        : constant Single := 1.0 / Single (Num_Steps);
      T           : Single := Step;
      Coord_Index : Int := 1;
   begin
      Curve_Coords (Coord_Index) := CP0;                   --  Start of spline
--        for Coord_Index in 1 .. Num_Steps loop   --  Build spline
      while T < 1.0 + 0.5 * Step loop
         Coord_Index := Coord_Index + 1;
         Curve_Coords (Coord_Index) := Blend_Vectors (CP0, CP1, CP2, CP3, T);
         T := T + Step;
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

   procedure Build_Patch (Patch : Teapot_Data.Patch_Type; Num_Steps : Int;
                          Patch_Array : out Singles.Vector3_Array) is
      use Teapot_Data;
      CP0, CP1, CP2, CP3 : Singles.Vector3 := (0.0, 0.0, 0.0);
      Step        : constant Single := 1.0 / Single (Num_Steps);
      Index       : Int;
      T           : Single := 0.0;
      Curve       : Singles.Vector3_Array (1 .. Num_Steps + 1);
   begin
      for Step_Count in 1 .. Num_Steps loop
         Index := 1 + (Step_Count - 1) * 2 * Num_Steps;
         --  Splines of constant U
         CP0 := U_Coord (Patch, 1, T);
         CP1 := U_Coord (Patch, 2, T);
         CP2 := U_Coord (Patch, 3, T);
         CP3 := U_Coord (Patch, 4, T);
         Build_Curve (CP0, CP1, CP2, CP3, Num_Steps, Curve);
         Utilities.Print_Vector ("Build_Patch U CP0", CP0);
         Utilities.Print_Vector ("Build_Patch U CP1", CP1);
         Utilities.Print_Vector ("Build_Patch U CP2", CP2);
         Utilities.Print_Vector ("Build_Patch U CP3", CP3);
         Put_Line ("Build_Patch, Index 1: " & Int'Image (Index) & " T: " &
                     Single'Image (T));
         Utilities.Print_GL_Array3 ("Curve segment", Curve);
         Patch_Array (Index .. Index + Num_Steps) := Curve;
         Utilities.Print_GL_Array3 ("Patch_Array", Patch_Array);
         --  Splines of constant V
         CP0 := V_Cord (Patch, 1, T);
         CP1 := V_Cord (Patch, 2, T);
         CP2 := V_Cord (Patch, 3, T);
         CP3 := V_Cord (Patch, 4, T);
         Build_Curve (CP0, CP1, CP2, CP3, Num_Steps, Curve);
         Utilities.Print_Vector ("Build_Patch V CP0", CP0);
         Utilities.Print_Vector ("Build_Patch V CP1", CP1);
         Utilities.Print_Vector ("Build_Patch V CP2", CP2);
         Utilities.Print_Vector ("Build_Patch V CP3", CP3);
         Index := Index + Num_Steps + 1;
         Put_Line ("Build_Patch, Index 2: " & Int'Image (Index) & " T: " &
                     Single'Image (T));
         Utilities.Print_GL_Array3 ("Curve Segment", Curve);
         Patch_Array (Index .. Index + Num_Steps) := Curve;
         Utilities.Print_GL_Array3 ("Patch_Array", Patch_Array);
         T := T + Step;
      end loop;

   exception
      when  others =>
         Put_Line ("An exception occurred in Pascal_Teapot.Build_Patch.");
         raise;
   end Build_Patch;

   --  --------------------------------------------------------------------------------

   procedure Build_Teapot (Num_Steps : Int;
                           theTeapot : out Singles.Vector3_Array) is
      Patches            : constant Teapot_Data.Patch_Array := Teapot_Data.Patchs;
      Patch_Array_Length : constant Int := 2 * (Num_Steps ** 2 + 1);
      aPatch             : Singles.Vector3_Array (1 .. Patch_Array_Length) :=
                             (others => (0.0, 0.0, 0.0));
      Offset             : Int;
   begin
--        for Patch_Index in Patchs'Range loop
      for Patch_Index in Int range 1 .. 1 loop
         Offset := (Patch_Index - 1) * Num_Steps * Num_Steps;
         Put_Line ("Pascal_Teapot.Build_Teapot Patch_Array_Length" &
               Int'Image (Patch_Array_Length));
         Put ("Pascal_Teapot.Build_Teapot, Patch " & Int'Image (Patch_Index));
         Print_Patch ("", Patches (Patch_Index));
--           Put_Line ("Pascal_Teapot.Build_Teapot Patch_Array_Length: " &
--                       Int'Image (Patch_Array_Length));
         Build_Patch (Patches (Patch_Index), Num_Steps, aPatch);
         Utilities.Print_GL_Array3 ("", aPatch);
         New_Line;
         for Patch_Index in 1 .. Patch_Array_Length loop
                theTeapot (Offset + Patch_Index) :=
                  aPatch (Patch_Index);
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

   procedure Print_Patch (Name     : String;
                          thePatch : Teapot_Data.Patch_Type) is
   use GL.Types;
   begin
      Put_Line (Name & ":");
      for Row in thePatch'Range loop
         for Column in thePatch'Range (2) loop
            Put (Int'Image (thePatch (Row, Column)) & "   ");
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Patch;

   --  ------------------------------------------------------------------------

   function U_Coord (Patch : Teapot_Data.Patch_Type; Index : Int; T : Single)
                       return Singles.Vector3 is
      use Teapot_Data;
   begin
      return Blend_Vectors (Control_Points (Patch (Index, 1)),
                            Control_Points (Patch (Index, 2)),
                            Control_Points (Patch (Index, 3)),
                            Control_Points (Patch (Index, 4)), T);
    end U_Coord;

   --------------------------------------------------------------------------------

   function V_Cord (Patch : Teapot_Data.Patch_Type; Index : Int; T : Single)
                    return Singles.Vector3 is
      use Teapot_Data;
   begin
      return Blend_Vectors (Control_Points (Patch (1, Index)),
                            Control_Points (Patch (2, Index)),
                            Control_Points (Patch (3, Index)),
                            Control_Points (Patch (4, Index)), T);
    end V_Cord;

   --  --------------------------------------------------------------------------------

end Pascal_Teapot;
