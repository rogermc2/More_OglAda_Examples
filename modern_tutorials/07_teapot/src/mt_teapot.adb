
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

with Teacup_Maths; use Teacup_Maths;

package body MT_Teapot is
   use GL.Types;

   subtype Vertex is GL.Types.Singles.Vector3;
   type Control_Point_Array is array (GL.Types.Int range <>, GL.Types.Int range <>) of Vertex;

   function Compute_Position (Control_Points : Control_Point_Array; U, V : Single)
                               return GL.Types.Singles.Vector3;
   procedure Build_Vertices (Vertices : out Vertices_Array;
                             Colours  : out Colours_Array);

   --  --------------------------------------------------------------------------------

   procedure Build_Control_Points (Patch          : GL.Types.Int;
                                   Control_Points : out Control_Point_Array) is
      use GL.Types;
      thePatch : Teapot_Data.Bezier_Patch := Teapot_Data.Patchs (Patch);
   begin
      for Index_I in Int range 1 .. Teapot_Data.Order loop
         for Index_J in Int range 1 .. Teapot_Data.Order loop
--              Put_Line ("MT_Teapot.Build_Control_Points thePatch (Index_I, Index_J)" &
--                          Int'Image (thePatch (Index_I, Index_J)));
            Control_Points (Index_I, Index_J) :=
              Teapot_Data.CP_Vertices (thePatch (Index_I, Index_J));
--              Utilities.Print_Vector ("MT_Teapot.Build_Control_Points CP_Vertices",
--                                      Control_Points (Index_I, Index_J));
         end loop;
      end loop;

   exception
      when  others =>
         Put_Line ("An exception occurred in MT_Teapot.Build_Control_Points.");
         raise;
   end Build_Control_Points;

   --  --------------------------------------------------------------------------------

   procedure Build_Elements (Elements : out Element_Array) is
      use GL.Types;
      N      : Int := 0;
      Res_UV : constant Int := Res_U * Res_V;
   begin
      --  1 square ABCD = 2 triangles ABC + CDA
      for Patch_Num in 0 .. Int (Teapot_Data.Patchs'Length - 1) loop
         for Ru in 0 .. Res_U - 1 loop
            for Rv in 0 .. Res_V - 1 loop
               --  ABC
               N := N + 1;
               Elements (N) := Patch_Num * Res_UV + Ru * Res_V + rv;
               N := N + 1;
               Elements (N) := Patch_Num * Res_UV + Ru * Res_V + rv + 1;
               N := N + 1;
               Elements (N) := Patch_Num * Res_UV + (Ru + 1) * Res_V + rv + 1;
               --  CDA
               N := N + 1;
               Elements (N) := Patch_Num * Res_UV + (Ru + 1) * Res_V + rv + 1;
               N := N + 1;
               Elements (N) := Patch_Num * Res_UV + (Ru + 1) * Res_V + rv;
               N := N + 1;
               Elements (N) := Patch_Num * Res_UV + Ru * Res_V + rv;
            end loop;
         end loop;
      end loop;
      Put_Line ("MT_Teapot.Build_Elements, last N" & Int'Image (N));

   exception
      when  others =>
         Put_Line ("An exception occurred in MT_Teapot.Build_Elements.");
         raise;
   end Build_Elements;

   --  --------------------------------------------------------------------------------

   procedure Build_CP_Elements (CP_Elements : out Teapot_CP_Elements) is
      Patch : Teapot_Data.Bezier_Patch;
   begin
      for Patch_Num in Teapot_Data.Patchs'First .. Teapot_Data.Patchs'Last loop
         Patch := Teapot_Data.Patchs (Patch_Num);
         for I in 1 .. Teapot_Data.Order loop
            for J in 1 .. Teapot_Data.Order loop
               CP_Elements (Patch_Num, I, J) :=  Patch (I, J) - 1;
            end loop;
         end loop;
      end loop;

   exception
      when  others =>
         Put_Line ("An exception occurred in MT_Teapot.Build_CP_Elements.");
         raise;
   end Build_CP_Elements;

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
      use GL.Types;
      Control_Points : Control_Point_Array
        (1 .. Teapot_Data.Order, 1 .. Teapot_Data.Order);
      Res_UV         : constant Int := Res_U * Res_V;
      U              : Single;
      V              : Single;
      P_Part         : Int;
      PU_Part        : Int;
      Colour_P_Part  : Int;
      Colour_PU_Part : Int;
   begin
      --  Evaluate the Bézier surface, with a resolution of 10x10.
      --  For each 4x4 patch, compute each point in the 10x10 grid
      --  with u and v progressing in 1/10 steps.
      for Patch_Count in 0 .. Int (Teapot_Data.Patchs'Length - 1) loop
         Build_Control_Points (Patch_Count + 1, Control_Points);
         P_Part := 1 + Patch_Count * Res_UV;
         Colour_P_Part := 1 + 3 * Patch_Count * Res_UV;
         for Ru in 0 .. Res_U - 1 loop
            U := Single (Ru) / Single (Res_U - 1);
            PU_Part := P_Part + Ru * Res_V;
            Colour_PU_Part := Colour_P_Part + 3 * Ru * Res_V;
            for Rv in 0 .. Res_V - 1 loop
               V := Single (Rv) / Single (Res_V - 1);
--           Put_Line ("MT_Teapot.Build_Vertices, PU_Part + Rv " & Int'Image (PU_Part + Rv));
               Vertices (PU_Part + Rv) :=
                 Compute_Position (Control_Points, U, V);
               Colours (Colour_PU_Part + 3 * rv) :=
                 Single (Patch_Count) / Single (Teapot_Data.Num_Patchs);
               Colours (Colour_PU_Part+ 3 * rv + 1) :=
                 Single (Patch_Count) / Single (Teapot_Data.Num_Patchs);
               Colours (Colour_PU_Part + 3 * rv + 2) := 0.8;
            end loop;
         end loop;
      end loop;

   exception
      when  others =>
         Put_Line ("An exception occurred in MT_Teapot.Build_Vertices.");
         raise;
   end Build_Vertices;

   --  --------------------------------------------------------------------------------

   function Compute_Position (Control_Points : Control_Point_Array; U, V : Single)
                               return Singles.Vector3 is
      Position : Singles.Vector3 := (0.0, 0.0, 0.0);
      Pol_I    : Single;
      Pol_J    : Single;
   begin
      for I in 0 .. Teapot_Data.Order loop
         Pol_I := Bernstein_Polynomial (I, Teapot_Data.Order, U);
         for J in 0 .. Teapot_Data.Order loop
            Pol_J := Bernstein_Polynomial (J, Teapot_Data.Order, V);
         end loop;
      end loop;
      return Position;

   exception
      when  others =>
         Put_Line ("An exception occurred in MT_Teapot.Compute_Position.");
         raise;
   end Compute_Position;

   --  --------------------------------------------------------------------------------

end MT_Teapot;
