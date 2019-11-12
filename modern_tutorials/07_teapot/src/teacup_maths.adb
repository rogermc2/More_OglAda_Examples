
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

package body Teacup_Maths is

   function Bernstein_Polynomial (I, N : Int; U : Single) return Single is
      Result : Single := 0.0;
   begin
      if I <=  N then
         Result := Binomial_Coefficient (I, N) *
           (U ** Natural (I)) * ((1.0 - U) ** Natural (N - I));
      else
         New_Line;
         raise Maths_Error with "MT_Teapot.Bernstein_Polynomial, invalid parameters I, N: " &
           Int'Image (I) & "  " &  Int'Image (N);
      end if;
      return Result;

   end Bernstein_Polynomial;

   --  --------------------------------------------------------------------------------

   function Binomial_Coefficient (I, N : Int) return Single is
      use Maths;
      Result : Single := 0.0;
      IInt   : constant Integer := Integer (I);
      NInt   : constant Integer := Integer (N);
   begin
      if I >= 0 and N >= 0 then
         Result := Single (Factorial (NInt)) /
           Single (Factorial (IInt) * Factorial (NInt - IInt));
      else
         raise Maths_Error with "MT_Teapot.Binomial_Coefficient, invalid parameters I, N: " &
           Int'Image (I) & "  " &  Int'Image (N);
      end if;
      return Result;
   end Binomial_Coefficient;

   --  --------------------------------------------------------------------------------

end Teacup_Maths;
