
with Ada.Text_IO; use Ada.Text_IO;

package body Teapot_Maths is

   function Bernstein_Polynomial (I, N : Int; U : Single) return Single is
      Result : Single := 0.0;
   begin
      if I <=  N then
         Result := Binomial_Coefficient (I, N) *
           (U ** Natural (I)) * ((1.0 - U) ** Natural (N - I));
      else
         New_Line;
         raise Maths_Error with "Teapot_Maths.Bernstein_Polynomial, invalid parameters I, N: " &
           Int'Image (I) & "  " &  Int'Image (N);
      end if;
      return Result;

   end Bernstein_Polynomial;

   --  --------------------------------------------------------------------------------

   function Binomial_Coefficient (I, N : Int) return Single is
      Result : Single := 0.0;
   begin
      if I >= 0 and N >= 0 then
         Result := Single (Factorial (N)) / Single (Factorial (I) * Factorial (N - I));
      else
         raise Maths_Error with "Teapot_Maths.Binomial_Coefficient, invalid parameters I, N: " &
           Int'Image (I) & "  " &  Int'Image (N);
      end if;
      return Result;
   end Binomial_Coefficient;

   --  --------------------------------------------------------------------------------

   function Factorial (N : Int) return Int is
      Result : Int := 1;
   begin
      if N >= 0 then
         for I in reverse 1 .. N loop
            Result := I * Result;
         end loop;
      else
         Put_Line ("Teapot_Maths.Binomial_Coefficient, invalid paramters.");
      end if;
      return Result;

   end Factorial;

   --  --------------------------------------------------------------------------------

end Teapot_Maths;
