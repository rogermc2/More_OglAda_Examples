
with Ada.Text_IO; use Ada.Text_IO;

package body Teacup_Maths is

    function Bernstein_Polynomial (I, N : Int; U : Single) return Single is
    begin
        return Binomial_Coefficient (I, N) *
          (U ** Natural (I)) * ((1.0 - U) ** Natural (N - I));
    end Bernstein_Polynomial;

    --  --------------------------------------------------------------------------------

    function Binomial_Coefficient (I, N : Int) return Single is
        Result : Single := 0.0;
    begin
        if I >= 0 and N >= 0 then
            Result := Single (Factorial (N)) / Single (Factorial (I) * Factorial (N - I));
        else
            Put_Line ("MT_Teapot.Binomial_Coefficient, invalid paramters.");
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
            Put_Line ("MT_Teapot.Binomial_Coefficient, invalid paramters.");
        end if;
        return Result;

    end Factorial;

    --  --------------------------------------------------------------------------------

end Teacup_Maths;
